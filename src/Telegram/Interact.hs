{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Interact where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, catch, try)
import           Control.Monad              (replicateM, replicateM_, void)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.State  (StateT, get, modify, runStateT)
import           Data.Aeson                 (decode, encode)
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 (ByteString, fromStrict, unpack)
import           Data.Char                  (isDigit)
import qualified Data.Configurator.Types    as DataConfigurator
import           Data.Function              ((&))
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           GHC.Base                   (Any, when)
import           Telegram.Api
import           Telegram.Config
import qualified Telegram.Database.Interact as DB
import qualified Telegram.Database.Types    as DB
import           Telegram.Keyboard.Builder
import           Telegram.Types

withErrorPrinting :: (Monad (t IO), MonadTrans t, Monoid b) => IO b -> t IO b
withErrorPrinting f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      lift $ print e
      return mempty
    Right x -> return x

onText ::
     Config -> Integer -> String -> Integer -> Integer -> StateT DB.DB IO ()
onText config chatId text messageId repAmount = do
  isKeyboardResponse <- isKeyboardResponse chatId text
  if isKeyboardResponse
    then DB.setRepeatsAmount chatId (getIntByChar (head text))
    else withErrorPrinting $
         forwardMessageNTimes config chatId messageId repAmount
  DB.delAwaitingChat chatId
  where
    isKeyboardResponse chatId text = do
      isAwaiting <- DB.isAwaiting chatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate :: Config -> Update -> StateT DB.DB IO ()
handleUpdate config update = do
  let updateId = update & updateUpdateId
  let message = fromJust (update & updateMessage)
  let maybeText = message & messageText
  let messageId = message & messageMessageId
  let chatId_ = message & messageChat & chatId
  let helpText_ = config & helpText
  repAmount <- DB.getRepeatsAmount chatId_
  case maybeText of
    Just "/start" -> withErrorPrinting $ sendMessage config chatId_ helpText_
    Just "/help" -> withErrorPrinting $ sendMessage config chatId_ helpText_
    Just "/repeat" -> onRepeat config chatId_ repAmount
    Just text -> onText config chatId_ text messageId repAmount
    Nothing ->
      withErrorPrinting $
      forwardMessageNTimes config chatId_ messageId repAmount
  DB.setOffset (updateId + 1)
  where
    repeatText_ repAmount = (config & repeatText) ++ show repAmount
    onRepeat config chatId repAmount = do
      DB.addAwaitingChat chatId
      withErrorPrinting $
        sendKyboardWithText config chatId (repeatText_ repAmount)

loop :: Config -> StateT DB.DB IO b
loop config = do
  db <- get
  let offset = db & DB.offset
  isTimeToBackup <- isTimeToBackup
  when isTimeToBackup backUpAndUpdateTimer
  updates <- withErrorPrinting $ getUpdates config offset
  let filtredUpdates = filter isJustMessage updates
  mapM_ (handleUpdate config) filtredUpdates
  loop config
  where
    printError = (print :: SomeException -> IO ())
    isJustMessage Update {updateMessage = maybeMessage} = isJust maybeMessage
    loopDelay = config & uSecLoopPeriod
    isTimeToBackup = do
      prevTime <- DB.getFromDatabase DB.prevTime
      curTime <- lift $ getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup "./backup.dat"
      DB.updateTime

start :: DataConfigurator.Config -> IO ()
start configHandle = do
  config <- (parseConfig configHandle)
  initDB <- DB.getRestoredOrNewDatabase (config & defaultRepeatAmount)
  runStateT (loop config) initDB
  return ()
