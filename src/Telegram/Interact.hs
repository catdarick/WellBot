{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Interact where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, catch, try)
import           Control.Monad              (replicateM, replicateM_, void)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.State  (StateT, get, gets, modify,
                                             runStateT)
import           Data.Aeson                 (decode, encode)
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 (ByteString, fromStrict, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char                  (isDigit)
import qualified Data.Configurator.Types    as DataConfigurator
import           Data.Function              ((&))
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           GHC.Base                   (Any, when)
import qualified Telegram.Api               as Api
import           Config
import qualified Telegram.Database.Interact as DB
import qualified Telegram.Database.Types    as DB
import           Telegram.Keyboard.Builder
import           Telegram.Types

data Handle m a =
  Handle
    { hSendMessage          :: Api.Handle -> Integer -> String -> m a
    , hForwardMessage       :: Api.Handle -> Integer -> Integer -> m a
    , hForwardMessageNTimes :: Api.Handle -> Integer -> Integer -> Integer -> m a
    , hSendKyboardWithText  :: Api.Handle -> Integer -> String -> m a
    , hApi :: Api.Handle
    }

withErrorPrinting f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      lift $ print e
      return mempty
    Right x -> return x

onText ::
     Monoid b
  => Handle IO b
  -> Config
  -> ChatId
  -> String
  -> MessageId
  -> Integer
  -> StateT DB.DB IO b
onText h config chatId text messageId repAmount = do
  let hApi_ = h & hApi
  isKeyboardResponse <- isKeyboardResponse chatId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount chatId (getIntByChar (head text)) >>
           return mempty
      else withErrorPrinting $
           (h & hForwardMessageNTimes) hApi_ chatId messageId repAmount
  DB.delAwaitingChat chatId
  return res
  where
    isKeyboardResponse chatId text = do
      isAwaiting <- DB.isAwaiting chatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate :: Monoid b => Handle IO b -> Config -> Update -> StateT DB.DB IO b
handleUpdate h config update = do
  let updateId = update & updateUpdateId
  let message = fromJust (update & updateMessage)
  let maybeText = message & messageText
  let messageId = message & messageMessageId
  let hApi_ = h & hApi
  let chatId_ = message & messageChat & chatId
  let helpText_ = config & helpText
  repAmount <- DB.getRepeatsAmount chatId_
  res <-
    case maybeText of
      Just "/start" ->
        withErrorPrinting $ (h & hSendMessage) hApi_ chatId_ helpText_
      Just "/help" ->
        withErrorPrinting $ (h & hSendMessage) hApi_ chatId_ helpText_
      Just "/repeat" -> onRepeat hApi_ chatId_ repAmount
      Just text -> onText h config chatId_ text messageId repAmount
      Nothing ->
        withErrorPrinting $
        (h & hForwardMessageNTimes) hApi_ chatId_ messageId repAmount
  DB.setOffset (updateId + 1)
  return res
  where
    repeatText_ repAmount = (config & repeatText) ++ show repAmount
    onRepeat config chatId repAmount = do
      DB.addAwaitingChat chatId
      withErrorPrinting $
        (h & hSendKyboardWithText) config chatId (repeatText_ repAmount)

loop :: Monoid b1 => Handle IO b1 -> Config -> StateT DB.DB IO b2
loop h config = do
  let hApi_ = h & hApi
  lift $ print "loop"
  offset <- gets DB.offset
  isTimeToBackup <- isTimeToBackup
  when isTimeToBackup backUpAndUpdateTimer
  updates <- withErrorPrinting $ Api.getUpdates hApi_ offset
  let filtredUpdates = filter isJustMessage updates
  handleUpdates filtredUpdates
  loop h config
  where
    handleUpdates = mapM_ (handleUpdate h config)
    isJustMessage Update {updateMessage = maybeMessage} = isJust maybeMessage
    isTimeToBackup = do
      prevTime <- DB.getFromDatabase DB.prevTime
      curTime <- lift getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup "./backup.dat"
      DB.updateTime

start :: DataConfigurator.Config -> IO ()
start configHandle = do
  config <- (parseConfig configHandle)
  apiConfig <- (Api.parseConfig configHandle)
  initDB <- DB.getRestoredOrNewDatabase "./backup.dat" (config & defaultRepeatAmount)
  let h =
        Handle
          { hSendMessage = Api.sendMessage 
          , hForwardMessage = Api.forwardMessage
          , hForwardMessageNTimes = Api.forwardMessageNTimes
          , hSendKyboardWithText = Api.sendKyboardWithText
          , hApi = Api.Handle{Api.hConfig = apiConfig}
          }
  runStateT (loop h config) initDB
  return ()
