{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic where

import           Class.Bot
import           Class.Update
import           Config
import           Control.Exception         (SomeException, try)
import           Control.Monad             (replicateM_, void, when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT (runStateT), gets)
import           Data.Char                 (isDigit)
import qualified Data.Configurator.Types   as Configurator
import           Data.Function             ((&))
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (diffUTCTime, getCurrentTime)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import           ErrorHandler

onRepeat ::
     Bot a
  => a
  -> Config
  -> UserOrChatId
  -> RepeatsAmount
  -> BotState a IO (ReturningType a)
onRepeat bot config userOrChatId repAmount = do
  let repeatText_ repAmount = (config & repeatText) ++ show repAmount
  DB.addAwaitingChat userOrChatId
  withErrorPrinting $
    sendKeyboardWithText bot config userOrChatId (repeatText_ repAmount)

onText ::
     Bot a
  => a
  -> Config
  -> UserOrChatId
  -> String
  -> MesssageId
  -> RepeatsAmount
  -> BotState a IO (ReturningType a)
onText bot config userOrChatId text messageId repAmount = do
  isKeyboardResponse <- isKeyboardResponse userOrChatId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount userOrChatId (getIntByChar (head text)) >>
           return mempty
      else withErrorPrinting $
           forwardMessageNTimes bot config userOrChatId messageId repAmount
  DB.delAwaitingChat userOrChatId
  return res
  where
    isKeyboardResponse userOrChatId text = do
      isAwaiting <- DB.isAwaiting userOrChatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate ::
     Bot a => a -> Config -> UpdateType a -> BotState a IO (ReturningType a)
handleUpdate bot config update = do
  let maybeText = getMaybeText update
  let userOrChatId = getUserOrChatId update
  let messageId = getMessageId update
  repAmount <- DB.getRepeatsAmount config userOrChatId
  case maybeText of
    Just "/start" ->
      withErrorPrinting $
      sendMessage bot config userOrChatId (config & helpText)
    Just "/help" ->
      withErrorPrinting $
      sendMessage bot config userOrChatId (config & helpText)
    Just "/repeat" -> onRepeat bot config userOrChatId repAmount
    Just text -> onText bot config userOrChatId text messageId repAmount
    Nothing ->
      withErrorPrinting $
      forwardMessageNTimes bot config userOrChatId messageId repAmount

loop :: Bot a => a -> Config -> BotState a IO ()
loop bot config = do
  offset <- gets DB.offset
  isTimeToBackup <- isTimeToBackup
  when isTimeToBackup backUpAndUpdateTimer
  updatesAndOffset <- getUpdatesAndOffset bot config
  let updates = fst updatesAndOffset
  let newOffset = snd updatesAndOffset
  handleUpdates updates
  DB.setOffset newOffset
  loop bot config
  where
    handleUpdates = mapM_ (handleUpdate bot config)
    isTimeToBackup = do
      prevTime <- gets DB.prevTime
      curTime <- lift getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup $ (config & backupPath) ++ (backupName bot)
      DB.updateTime

start :: Bot a => a -> Config -> IO ()
start bot config = do
  db <- DB.getRestoredOrClearDatabase (backupName bot) (defaultOffset bot)
  res <- runStateT (initAndGoLoop config) db
  return ()
  where
    initAndGoLoop config = do
      initBot bot config
      loop bot config
