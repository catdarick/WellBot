{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic where

import           Bot.Types
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
import           Data.Time                 (UTCTime, diffUTCTime,
                                            getCurrentTime)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import           ErrorHandler

onRepeat ::
     Bot a => UserOrChatId -> RepeatsAmount -> BotStateT a IO (ReturningType a)
onRepeat userOrChatId repAmount = do
  bot <- gets bot
  config <- gets config
  let repeatText_ repAmount = (config & repeatText) ++ show repAmount
  DB.addAwaitingChat userOrChatId
  withDebugLogging "sendKeyboardWithText" $withErrorLogging $
    sendKeyboardWithText bot config userOrChatId (repeatText_ repAmount)

onText ::
     Bot a
  => UserOrChatId
  -> String
  -> MesssageId
  -> RepeatsAmount
  -> BotStateT a IO (ReturningType a)
onText userOrChatId text messageId repAmount = do
  bot <- gets bot
  config <- gets config
  isKeyboardResponse <- isKeyboardResponse userOrChatId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount userOrChatId (getIntByChar (head text)) >>
           return mempty
      else withDebugLogging "forwardMessageNTimes" $withErrorLogging $
           forwardMessageNTimes bot config userOrChatId messageId repAmount
  DB.delAwaitingChat userOrChatId
  return res
  where
    isKeyboardResponse userOrChatId text = do
      isAwaiting <- DB.isAwaiting userOrChatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate :: Bot a => UpdateType a -> BotStateT a IO (ReturningType a)
handleUpdate update = do
  bot <- gets bot
  config <- gets config
  let maybeText = getMaybeText update
  let userOrChatId = getUserOrChatId update
  let messageId = getMessageId update
  repAmount <- DB.getRepeatsAmount config userOrChatId
  case maybeText of
    Just "/start" ->
      withDebugLogging "sendMessage" $
      withErrorLogging $ sendMessage bot config userOrChatId (config & helpText)
    Just "/help" ->
      withDebugLogging "sendMessage" $
      withErrorLogging $ sendMessage bot config userOrChatId (config & helpText)
    Just "/repeat" -> onRepeat userOrChatId repAmount
    Just text -> onText userOrChatId text messageId repAmount
    Nothing ->
      withDebugLogging "forwardMessageNTimes" $
      withErrorLogging $
      forwardMessageNTimes bot config userOrChatId messageId repAmount

loop :: Bot a => BotStateT a IO ()
loop = do
  bot <- gets bot
  config <- gets config
  offset <- DB.getOffset
  isTimeToBackup <- isTimeToBackup config
  when isTimeToBackup (backUpAndUpdateTimer config)
  updatesAndOffset <-
    withDebugLogging "getUpdatesAndOffset" $getUpdatesAndOffset bot config
  let updates = fst updatesAndOffset
  let newOffset = snd updatesAndOffset
  handleUpdates updates
  DB.setOffset newOffset
  withDebugLogging "loop" loop
  where
    handleUpdates = mapM_ handleUpdateWithDebugLogging
    handleUpdateWithDebugLogging update =
      withDebugLogging "handleUpdate" $ handleUpdate update
    isTimeToBackup config = do
      prevTime <- DB.getPrevTime
      curTime <- lift getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer config = do
      DB.backup
      DB.updateTime

start :: Bot a => a -> Config -> IO ()
start bot config = do
  db <- DB.getRestoredOrClearDatabase backupName (defaultOffset bot)
  let botState = BotState_ {config = config, database = db, bot = bot, logOffset=0}
  res <- runStateT (initAndGoLoop config) botState
  return ()
  where
    backupName = (bot & name) ++ ".backup"
    initAndGoLoop config = do
      initBot bot config
      loop
