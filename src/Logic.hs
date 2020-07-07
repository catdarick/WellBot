{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Logic where

import           Class.Bot
import           Class.Update
import           Config
import           Control.Monad             (replicateM_, when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT (runStateT), gets)
import           Data.Char                 (isDigit)
import qualified Data.Configurator.Types   as Configurator
import           Data.Function             ((&))
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (diffUTCTime, getCurrentTime)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB

forwardMessageNTimes bot config userOrChatId messageId n =
  replicateM_ (fromInteger n) (forwardMessage bot config userOrChatId messageId)

onRepeat bot config userOrChatId repAmount = do
  let repeatText_ repAmount = (config & repeatText) ++ show repAmount
  DB.addAwaitingChat userOrChatId
  lift $ sendKeyboardWithText bot config userOrChatId (repeatText_ repAmount)

onText bot config userId text messageId repAmount = do
  isKeyboardResponse <- isKeyboardResponse userId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount userId (getIntByChar (head text)) >>
           return mempty
      else lift $ forwardMessageNTimes bot config userId messageId repAmount
  DB.delAwaitingChat userId
  return res
  where
    isKeyboardResponse userId text = do
      isAwaiting <- DB.isAwaiting userId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate ::
     ( Bot a u
     , Read offsetType
     , Read additionalInfoType
     , Show offsetType
     , Show additionalInfoType
     , Update u
     )
  => a
  -> Config
  -> u
  -> StateT (DB.Database offsetType additionalInfoType) IO ()
handleUpdate bot config update = do
  let maybeText = getMaybeText update
  let userOrChatId = getUserOrChatId update
  let messageId = getMessageId update
  repAmount <- DB.getRepeatsAmount config userOrChatId
  case maybeText of
    Just "/start" ->
      lift $ sendMessage bot config userOrChatId (config & helpText)
    Just "/help" ->
      lift $ sendMessage bot config userOrChatId (config & helpText)
    Just "/repeat" -> onRepeat bot config userOrChatId repAmount
    Just text -> onText bot config userOrChatId text messageId repAmount
    Nothing ->
      lift $ forwardMessageNTimes bot config userOrChatId messageId repAmount
  return ()

loop ::
     ( Bot a u
     , Read (OffsetType a)
     , Read (AdditionalType a)
     , Show (OffsetType a)
     , Show (AdditionalType a)
     )
  => a
  -> Config
  -> BotState a IO b
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
      lift $ print "Backing up"
      DB.backup $ (config & backupPath) ++ (backupName bot)
      DB.updateTime

start ::
     ( Bot a u
     , Read (OffsetType a)
     , Read (AdditionalType a)
     , Show (OffsetType a)
     , Show (AdditionalType a)
     )
  => a
  -> Configurator.Config
  -> IO ()
start bot configHandle = do
  config <- parseConfig configHandle
  db <- DB.getRestoredOrClearDatabase (backupName bot) (defaultOffset bot)
  res <- runStateT (initAndGoLoop config) db
  --print res
  return ()
  where
    initAndGoLoop config = do
      initBot bot config
      loop bot config
