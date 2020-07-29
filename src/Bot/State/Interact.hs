module Bot.State.Interact where

import           Bot.Classes
import qualified Bot.State.Database.Interact as DB
import           Bot.State.Types
import           Config
import           Control.Monad               (when)
import           Control.Monad.Extra         (whenM)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (get, gets, put)
import           Data.Function               ((&))
import           Data.Time                   (UTCTime, diffUTCTime,
                                              getCurrentTime)

getBotAndConfig :: Bot a => BotStateT a IO (a, Config)
getBotAndConfig = do
  bot <- gets bot
  config <- gets config
  return (bot, config)

updateTime :: Bot a => BotStateT a IO ()
updateTime = do
  state <- get
  newTime <- lift getCurrentTime
  put (state {prevTime = newTime})

getPrevTime :: Bot a => BotStateT a IO UTCTime
getPrevTime = gets prevTime

isTimeToBackup :: Bot a => BotStateT a IO Bool
isTimeToBackup = do
  config <- gets config
  prevTime <- getPrevTime
  curTime <- lift getCurrentTime
  return $ diffUTCTime curTime prevTime > (config & diffTimeBackupPeriod)

backupAndUpdateTimer :: Bot a => BotStateT a IO ()
backupAndUpdateTimer = do
  DB.backup
  updateTime

getInitialBotState ::
     (Read additionalInfoType, Show additionalInfoType, Bot a)
  => a
  -> Config
  -> IO (BotState (OffsetType a) additionalInfoType a)
getInitialBotState bot config = do
  let backupName = (bot & name) ++ ".backup"
  prevTime <- getCurrentTime
  db <- DB.getRestoredOrClearDatabase backupName (defaultOffset bot)
  let botState =
        BotState
          { bot = bot
          , config = config
          , database = db
          , logOffset = 0
          , prevTime = prevTime
          }
  return botState
