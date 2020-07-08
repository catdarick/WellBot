module Logger.Interact where

import           Bot.Types
import           Class.Bot
import           Config
import           Control.Exception         (SomeException, catch, try)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets)
import           Data.Function             ((&))
import           GHC.Exception.Type        (SomeException (SomeException))
import           Logger.Types

appendLog :: Bot a => String -> String -> BotStateT a IO ()
appendLog lvl msg = do
  name <- gets $name . bot
  path <- gets $ logPath . config
  offset <- gets logOffset
  let spaceOffset = replicate (offset * 4) ' '
  lift $
    appendFile
      (path ++ name ++ ".log")
      (spaceOffset ++ lvl ++ ": " ++ msg ++ "\n")

debug :: Bot a => String -> BotStateT a IO ()
debug msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel >= DEBUG) $ appendLog "DEBUG" msg

info :: Bot a => String -> BotStateT a IO ()
info msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel >= INFO) $ appendLog "INFO" msg

warn :: Bot a => String -> BotStateT a IO ()
warn msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel >= WARNING) $ appendLog "WARNING" msg
