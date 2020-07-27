{-# LANGUAGE ScopedTypeVariables #-}

module Logger.Interact where

import           Bot.Classes
import           Bot.State.Types
import           Config
import           Control.Exception         (SomeException, catch, try)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets, modify)
import           Data.Function             ((&))
import           Data.Time                 (getCurrentTime)
import           GHC.Exception.Type        (SomeException (SomeException))
import           Logger.Types
import           Text.Printf               (printf)

appendLog :: Bot a => String -> String -> BotStateIO a ()
appendLog lvl msg = do
  time <- lift getCurrentTime
  name <- gets $name . bot
  path <- gets $ logPath . config
  offset <- gets logOffset
  let spaceOffset = replicate (offset * 4) ' '
  let logString =
        printf "%19.19s %s %7.7s: %s\n" (show time) spaceOffset lvl msg
  lift $ appendFile (path ++ name ++ ".log") logString
  lift $ print (spaceOffset ++ lvl ++ ": " ++ msg ++ "\n")
  return ()

debug :: Bot a => String -> BotStateIO a ()
debug msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel <= DEBUG) $ appendLog "DEBUG" msg

info :: Bot a => String -> BotStateIO a ()
info msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel <= INFO) $ appendLog "INFO" msg

warn :: Bot a => String -> BotStateIO a ()
warn msg = do
  logSinceLevel <- gets $logSinceLevel . config
  when (logSinceLevel <= WARNING) $ appendLog "WARNING" msg

incLogOffset :: BotStateIO a ()
incLogOffset =
  modify
    (\st@BotState {logOffset = logOffset} -> st {logOffset = logOffset + 1})

decLogOffset :: BotStateIO a ()
decLogOffset =
  modify
    (\st@BotState {logOffset = logOffset} -> st {logOffset = logOffset - 1})

withDebugLogging :: Bot a => String -> BotStateIO a b -> BotStateIO a b
withDebugLogging functionName function = do
  config <- gets config
  if (config & logSinceLevel) <= DEBUG
    then do
      debug $ "Entry in " ++ functionName
      incLogOffset
      res <- function
      decLogOffset
      debug $ "Exit from " ++ functionName
      return res
    else function

withErrorLogging :: (Bot a, Monoid b) => IO b -> BotStateIO a b
withErrorLogging f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      warn $ show e
      return mempty
    Right x -> return x
