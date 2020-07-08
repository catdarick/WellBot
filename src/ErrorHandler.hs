{-# LANGUAGE ScopedTypeVariables #-}

module ErrorHandler where

import           Bot.Types
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.State (gets, modify)
import qualified Logger.Interact           as Log
import Config
import Data.Function ((&))
import Logger.Types
--withLogging:: (Monad (t IO), MonadTrans t, Monoid b) => String ->IO b -> t IO b
incLogOffset =
  modify
    (\st@BotState_ {logOffset = logOffset} -> st {logOffset = logOffset + 1})

decLogOffset =
  modify
    (\st@BotState_ {logOffset = logOffset} -> st {logOffset = logOffset - 1})

withDebugLogging functionName function = do
  config <- gets config
  if ((config&logSinceLevel) >= DEBUG) then do
    Log.debug $ "Entry in " ++ functionName
    incLogOffset
    res <- function
    decLogOffset
    Log.debug $ "Exit from " ++ functionName
    return res
  else function

--withErrorLogging :: (Monad (t IO), MonadTrans t, Monoid b) => IO b -> t IO b
withErrorLogging f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      Log.warn $ show e
      return mempty
    Right x -> return x
