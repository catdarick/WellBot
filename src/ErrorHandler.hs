{-# LANGUAGE ScopedTypeVariables #-}

module ErrorHandler where

import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans, lift)

withErrorPrinting :: (Monad (t IO), MonadTrans t, Monoid b) => IO b -> t IO b
withErrorPrinting f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      lift $ print e
      return mempty
    Right x -> return x
