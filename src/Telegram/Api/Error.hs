module Telegram.Api.Error where

import           Bot.ErrorException
import           Control.Exception   (throw)
import           Control.Monad       (unless, when)
import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Function       ((&))
import           Data.Maybe          (fromMaybe)
import           GHC.Exception       (errorCallException)
import           Telegram.Api.Types

isResponseOk :: Maybe (Response respType) -> Bool
isResponseOk = maybe False getOkFromResponse
  where
    getOkFromResponse res = res & responseOk

onErrorResponse :: MonadThrow m => Maybe (Response respType) -> m a
onErrorResponse maybeResponse =
  throwM $
  case maybeResponse of
    Just response ->
      case response & errorCode of
        401 -> BadTokenException
        404 -> BadTokenException
        _ ->
          CommonException $
          show (errorCode response) ++ ": " ++ errorDescription response
    Nothing -> CommonException "Bad response"
  where
    errorCode response = fromMaybe 0 (response & responseErrorCode)
    errorDescription response = fromMaybe "" (response & responseDescription)

throwIfError :: MonadThrow m => Maybe (Response respType) -> p -> m ()
throwIfError maybeResponse errorLocation =
  unless (isResponseOk maybeResponse) (onErrorResponse maybeResponse)
