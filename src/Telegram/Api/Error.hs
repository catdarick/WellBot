module Telegram.Api.Error where

import           Control.Exception  (throw)
import           Control.Monad      (unless)
import           Data.Function      ((&))
import           Data.Maybe         (fromMaybe)
import           GHC.Exception      (errorCallException)
import           Telegram.Api.Types

isResponseOk :: Maybe (Response respType) -> Bool
isResponseOk = maybe False getOkFromResponse
  where
    getOkFromResponse res = res & responseOk

getErrorMessage :: Maybe (Response respType) -> String
getErrorMessage maybeResponse =
  case maybeResponse of
    Just response ->
      show (errorCode response) ++ ": " ++ (errorDescription response)
    Nothing -> "Bad response"
  where
    errorCode response = fromMaybe 0 (response & responseErrorCode)
    errorDescription response = fromMaybe "" (response & responseDescription)

throwIfError :: Applicative f => Maybe (Response respType) -> String -> f ()
throwIfError maybeResponse errorLocation =
  unless (isResponseOk maybeResponse) $ throwError maybeResponse
  where
    throwError maybeResponse =
      throw $
      errorCallException $
      "Error in " ++
      errorLocation ++ " with code " ++ getErrorMessage maybeResponse
