{-# LANGUAGE ScopedTypeVariables #-}

module Vk.Api.Longpoll.Interact where

import           Bot.ErrorException
import           Config
import           Control.Monad.Catch   (MonadThrow, throwM)
import           Data.Aeson            (decode)
import           Data.Function         ((&))
import           Vk.Api.Interact       (defaultServer, getRequest)
import           Vk.Api.Longpoll.Types

getServerAndTokenAndOffset :: Config -> IO (Maybe (Response Container))
getServerAndTokenAndOffset config = do
  let path = "method/groups.getLongPollServer"
  let groupIdPair = ("group_id", show $ config & vkGroupId)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [groupIdPair, tokenPair, versionPair]
  bsResponse <- getRequest defaultServer path queryPairs
  let maybeResponse = decode bsResponse
  return maybeResponse

getResultOrThrow :: MonadThrow m => Response respType -> m respType
getResultOrThrow resp =
  case resp & responseResponse of
    Just result -> return result
    Nothing     -> throwError resp

throwError :: MonadThrow m => Response respType -> m a
throwError resp =
  case resp & responseError of
    Just (Error 5 msg) -> throwM BadTokenException
    Just (Error 15 msg) -> throwM BadTokenException
    Just (Error code msg) ->
      throwM $
      CommonException $
      "Longpoll error with code " ++ show code ++ ": " ++ msg
    Nothing -> throwM BadResponseException
