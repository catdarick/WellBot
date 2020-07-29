{-# LANGUAGE ScopedTypeVariables #-}

module Vk.Api.Methods.Interact where

import           Bot.ErrorException
import           Config
import           Control.Exception    (throw, try)
import           Control.Monad.Catch  (MonadThrow (throwM))
import           Data.Aeson           (decode)
import           Data.Function        ((&))
import           Data.Maybe           (fromMaybe, isJust)
import           System.Random        (getStdRandom, randomR)
import           Vk.Api.Interact      (defaultServer, getRequest)
import           Vk.Api.Methods.Types
import           Vk.Keyboard.Builder

getUpdatesAndOffset ::
     Config -> String -> String -> String -> IO (Maybe ([Update], String))
getUpdatesAndOffset config server offset token = do
  let tokenPair = ("key", token)
  let timeoutPair = ("wait", show $ config & secTimeout)
  let offsetPair = ("ts", offset)
  let queryPairs = [actionPair, tokenPair, offsetPair, timeoutPair, versionPair]
  bsResponse <- getRequest server "" queryPairs
  let maybeUpdates = decode bsResponse :: Maybe Updates
  case maybeUpdates of
    Just updates ->
      return $ Just (updates & updatesUpdates, updates & updatesTs)
    Nothing -> do
      let maybeError = decode bsResponse :: Maybe Error
      if isJust maybeError
        then return Nothing
        else throwM BadResponseException
  where
    versionPair = ("v", config & vkApiVersion)
    actionPair = ("act", "a_check")

sendMessage :: Config -> UserId -> String -> IO ()
sendMessage config userId text = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", text)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [userIdPair, randomIdPair, textPair, tokenPair, versionPair]
  bsResponse <- getRequest defaultServer sendPath queryPairs
  return ()

forwardMessage :: Config -> UserId -> MessageId -> IO ()
forwardMessage config userId messageId = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", "")
  let forwardPair = ("forward_messages", show messageId)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs =
        [ userIdPair
        , forwardPair
        , randomIdPair
        , textPair
        , tokenPair
        , versionPair
        ]
  bsResponse <- getRequest defaultServer sendPath queryPairs
  return ()

sendKeyboardWithText :: Config -> UserId -> String -> IO ()
sendKeyboardWithText config userId text = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", text)
  let keyboardPair = ("keyboard", getKeyboardJSON config)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs =
        [ userIdPair
        , keyboardPair
        , randomIdPair
        , textPair
        , tokenPair
        , versionPair
        ]
  bsResponse <- getRequest defaultServer sendPath queryPairs
  return ()

getRandomInt64 :: IO Integer
getRandomInt64 = getStdRandom (randomR (0, 2 ^ 64 - 1))

sendPath :: String
sendPath = "method/messages.send"
