{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Api where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throw, toException, try)
import Control.Monad (replicateM, replicateM_, unless)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State (StateT, get, modify, runStateT)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isDigit)
import qualified Data.Configurator.Types as DataConfigurator
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Base (when)
import GHC.Exception (errorCallException)
import qualified Network.HTTP.Client.Conduit as Conduit
import qualified Network.HTTP.Conduit as Conduit
import Network.HTTP.Simple (httpBS)

import Data.Configurator (require)

import qualified Telegram.Database.Interact as DB
import qualified Telegram.Database.Types as DB
import Telegram.Keyboard.Builder
import Telegram.Types

data Config =
  Config
    { cTimeout :: Int
    , cToken :: String
    , cKeysAmount :: Int
    }

parseConfig cfg = do
  tgToken <- require cfg "tgToken" :: IO String
  keysAmount <- require cfg "tgKeysAmount" :: IO Int
  secTimeout <- require cfg "secLongpollingTimeout" :: IO Int
  return
    Config {cToken = tgToken, cKeysAmount = keysAmount, cTimeout = secTimeout}

data Handle =
  Handle
    { hConfig :: Config
    }

--doGetRequest :: String -> [(String, String)] -> IO LBS.ByteString
doGetRequest h method queryPairs = do
  initReq <- Conduit.parseRequest "https://api.telegram.org"
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  bsResponse <- httpBS req
  returnResponseBody bsResponse
  where
    timeout = h & hConfig & cTimeout
    token = h & hConfig & cToken
    timeoutQueryPair = ("timeout", show $ timeout)
    stringPairToByteStringPair (k, v) = (BS.pack k, Just $ BS.pack v)
    bsUrlPath = BS.pack ("bot" ++ token ++ "/" ++ method)
    setPath reqVal path = reqVal {Conduit.path = path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs =
      map stringPairToByteStringPair (timeoutQueryPair : queryPairs)
    returnResponseBody x = return $ LBS.fromStrict $ Conduit.responseBody x

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

--getUpdates :: Integer -> IO [Update]
getUpdates h offset = do
  bsResponse <- doGetRequest h "getUpdates" [offsetParam]
  let maybeResponse = (decode bsResponse :: Maybe (Response [Update]))
  throwIfError maybeResponse "getUpdates"
  return $ maybe [] getUpdatesFromResult maybeResponse
  where
    getUpdatesFromResult maybeRes = fromMaybe [] (maybeRes & responseResult)
    offsetParam = ("offset", show offset)

--sendMessage :: ChatId -> String -> IO ()
sendMessage h chatId text = do
  bsResponse <- doGetRequest h "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

--forwardMessage :: ChatId -> MessageId -> IO ()
forwardMessage h chatId messageId = do
  bsResponse <- doGetRequest h "forwardMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "forwardMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    messageIdParam = ("message_id", show messageId)
    fromChatIdParam = ("from_chat_id", show chatId)
    queryPairs = [chatIdParam, messageIdParam, fromChatIdParam]

--forwardMessageNTimes :: ChatId -> MessageId -> Integer -> IO ()
forwardMessageNTimes h chatId messageId n =
  replicateM_ (fromInteger n) (forwardMessage h chatId messageId)

--sendKyboardWithText :: ChatId -> String -> IO ()
sendKyboardWithText h chatId text = do
  bsResponse <- doGetRequest h "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    keysAmount = h & hConfig & cKeysAmount
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    keyboardParam = ("reply_markup", getKeyboardJSON keysAmount)
    queryPairs = [chatIdParam, textParam, keyboardParam]
