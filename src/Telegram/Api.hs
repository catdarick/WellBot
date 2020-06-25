{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Api where

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, throw, toException,
                                              try)
import           Control.Monad               (replicateM, replicateM_, unless)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, get, modify, runStateT)
import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Char8       (pack)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  (ByteString, fromStrict, unpack)
import           Data.Char                   (isDigit)
import qualified Data.Configurator.Types     as DataConfigurator
import           Data.Function               ((&))
import           Data.Maybe                  (fromJust, fromMaybe, isJust,
                                              isNothing)
import           Data.Time                   (diffUTCTime, getCurrentTime)
import           GHC.Base                    (when)
import           GHC.Exception               (errorCallException)
import qualified Network.HTTP.Client.Conduit as Conduit
import qualified Network.HTTP.Conduit        as Conduit
import           Network.HTTP.Simple         (httpBS)
import           Telegram.Config
import qualified Telegram.Database.Interact  as DB
import qualified Telegram.Database.Types     as DB
import           Telegram.Keyboard.Builder
import           Telegram.Types

doGetRequest :: Config -> String -> [(String, String)] -> IO ByteString
doGetRequest config method queryPairs = do
  initReq <- Conduit.parseRequest "https://api.telegram.org"
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  bsResponse <- httpBS req
  returnResponseBody bsResponse
  where
    stringPairToByteStringPair (k, v) = (pack k, Just $ pack v)
    bsUrlPath = pack ("bot" ++ (config & tgToken) ++ "/" ++ method)
    setPath reqVal path = reqVal {Conduit.path = path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    returnResponseBody x = return $ fromStrict $ Conduit.responseBody x

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

getUpdates :: Config -> Integer -> IO [Update]
getUpdates config offset = do
  let offsetParam = ("offset", show offset)
  bsResponse <- doGetRequest config "getUpdates" [offsetParam]
  let maybeResponse = (decode bsResponse :: Maybe (Response [Update]))
  throwIfError maybeResponse "getUpdates"
  return $ maybe [] getUpdatesFromResult maybeResponse
  where
    getUpdatesFromResult maybeRes = fromMaybe [] (maybeRes & responseResult)

sendMessage :: Config -> Integer -> String -> IO ()
sendMessage config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

forwardMessage :: Config -> Integer -> Integer -> IO ()
forwardMessage config chatId messageId = do
  bsResponse <- doGetRequest config "forwardMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "forwardMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    messageIdParam = ("message_id", show messageId)
    fromChatIdParam = ("from_chat_id", show chatId)
    queryPairs = [chatIdParam, messageIdParam, fromChatIdParam]

forwardMessageNTimes :: Config -> Integer -> Integer -> Integer -> IO ()
forwardMessageNTimes config chatId messageId n =
  replicateM_ (fromInteger n) (forwardMessage config chatId messageId)

sendKyboardWithText :: Config -> Integer -> String -> IO ()
sendKyboardWithText config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    keyboardParam = ("reply_markup", getKeyboardJSON config)
    queryPairs = [chatIdParam, textParam, keyboardParam]
