{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Api.Interact where

import           Config
import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, throw, toException,
                                              try)
import           Control.Monad               (replicateM, replicateM_, unless)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, get, modify, runStateT)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
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
import           Telegram.Api.Types
import           Telegram.Keyboard.Builder

doGetRequest :: Config -> String -> [(String, String)] -> IO LBS.ByteString
doGetRequest config method queryPairs = do
  initReq <- Conduit.parseRequest "https://api.telegram.org"
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  bsResponse <- httpBS req
  returnResponseBody bsResponse
  where
    timeoutQueryPair = ("timeout", show $ config & secTimeout)
    stringPairToByteStringPair (k, v) = (BS.pack k, Just $ BS.pack v)
    bsUrlPath = BS.pack ("bot" ++ (config & tgToken) ++ "/" ++ method)
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

getUpdates :: Config -> Integer -> IO [Update]
getUpdates config offset = do
  bsResponse <- doGetRequest config "getUpdates" [offsetParam]
  let maybeResponse = (decode bsResponse :: Maybe (Response [Update]))
  throwIfError maybeResponse "getUpdates"
  return $ maybe [] getUpdatesFromResult maybeResponse
  where
    getUpdatesFromResult maybeRes = fromMaybe [] (maybeRes & responseResult)
    offsetParam = ("offset", show offset)

sendMessage :: Config -> ChatId -> String -> IO ()
sendMessage config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

forwardMessage :: Config -> ChatId -> MessageId -> IO ()
forwardMessage config chatId messageId = do
  bsResponse <- doGetRequest config "forwardMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "forwardMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    messageIdParam = ("message_id", show messageId)
    fromChatIdParam = ("from_chat_id", show chatId)
    queryPairs = [chatIdParam, messageIdParam, fromChatIdParam]

sendKeyboardWithText :: Config -> ChatId -> String -> IO ()
sendKeyboardWithText config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    keyboardParam = ("reply_markup", getKeyboardJSON config)
    queryPairs = [chatIdParam, textParam, keyboardParam]
