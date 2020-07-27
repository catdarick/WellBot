{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Api.Interact where

import           Config
import           Control.Monad               (unless)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import qualified Network.HTTP.Client.Conduit as Conduit
import qualified Network.HTTP.Conduit        as Conduit
import           Network.HTTP.Simple         (httpBS)
import           Telegram.Api.Error
import           Telegram.Api.Types
import           Telegram.Keyboard.Builder

getRequest :: Config -> String -> [(String, String)] -> IO LBS.ByteString
getRequest config method queryPairs = do
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

getUpdates :: Config -> Offset -> IO [Update]
getUpdates config offset = do
  bsResponse <- getRequest config "getUpdates" [offsetParam]
  let maybeResponse = (decode bsResponse :: Maybe (Response [Update]))
  throwIfError maybeResponse "getUpdates"
  return $ maybe [] getUpdatesFromResult maybeResponse
  where
    getUpdatesFromResult maybeRes = fromMaybe [] (maybeRes & responseResult)
    offsetParam = ("offset", show offset)

sendMessage :: Config -> ChatId -> String -> IO ()
sendMessage config chatId text = do
  bsResponse <- getRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

forwardMessage :: Config -> ChatId -> MessageId -> IO ()
forwardMessage config chatId messageId = do
  bsResponse <- getRequest config "forwardMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "forwardMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    messageIdParam = ("message_id", show messageId)
    fromChatIdParam = ("from_chat_id", show chatId)
    queryPairs = [chatIdParam, messageIdParam, fromChatIdParam]

sendKeyboardWithText :: Config -> ChatId -> String -> IO ()
sendKeyboardWithText config chatId text = do
  bsResponse <- getRequest config "sendMessage" queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Message)
  throwIfError maybeResponse "sendMessage"
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    keyboardParam = ("reply_markup", getKeyboardJSON config)
    queryPairs = [chatIdParam, textParam, keyboardParam]
