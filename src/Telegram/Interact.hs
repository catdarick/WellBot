{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Interact where

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, try)
import           Control.Monad               (replicateM, replicateM_)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, get, modify, runStateT)
import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Char8       (pack)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  (ByteString, fromStrict, unpack)
import           Data.Char                   (isDigit)
import           Data.Function               ((&))
import           Data.Maybe                  (fromJust, isJust, isNothing)
import           Data.Time                   (diffUTCTime, getCurrentTime)
import           GHC.Base                    (when)
import qualified Network.HTTP.Client.Conduit as Conduit
import qualified Network.HTTP.Conduit        as Conduit
import           Network.HTTP.Simple         (httpBS)
import           Telegram.Config
import qualified Telegram.Database.Interact  as DB
import qualified Telegram.Database.Types     as DB
import           Telegram.Keyboard.Builder
import           Telegram.Types
import qualified Data.Configurator.Types as DataConfigurator

doGetRequest :: Config -> String -> [(String, String)] -> StateT s IO ByteString
doGetRequest config method queryPairs = do
  initReq <- Conduit.parseRequest "https://api.telegram.org"
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  eitherRes <- lift $ try $ httpBS req
  bsResponse <-
    lift $ either printErrorAndReturnEmpty returnResponseBody eitherRes
  return $ fromStrict bsResponse
  where
    stringPairToByteStringPair (k, v) = (pack k, Just $ pack v)
    bsUrlPath = pack ("bot" ++ (config & tgToken) ++ "/" ++ method)
    setPath reqVal path = reqVal {Conduit.path = path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    printErrorAndReturnEmpty (e :: SomeException) = do
      print e
      return ("{}")
    returnResponseBody x = do
      return $ Conduit.responseBody x

getUpdates :: Config -> StateT DB.DB IO [Update]
getUpdates config = do
  db <- get
  let offsetParam = ("offset", show (db & DB.offset))
  bsResponse <- doGetRequest config "getUpdates" [offsetParam]
  let maybeRes = (decode bsResponse :: Maybe (Response [Update]))
  return $ maybe [] getUpdatesFromResult maybeRes
  where
    getUpdatesFromResult res = res & responseResult

sendMessage ::
     Show a => Config -> a -> String -> StateT s IO (Maybe (Response Message))
sendMessage config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  return (decode bsResponse :: Maybe (Response Message))
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

forwardMessage ::
     (Show a2, Show a1)
  => Config
  -> a1
  -> a2
  -> StateT s IO (Maybe (Response Message))
forwardMessage config chatId messageId = do
  bsResponse <- doGetRequest config "forwardMessage" queryPairs
  return (decode bsResponse :: Maybe (Response Message))
  where
    chatIdParam = ("chat_id", show chatId)
    messageIdParam = ("message_id", show messageId)
    fromChatIdParam = ("from_chat_id", show chatId)
    queryPairs = [chatIdParam, messageIdParam, fromChatIdParam]

sendKyboard :: Show a => Config -> a -> StateT s IO (Maybe (Response Message))
sendKyboard config chatId = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  return (decode bsResponse :: Maybe (Response Message))
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", config & repeatText)
    keyboardParam = ("reply_markup", getKeyboardJSON config)
    queryPairs = [chatIdParam, textParam, keyboardParam]

handleUpdate :: Config -> Update -> StateT DB.DB IO ()
handleUpdate config update = do
  let updateId = update & updateUpdateId
  let message = fromJust (update & updateMessage)
  let maybeText = message & messageText
  let messageId = message & messageMessageId
  let chatId_ = message & messageChat & chatId
  repAmount <- DB.getRepeatsAmount chatId_
  case maybeText of
    Just "/start" -> sendMessage config chatId_ (config & helpText) >> return ()
    Just "/help" -> sendMessage config chatId_ (config & helpText) >> return ()
    Just "/repeat" -> onRepeat config chatId_ >> return ()
    Just text -> onText chatId_ text messageId
    Nothing -> forwardMessageNTimes chatId_ messageId repAmount >> return ()
  DB.setOffset (updateId + 1)
  where
    isKeyboardResponse chatId text = do
      isAwaiting <- DB.isAwaiting chatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = (toInteger $ fromEnum ch - fromEnum '0')
    forwardMessageNTimes chatId messageId n =
      replicateM_ (fromInteger n) (forwardMessage config chatId messageId)
    onRepeat config chatId = do
      DB.addAwaitingChat chatId
      sendKyboard config chatId
    onText chatId text messageId = do
      repAmount <- DB.getRepeatsAmount chatId
      isKeyboardResponse <- isKeyboardResponse chatId text
      if isKeyboardResponse
        then DB.setRepeatsAmount chatId (getIntByChar (head text))
        else forwardMessageNTimes chatId messageId repAmount
      DB.delAwaitingChat chatId

loop :: Config -> StateT DB.DB IO b
loop config = do
  isTimeToBackup <- isTimeToBackup
  if isTimeToBackup
    then backUpAndUpdateTimer
    else continue
  updates <- getUpdates config
  let filtredUpdates = filter isJustMessage updates
  mapM_ (handleUpdate config) filtredUpdates
  loop config
  where
    isJustMessage Update {updateMessage = maybeMessage} = isJust maybeMessage
    loopDelay = config & uSecLoopPeriod
    continue = do
      return ()
    isTimeToBackup = do
      prevTime <- DB.getFromDatabase DB.prevTime
      curTime <- lift $ getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup "./backup.dat"
      DB.updateTime

start :: DataConfigurator.Config -> IO ()
start configHandle = do
  config <- (parseConfig configHandle)
  initDB <- DB.getRestoredOrNewDatabase (config & defaultRepeatAmount)
  runStateT (loop config) initDB
  return ()
