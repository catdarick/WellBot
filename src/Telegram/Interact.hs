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
import           Data.ByteString.Internal    (inlinePerformIO)
import           Data.ByteString.Lazy.Char8  (ByteString, fromStrict, unpack)
import           Data.Char                   (isDigit)
import           Data.Function               ((&))
import           Data.Time                   (diffUTCTime, getCurrentTime,
                                              getTime_resolution)
import           GHC.Base                    (when)
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Simple         (getResponseBody, httpBS)
import           Telegram.Config
import qualified Telegram.Database.Interact  as DB
import qualified Telegram.Database.Types     as DB
import qualified Telegram.JsonTypes          as TgTypes
import           Telegram.Keyboard.Builder

doGetRequest :: Config -> String -> [(String, String)] -> StateT s IO ByteString
doGetRequest config method queryPairs = do
  initReq <- parseRequest "https://api.telegram.org"
  let req = setPathAndQueryString initReq urlPath bsQueryPairs
  eitherRes <- lift $ try $ httpBS req
  bsResponse <-
    lift $ either printErrorAndReturnEmpty returnResponseBody eitherRes
  return $ fromStrict bsResponse
  where
    stringPairToByteStringPair (k, v) = (pack k, Just $ pack v)
    urlPath = pack ("bot" ++ (config & tgToken) ++ "/" ++ method)
    setPath reqVal path = reqVal {path = path}
    setPathAndQueryString req path query =
      setQueryString query (setPath req urlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    printErrorAndReturnEmpty (e :: SomeException) = do
      print e
      return ("{}")
    returnResponseBody x = do
      return $ getResponseBody x

getUpdates config = do
  db <- get
  let offsetParam = ("offset", show (db & DB.offset))
  bsResponse <- doGetRequest config "getUpdates" [offsetParam]
  let maybeRes =
        (decode bsResponse :: Maybe (TgTypes.Response [TgTypes.Update]))
  return $ maybe [] getUpdatesFromResult maybeRes
  where
    getUpdatesFromResult res = res & TgTypes.responseResult

sendMessage config chatId text = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  return (decode bsResponse :: Maybe (TgTypes.Response TgTypes.Message))
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", text)
    queryPairs = [chatIdParam, textParam]

sendKyboard config chatId = do
  bsResponse <- doGetRequest config "sendMessage" queryPairs
  return (decode bsResponse :: Maybe (TgTypes.Response TgTypes.Message))
  where
    chatIdParam = ("chat_id", show chatId)
    textParam = ("text", config & repeatText)
    keyboardParam = ("reply_markup", getKeyboardJSON config)
    queryPairs = [chatIdParam, textParam, keyboardParam]

handleUpdate config update = do
  lift $ print update
  let updateId = update & TgTypes.updateUpdateId
  let maybeText = update & TgTypes.updateMessage & TgTypes.messageText
  let chatId =
        update & TgTypes.updateMessage & TgTypes.messageChat & TgTypes.chatId
  case maybeText of
    Just "/start" -> sendMessage config chatId (config & helpText) >> return ()
    Just "/help" -> sendMessage config chatId (config & helpText) >> return ()
    Just "/repeat" -> onRepeat config chatId >> return ()
    Just text -> onText chatId text
    Nothing -> sendMessage config chatId (config & sadText) >> return ()
  DB.setOffset (updateId + 1)
  where
    isKeyboardResponse chatId text = do
      isAwaiting <- DB.isAwaiting chatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = (toInteger $ fromEnum ch - fromEnum '0')
    onRepeat config chatId = do
      DB.addAwaitingChat chatId
      sendKyboard config chatId
    onText chatId text = do
      repAmount <- DB.getRepeatsAmount chatId
      isKeyboardResponse <- isKeyboardResponse chatId text
      if isKeyboardResponse
        then DB.setRepeatsAmount chatId (getIntByChar (head text))
        else replicateM_
               (fromInteger repAmount)
               (sendMessage config chatId text)
      DB.delAwaitingChat chatId

loop config = do
  let loopDelay = config & uSecLoopPeriod
  db <- get
  isTimeToBackup <- isTimeToBackup db
  if isTimeToBackup
    then backUpAndUpdateTimer
    else do
      return ()
  updates <- getUpdates config
  lift $ print (db)
  mapM_ (handleUpdate config) updates
  lift $ threadDelay loopDelay
  loop config
  where
    isTimeToBackup db = do
      curTime <- lift $ getCurrentTime
      return $
        diffUTCTime curTime (db & DB.prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup "./backup.dat"
      DB.updateTime

start configHandle = do
  config <- (parseConfig configHandle)
  initDB <- DB.getRestoredOrNewDatabase (config & defaultRepeatAmount)
  runStateT (loop config) initDB
  return ()
