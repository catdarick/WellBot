{-# LANGUAGE OverloadedStrings #-}

module Telegram.Interact where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (replicateM)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, get, modify, runStateT)
import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Char8       (pack)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Internal    (inlinePerformIO)
import           Data.ByteString.Lazy.Char8  (fromStrict)
import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.ByteString.Lazy.Char8  (unpack)
import           Data.Char                   (isDigit)
import qualified Data.Configurator           as Configurator
import qualified Data.Configurator.Types     as Configurator.Types
import           Data.Function               ((&))
import           Data.UnixTime               (getUnixTime)
import qualified Data.UnixTime               as UT
import           Foreign.C                   (CTime (CTime))
import           GHC.Base                    (when)
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Simple         (getResponseBody, httpBS)
import qualified Telegram.JsonTypes          as TgTypes
import qualified Telegram.RuntimeDB          as DB

data Config =
  Config
    { tgToken             :: String
    , helpText            :: String
    , repeatText          :: String
    , keysAmount          :: Integer
    , defaultRepeatAmount :: Integer
    }
  deriving (Show)

getKeyboardJSON config = do
  let kAmount = config & keysAmount
  encode $
    TgTypes.Replykeyboardmarkup
      { TgTypes.replykeyboardmarkupKeyboard = [map intToKey [1,2 .. kAmount]]
      , TgTypes.replykeyboardmarkupOneTimeKeyboard = True
      , TgTypes.replykeyboardmarkupResizeKeyboard = True
      }
  where
    intToKey x = TgTypes.Keyboardbutton {TgTypes.keyboardbuttonText = show x}

parseConfig :: Configurator.Types.Config -> IO Telegram.Interact.Config
parseConfig cfg = do
  token <- Configurator.require cfg "tgToken" :: IO String
  help <- Configurator.require cfg "commandHelpText" :: IO String
  repeat <- Configurator.require cfg "commandRepeatText" :: IO String
  keysAm <- Configurator.require cfg "tgKeysAmount" :: IO Integer
  keysAm <- Configurator.require cfg "tgKeysAmount" :: IO Integer
  repAm <- Configurator.require cfg "defaultRepeatAmount" :: IO Integer
  return
    Config
      { tgToken = token
      , helpText = help
      , repeatText = repeat
      , keysAmount = keysAm
      , defaultRepeatAmount = repAm
      }

doGetRequest :: Config -> [Char] -> [(String, String)] -> StateT s IO ByteString
doGetRequest config method query = do
  initReq <- parseRequest "https://api.telegram.org"
  let bsQuery = map (\(k, v) -> (pack k, Just $ pack v)) query
  let req =
        setQueryString
          bsQuery
          initReq
            { method = "GET"
            , path = pack ("bot" ++ tgToken config ++ "/" ++ method)
            }
   -- lift $ print initReq
  res <- httpBS req
  let bsRes = fromStrict $ getResponseBody res
  return (bsRes)

doPostRequest ::
     Num s
  => Config
  -> [Char]
  -> [(String, String)]
  -> ByteString
  -> StateT s IO ByteString
doPostRequest config method query jsonData = do
  initReq <- parseRequest "https://api.telegram.org"
  let bsQuery = map (\(k, v) -> (pack k, Just $ pack v)) query
  let req =
        setQueryString
          bsQuery
          initReq
            { method = "POST"
            , path = pack ("bot" ++ tgToken config ++ "/" ++ method)
            }
  lift $ print req
  res <- httpBS req
  let bsRes = fromStrict $ getResponseBody res
  lift $ print bsRes
  return (bsRes)

--getUpdates :: Config -> Integer -> m (Maybe TgTypes.Response)
getUpdates config = do
  db <- get
  bsRes <- doGetRequest config "getUpdates" [("offset", show (db & DB.offset))]
  let maybeRes = (decode bsRes :: Maybe (TgTypes.Response [TgTypes.Update]))
    --lift $ print bsRes
    --lift $ print offset
  case maybeRes of
    Just res -> return (Just $ res & TgTypes.responseResult)
    Nothing  -> return (Nothing)

--sendMessage :: Show a => Config -> a -> [Char] -> IO (Maybe (TgTypes.Response TgTypes.Message))
sendMessage config chatId text = do
  let query = [("chat_id", show chatId), ("text", text)]
  bsRes <- doGetRequest config "sendMessage" query
  return (decode bsRes :: Maybe (TgTypes.Response TgTypes.Message))

sendKyboard config chatId = do
  let query =
        [ ("chat_id", show chatId)
        , ("text", config & repeatText)
        , ("reply_markup", unpack $ getKeyboardJSON config)
        ]
  bsRes <- doGetRequest config "sendMessage" query
  return (decode bsRes :: Maybe (TgTypes.Response TgTypes.Message))

handleUpdate config update = do
  let updateId = update & TgTypes.updateUpdateId
  let text = update & TgTypes.updateMessage & TgTypes.messageText
  let chatId =
        update & TgTypes.updateMessage & TgTypes.messageChat & TgTypes.chatId
  repAmount <- DB.getRepeatsAmount chatId
  res <-
    case text of
      "/start" -> do
        t <- sendMessage config chatId (config & helpText)
        DB.setOffset (updateId + 1)
        return ()
      "/help" -> do
        t <- sendMessage config chatId (config & helpText)
        DB.setOffset (updateId + 1)
        return ()
      "/repeat" -> do
        t <- sendKyboard config chatId
        DB.addAwaitingChat chatId
        DB.setOffset (updateId + 1)
        return ()
      something -> do
        isAwaiting <- DB.isAwaiting chatId
        if isAwaiting && (length text == 1 && isDigit (head text))
          then do 
            DB.setRepeatsAmount chatId (toInteger $ fromEnum (head text) - fromEnum '0')
            DB.setOffset (updateId + 1)
            return ()
          else do
            lift $ print repAmount
            replicateM (fromInteger repAmount) (sendMessage config chatId something)
            DB.setOffset (updateId + 1)
            return ()
        DB.delAwaitingChat chatId
                --return ()
  return ()

loop config = do
  db <- get
  lift $ print (db)
  maybeRes <- getUpdates config
  case maybeRes of
    Just updates -> do
      mapM_ (handleUpdate config) updates
    Nothing -> do
      return ()
  lift $ threadDelay 400000
  loop config

start :: Config -> IO ()
start config = do
  print $ getKeyboardJSON config
  runStateT
    (loop config)
    (DB.DB
       { DB.offset = 0
       , DB.defaultRepeatAmount = config & defaultRepeatAmount
       , DB.chats = []
       , DB.awaitingChatsID = []
       })
  return ()
