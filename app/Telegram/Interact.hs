{-# LANGUAGE OverloadedStrings #-}
module Telegram.Interact where

import Network.HTTP.Client.Conduit 
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types  as Configurator.Types
import Network.HTTP.Conduit 
import Data.ByteString.Char8 (pack)
import Network.HTTP.Simple (getResponseBody, httpBS)
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.ByteString.Char8         as BS
import qualified Telegram.JsonTypes as TgTypes
import Data.Aeson (encode, decode)
import Data.Function ((&))
import Control.Monad.Trans.State (runStateT, get, StateT, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.UnixTime (getUnixTime)
import qualified Data.UnixTime as UT
import Foreign.C (CTime(CTime))
import Data.ByteString.Internal (inlinePerformIO)
import Data.ByteString.Lazy.Char8 (ByteString)
import GHC.Base (when)
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy.Char8 (unpack)


data Config = Config
    { tgToken::String
    , helpText::String
    , repeatText::String
    , keysAmount::Integer
    } deriving (Show)


getKeyboardJSON config = do --"{\"inline_keyboard\":[[{\"text\":\"A\",\"callback_data\":\"A1\"},{\"text\":\"B\",\"callback_data\":\"C1\"}]]}"
    let kAmount = config&keysAmount
    encode $ TgTypes.Replykeyboardmarkup 
        { TgTypes.replykeyboardmarkupKeyboard = map intToKey [1,2..kAmount]
        , TgTypes.replykeyboardmarkupOneTimeKeyboard = True} where
            intToKey x = [TgTypes.Keyboardbutton {TgTypes.keyboardbuttonText = show x}]
 
parseConfig :: Configurator.Types.Config -> IO Telegram.Interact.Config
parseConfig cfg = do
    token  <- Configurator.require cfg "tgToken"           :: IO String
    help   <- Configurator.require cfg "commandHelpText"   :: IO String
    repeat <- Configurator.require cfg "commandRepeatText" :: IO String
    keysAm <- Configurator.require cfg "tgKeysAmount" :: IO Integer
    return Config { tgToken = token
                  , helpText = help
                  , repeatText = repeat
                  , keysAmount = keysAm}

doGetRequest :: Num s => Config -> [Char] -> [(String, String)] -> StateT s IO ByteString
doGetRequest config method query= do
    initReq <- parseRequest "https://api.telegram.org"
    
    let bsQuery = map (\ (k, v) -> (pack k, Just $ pack v)) query
    let req = setQueryString bsQuery initReq { method = "GET"
                                             , path = pack ("bot" ++ tgToken config ++ "/" ++ method)}
   -- lift $ print initReq                                         
    res <- httpBS req
    let bsRes = fromStrict $ getResponseBody res
    return (bsRes)

doPostRequest :: Num s => Config -> [Char] -> [(String, String)] -> ByteString-> StateT s IO ByteString
doPostRequest config method query jsonData = do
    initReq <- parseRequest "https://api.telegram.org"
    let bsQuery = map (\ (k, v) -> (pack k, Just $ pack v)) query
    let req = setQueryString bsQuery initReq { method = "POST"
                                             , path = pack ("bot" ++ tgToken config ++ "/" ++ method)
                                             --, requestBody = RequestBodyLBS jsonData 
                                             }
    lift $ print req                                         
    res <- httpBS req
    let bsRes = fromStrict $ getResponseBody res
    lift $ print bsRes
    return (bsRes)

--getUpdates :: Config -> Integer -> m (Maybe TgTypes.Response)
getUpdates config offset =  do 
    bsRes <- doGetRequest config "getUpdates" [("offset", show offset)]
    let maybeRes =  (decode bsRes :: Maybe (TgTypes.Response [TgTypes.Update]))
    --lift $ print bsRes
    --lift $ print offset
    case maybeRes of
        Just res -> return (Just $ res&TgTypes.responseResult)
        Nothing  -> return (Nothing)

--sendMessage :: Show a => Config -> a -> [Char] -> IO (Maybe (TgTypes.Response TgTypes.Message))
sendMessage config chatId text = do
    let query = [ ("chat_id", show chatId)
                , ("text", text)]
    bsRes <- doGetRequest config "sendMessage" query
    return (decode bsRes :: Maybe (TgTypes.Response TgTypes.Message))

sendKyboard config chatId = do
    let query = [ ("chat_id", show chatId)
                , ("text", config&repeatText)
                , ("reply_markup", unpack $ getKeyboardJSON config)]
    bsRes <- doGetRequest config "sendMessage" query 
    return (decode bsRes :: Maybe (TgTypes.Response TgTypes.Message))

handleUpdate config update = do
    let updateId = update&TgTypes.updateUpdateId
    let text = update&TgTypes.updateMessage&TgTypes.messageText
    let chatId = update&TgTypes.updateMessage&TgTypes.messageChat&TgTypes.chatId
    res  <- case text of
                "/start"  -> sendMessage config chatId (config&helpText)
                "/help"  -> sendMessage config chatId (config&helpText) 
                "/repeat" -> sendKyboard config chatId
                 --   sendMessage config chatId (config&repeatText)
                    
                something -> sendMessage config chatId something
    case res of
        Nothing -> return ()
        Just _  -> modify(\_ ->updateId + 1)

loop config = do 
    offset <- get
    maybeRes <- getUpdates config offset
    case maybeRes of
        Just updates -> do
            --lift $ print updates
            mapM_ (handleUpdate config) updates          
        Nothing  -> do 
            return ()
    lift $ threadDelay 400000
    loop config




start :: Config -> IO ()
start config = do
    print $ getKeyboardJSON config
    runStateT (loop config) 1
    return ()
    