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
import Data.Aeson (decode)
import Data.Function ((&))
import Control.Monad.Trans.State (modify)
import Control.Monad.Trans.Class (MonadTrans(lift))


data Config = Config
    { tgToken::String
    } deriving (Show)


parseConfig :: Configurator.Types.Config -> IO Telegram.Interact.Config
parseConfig cfg = do
    token <- Configurator.require cfg "tgToken" :: IO String
    return Config 
            {tgToken = token}

doGetRequest config method query= do
    initReq <- parseRequest "https://api.telegram.org"
    let bsQuery = map (\ (k, v) -> (pack k, Just $ pack v)) query
    let req = setQueryString bsQuery initReq { method = "GET"
                                             , path = pack ("bot" ++ tgToken config ++ "/" ++ method)}
    res <- httpBS req
    let bsRes = fromStrict $ getResponseBody res
    lift $ BS.putStrLn $ getResponseBody res
    return (bsRes)

--getUpdates :: Config -> Integer -> m (Maybe TgTypes.Response)
getUpdates config offset =  do 
    bsRes <- doGetRequest config "getUpdates" [("offset", show offset)]
    return (decode bsRes :: Maybe (TgTypes.Response [TgTypes.Update]))

--sendMessage :: Show a => Config -> a -> [Char] -> IO (Maybe (TgTypes.Response TgTypes.Message))
sendMessage config chatId text = do
    let query = [ ("chat_id", show chatId)
                , ("text", text)]
    bsRes <- doGetRequest config "sendMessage" query
    return (decode bsRes :: Maybe (TgTypes.Response TgTypes.Message))


handleUpdate config update = do
    let updateId = update&TgTypes.updateUpdateId
    let text = update&TgTypes.updateMessage&TgTypes.messageText
    let chatId = update&TgTypes.updateMessage&TgTypes.messageChat&TgTypes.chatId
    res <- sendMessage config chatId text
    case res of
        Nothing ->return ()
        Just _ ->  modify(\_ ->updateId + 1)


   

