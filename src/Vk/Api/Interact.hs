{-# LANGUAGE ScopedTypeVariables #-}

module Vk.Api.Interact where

import           Bot.ErrorException
import           Config
import           Control.Exception          (throw, try)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function              ((&))
import           Data.Maybe                 (fromMaybe, isJust)
import           GHC.Exception              (errorCallException)
import qualified Network.HTTP.Client        as Conduit
import           Network.HTTP.Simple        (HttpException, httpBS)
import           System.Random              (getStdRandom, randomR)
import           Vk.Keyboard.Builder

getRequest :: String -> String -> [(String, String)] -> IO LBS.ByteString
getRequest server method queryPairs = do
  initReq <- Conduit.parseRequest server
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  eitherBsResponse <- try $ httpBS req
  case eitherBsResponse of
    Left (e :: HttpException) -> throwM NoConnectionException
    Right bsResponse          -> returnResponseBody bsResponse
  where
    stringPairToByteStringPair (k, v) = (BS.pack k, Just $ BS.pack v)
    bsUrlPath = BS.pack method
    setPath reqVal path =
      reqVal {Conduit.path = (reqVal & Conduit.path) `mappend` path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    returnResponseBody x = return $ LBS.fromStrict $ Conduit.responseBody x

defaultServer :: String
defaultServer = "https://api.vk.com/"


