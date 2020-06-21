{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib
import           Telegram.JsonTypes        
import           Network.HTTP.Simple            ( httpBS, getResponseBody )
import           Data.Aeson.Lens                (values,  key, _String )
import           Data.Text                      ( Text )               
import qualified Data.ByteString.Char8         as BS
import           Data.Aeson
import           Data.Aeson.Casing           
import GHC.Generics 
import Data.Aeson.Types (parse, Parser)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Network.HTTP.Conduit 
import Network.HTTP.Client.Conduit (setQueryString)
import Control.Monad.Trans.State (runStateT, evalStateT, evalState, runState, StateT)


import Data.Configurator
import Data.Configurator.Types  as CValue

import qualified Telegram.JsonTypes as TgTypes
import Telegram.Interact
import Data.Function ((&))

getData = [("key1", Just "value1"), ("key2", Just "value2")] :: [(BS.ByteString, Maybe BS.ByteString)]
getJSON :: IO BS.ByteString
getJSON = do
  let x = "https://api.telegram.org/bot1117485587:AAGcu3atF-i1WdLHgpl9pC2wh5s0E05ocpw/getUpdates"
  let t = setQueryString [("offset", Just "990352328")] x

  print t
  res <- httpBS t  
  return (getResponseBody res)

--tmpFunc :: StateT 
   

--main :: IO ()
main = do
  cfg <- load [Required "/home/darick/wellbot/app/bot.cfg"]
  config <- (Telegram.Interact.parseConfig cfg)
  Telegram.Interact.start config
  maybeRes <- evalStateT (Telegram.Interact.getUpdates config 990352328) 1

  --t <- Telegram.Interact.sendMessage config 322778141 "Hi bruh"
  --print t
  return ()
  --json <- getJSON
  --print ""
  --let bsJSON = fromStrict json
 -- BS.putStrLn json
  --print (decode bsJSON :: Maybe Response)



