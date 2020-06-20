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

getJSON :: IO BS.ByteString
getJSON = do
  res <- httpBS "https://api.telegram.org/bot1117485587:AAGcu3atF-i1WdLHgpl9pC2wh5s0E05ocpw/getUpdates"
  return (getResponseBody res)

main :: IO ()
main = do
  json <- getJSON
  let bsJSON = fromStrict json
  BS.putStrLn json
  print (decode bsJSON :: Maybe Response)



