{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.State   (StateT, evalState, evalStateT,
                                              runState, runStateT)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Lens             (key, values, _String)
import           Data.Aeson.Types            (Parser, parse)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy.Char8  (fromStrict)
import           Data.Text                   (Text)
import           GHC.Generics
import           Lib
import           Network.HTTP.Client.Conduit (setQueryString)
import           Network.HTTP.Conduit
import           Network.HTTP.Simple         (getResponseBody, httpBS)
import           Telegram.JsonTypes

import           Data.Configurator
import           Data.Configurator.Types     as CValue

import           Control.Monad               (when)
import           Data.Function               ((&))
import           Telegram.Interact
import qualified Telegram.JsonTypes          as TgTypes

main = do
  cfg <- load [Required "/home/darick/wellbot/app/bot.cfg"]
  config <- (Telegram.Interact.parseConfig cfg)
  Telegram.Interact.start config

  return ()--json <- getJSON
