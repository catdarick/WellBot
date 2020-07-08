{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Config
import           Data.Configurator
import Control.Exception (SomeException, Exception, try)
import Data.Configurator.Types (Config)
import Logic
import Vk.Instances
import Telegram.Instances
main = do
  eitherCfg <- (try $ load [Required "/home/darick/wellbot/app/bot.cfg"]) -- :: IO (Either SomeException Config)
  case eitherCfg of
    Left (e::SomeException) -> print "Can't find config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      start TgBot config


         

