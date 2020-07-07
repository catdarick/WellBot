{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator
--import           Telegram.Interact
import Control.Exception (SomeException, Exception, try)
import Data.Configurator.Types (Config)
--import qualified Vk.Interact as VK
import Logic
import Vk.Instances
import Telegram.Instances
main = do

  eitherCfg <- (try $ load [Required "/home/darick/wellbot/app/bot.cfg"]) -- :: IO (Either SomeException Config)
  either onExcept (start TgBot) eitherCfg
  --either onExcept Telegram.Interact.start eitherCfg
  return () 
  where
    onExcept (x::SomeException) = do
      print "Can't find config file"
      return ()
         

