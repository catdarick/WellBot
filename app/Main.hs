{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator
import           Telegram.Interact
import Control.Exception (SomeException, Exception, try)
import Data.Configurator.Types (Config)


main = do
 -- x <- catch (readFile "/tmp/foo.txt") handler
  eitherCfg <- (try $ load [Required "/home/darick/wellbot/app/bot.cfg"]) -- :: IO (Either SomeException Config)
  either onExcept Telegram.Interact.start eitherCfg
  return () 
  where
    onExcept (x::SomeException) = do
      print "Can't find config file"
      return ()
         

