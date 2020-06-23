{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator
import           Telegram.Interact
import Control.Exception (SomeException, Exception, try)


main = do
 -- x <- catch (readFile "/tmp/foo.txt") handler
  cfg <- load [Required "/home/darick/wellbot/app/bot.cfg"]
  Telegram.Interact.start cfg
  return () 

