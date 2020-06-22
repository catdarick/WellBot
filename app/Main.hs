{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Configurator
import           Telegram.Interact

main = do
  cfg <- load [Required "/home/darick/wellbot/app/bot.cfg"]
  Telegram.Interact.start cfg
  return () 
