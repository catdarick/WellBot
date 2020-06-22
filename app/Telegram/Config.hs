{-# LANGUAGE OverloadedStrings #-}

module Telegram.Config where

import           Data.Configurator (require)

data Config =
  Config
    { tgToken             :: String
    , helpText            :: String
    , repeatText          :: String
    , keysAmount          :: Integer
    , defaultRepeatAmount :: Integer
    }
  deriving (Show)

parseConfig cfg = do
  token <- require cfg "tgToken" :: IO String
  help <- require cfg "commandHelpText" :: IO String
  repeat <- require cfg "commandRepeatText" :: IO String
  keysAm <- require cfg "tgKeysAmount" :: IO Integer
  keysAm <- require cfg "tgKeysAmount" :: IO Integer
  repAm <- require cfg "defaultRepeatAmount" :: IO Integer
  return
    Config
      { tgToken = token
      , helpText = help
      , repeatText = repeat
      , keysAmount = keysAm
      , defaultRepeatAmount = repAm
      }
