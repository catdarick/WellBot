{-# LANGUAGE OverloadedStrings #-}

module Telegram.Config where

import           Data.Configurator (require)
import           Data.Fixed        (Pico)
import           Data.Time         (NominalDiffTime, UTCTime, DiffTime, secondsToDiffTime)

data Config =
  Config
    { tgToken               :: String
    , helpText              :: String
    , repeatText            :: String
    , sadText               :: String
    , keysAmount              :: Integer
    , defaultRepeatAmount   :: Integer
    , uSecLoopPeriod        :: Int
    , diffTimeBackupPeriod :: NominalDiffTime
    }
  deriving (Show)

parseConfig cfg = do
  tgToken <- require cfg "tgToken" :: IO String
  helpText <- require cfg "commandHelpText" :: IO String
  repeatText <- require cfg "commandRepeatText" :: IO String
  keysAmount <- require cfg "tgKeysAmount" :: IO Integer
  defaultRepeatAmount <- require cfg "defaultRepeatAmount" :: IO Integer
  sadText <- require cfg "sadText" :: IO String
  uSecLoopPeriod <- require cfg "uSecLoopPeriod" :: IO Int
  secPeriod <- require cfg "secDatabaseBackupPeriod" :: IO Int
  let backupPeriod = sum $ replicate secPeriod (1 :: NominalDiffTime)
  return
    Config
      { tgToken = tgToken
      , helpText = helpText
      , repeatText = repeatText
      , keysAmount = keysAmount
      , defaultRepeatAmount = defaultRepeatAmount
      , sadText = sadText
      , uSecLoopPeriod = uSecLoopPeriod
      , diffTimeBackupPeriod = backupPeriod
      }
