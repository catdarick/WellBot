{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.Configurator (require)
import           Data.Fixed        (Pico)
import           Data.Time         (DiffTime, NominalDiffTime, UTCTime,
                                    secondsToDiffTime)
import qualified Logger.Types      as Log

data Config =
  Config
    { tgToken              :: String
    , helpText             :: String
    , repeatText           :: String
    , sadText              :: String
    , keysAmount           :: Integer
    , defaultRepeatAmount  :: Integer
    , secTimeout           :: Int
    , diffTimeBackupPeriod :: NominalDiffTime
    , vkToken              :: String
    , vkGroupId            :: Integer
    , vkApiVersion         :: String
    , backupPath           :: String
    , logPath              :: String
    , logSinceLevel        :: Log.Level
    }
  deriving (Show)

parseConfig cfg = do
  tgToken <- require cfg "tgToken" :: IO String
  helpText <- require cfg "commandHelpText" :: IO String
  repeatText <- require cfg "commandRepeatText" :: IO String
  keysAmount <- require cfg "tgKeysAmount" :: IO Integer
  defaultRepeatAmount <- require cfg "defaultRepeatAmount" :: IO Integer
  sadText <- require cfg "sadText" :: IO String
  secTimeout <- require cfg "secLongpollingTimeout" :: IO Int
  secPeriod <- require cfg "secDatabaseBackupPeriod" :: IO Int
  vkToken <- require cfg "vkToken" :: IO String
  vkGroupId <- require cfg "vkGroupId" :: IO Integer
  vkApiVersion <- require cfg "vkApiVersion" :: IO String
  backupPath <- require cfg "backupPath" :: IO String
  logPath <- require cfg "logPath" :: IO String
  logSinceLevel <- require cfg "logSinceLevel" :: IO Log.Level
  let backupPeriod = sum $ replicate secPeriod (1 :: NominalDiffTime)
  return
    Config
      { tgToken = tgToken
      , helpText = helpText
      , repeatText = repeatText
      , keysAmount = keysAmount
      , defaultRepeatAmount = defaultRepeatAmount
      , sadText = sadText
      , secTimeout = secTimeout
      , diffTimeBackupPeriod = backupPeriod
      , vkToken = vkToken
      , vkGroupId = vkGroupId
      , vkApiVersion = vkApiVersion
      , backupPath = backupPath
      , logPath = logPath
      , logSinceLevel = logSinceLevel
      }
