{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad           (when)
import           Data.Configurator       (require)
import qualified Data.Configurator.Types as Lib
import           Data.Fixed              (Pico)
import           Data.Function           ((&))
import           Data.Time               (DiffTime, NominalDiffTime, UTCTime,
                                          secondsToDiffTime)
import qualified Logger.Types            as Log

data Config =
  Config
    { tgToken              :: String
    , helpText             :: String
    , repeatText           :: String
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
    , vkEnabled            :: Bool
    , tgEnabled            :: Bool
    }
  deriving (Show, Eq)

parseConfig :: Lib.Config -> IO Config
parseConfig cfg = do
  tgToken <- require cfg "tgToken"
  helpText <- require cfg "commandHelpText"
  repeatText <- require cfg "commandRepeatText"
  keysAmount <- require cfg "keysAmount"
  defaultRepeatAmount <- require cfg "defaultRepeatAmount"
  secTimeout <- require cfg "secLongpollingTimeout"
  secPeriod <- require cfg "secDatabaseBackupPeriod"
  vkToken <- require cfg "vkToken"
  vkGroupId <- require cfg "vkGroupId"
  vkApiVersion <- require cfg "vkApiVersion"
  backupPath <- require cfg "backupPath"
  logPath <- require cfg "logPath"
  logSinceLevel <- require cfg "logSinceLevel"
  vkEnabled <- require cfg "vkEnabled"
  tgEnabled <- require cfg "tgEnabled"
  let backupPeriod = sum $ replicate secPeriod (1 :: NominalDiffTime)
  return
    Config
      { tgToken = tgToken
      , helpText = helpText
      , repeatText = repeatText
      , keysAmount = keysAmount
      , defaultRepeatAmount = defaultRepeatAmount
      , secTimeout = secTimeout
      , diffTimeBackupPeriod = backupPeriod
      , vkToken = vkToken
      , vkGroupId = vkGroupId
      , vkApiVersion = vkApiVersion
      , backupPath = backupPath
      , logPath = logPath
      , logSinceLevel = logSinceLevel
      , vkEnabled = vkEnabled
      , tgEnabled = tgEnabled
      }

checkConfig :: Config -> Either String ()
checkConfig config
  | (config & vkEnabled) && (config & vkToken) == "***" = Left "vkToken"
  | (config & vkEnabled) && (config & vkGroupId) == 111111111 = Left "vkGroupId"
  | (config & tgEnabled) && (config & tgToken) == "***:***" = Left "tgToken"
  | otherwise = Right ()
