module Logger.Types where

import           Data.Configurator
import           Data.Configurator.Types
import           Data.Text.Internal      (showText)

data Level
  = DEBUG
  | INFO
  | WARNING
  deriving (Show, Eq, Enum)

instance Ord Level where
  (<=) a b = fromEnum a <= fromEnum b

instance Configured Level where
  convert (String t) = Just $ getLevelFromString (showText t)

getLevelFromString :: String -> Level
getLevelFromString "DEBUG"   = DEBUG
getLevelFromString "INFO"    = INFO
getLevelFromString "WARNING" = WARNING
getLevelFromString _ = WARNING