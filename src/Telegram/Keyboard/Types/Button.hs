{-# LANGUAGE DeriveGeneric #-}

module Telegram.Keyboard.Types.Button where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics      (Generic)

newtype Button =
  Button
    { buttonText :: String
    }
  deriving (Generic, Show)

instance ToJSON Button where
  toJSON = genericToJSON $ aesonPrefix snakeCase
