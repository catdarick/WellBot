{-# LANGUAGE DeriveGeneric #-}

module Vk.Keyboard.Types.Button where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics             (Generic)
import           Vk.Keyboard.Types.Action

data Button =
  Button
    { buttonAction :: Action
    , buttonColor  :: String
    }
  deriving (Generic, Show)

instance ToJSON Button where
  toJSON = genericToJSON $ aesonPrefix snakeCase
