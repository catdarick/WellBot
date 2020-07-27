{-# LANGUAGE DeriveGeneric #-}

module Vk.Keyboard.Types.Keyboard where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics             (Generic)
import           Vk.Keyboard.Types.Button

data Keyboard =
  Keyboard
    { keyboardButtons :: [[Button]]
    , keyboardOneTime :: Bool
    , keyboardInline  :: Bool
    }
  deriving (Generic, Show)

instance ToJSON Keyboard where
  toJSON = genericToJSON $ aesonPrefix snakeCase
