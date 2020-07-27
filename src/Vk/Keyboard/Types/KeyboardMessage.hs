{-# LANGUAGE DeriveGeneric #-}

module Vk.Keyboard.Types.KeyboardMessage where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics               (Generic)
import           Vk.Keyboard.Types.Keyboard

data Messagewithkb =
  Messagewithkb
    { messagewithkbReplyMarkup :: Keyboard
    }
  deriving (Generic, Show)

instance ToJSON Messagewithkb where
  toJSON = genericToJSON $ aesonPrefix snakeCase
