{-# LANGUAGE DeriveGeneric #-}

module Telegram.Keyboard.Types.KeyboardMessage where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics                   (Generic)
import           Telegram.Keyboard.Types.Markup

data Messagewithkb =
  Messagewithkb
    { messagewithkbReplyMarkup :: Markup
    }
  deriving (Generic, Show)

instance ToJSON Messagewithkb where
  toJSON = genericToJSON $ aesonPrefix snakeCase
