{-# LANGUAGE DeriveGeneric #-}

module Telegram.Keyboard.Types.Markup where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics                   (Generic)
import           Telegram.Keyboard.Types.Button

data Markup =
  Markup
    { markupKeyboard        :: [[Button]]
    , markupOneTimeKeyboard :: Bool
    , markupResizeKeyboard  :: Bool
    }
  deriving (Generic, Show)

instance ToJSON Markup where
  toJSON = genericToJSON $ aesonPrefix snakeCase
