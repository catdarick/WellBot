{-# LANGUAGE DeriveGeneric #-}

module Telegram.Keyboard.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics      (Generic)

data Keyboardbutton =
  Keyboardbutton
    { keyboardbuttonText :: String
    }
  deriving (Generic, Show)

instance ToJSON Keyboardbutton where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-------
data Replykeyboardmarkup =
  Replykeyboardmarkup
    { replykeyboardmarkupKeyboard        :: [[Keyboardbutton]]
    , replykeyboardmarkupOneTimeKeyboard :: Bool
    , replykeyboardmarkupResizeKeyboard  :: Bool
    }
  deriving (Generic, Show)

instance ToJSON Replykeyboardmarkup where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-------
data Messagewithkb =
  Messagewithkb
    { messagewithkbReplyMarkup :: Replykeyboardmarkup
    }
  deriving (Generic, Show)

instance ToJSON Messagewithkb where
  toJSON = genericToJSON $ aesonPrefix snakeCase
