{-# LANGUAGE DeriveGeneric #-}

module Vk.Keyboard.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics      (Generic)

data Action =
  Action
    { actionType    :: String
    , actionLabel   :: String
    , actionPayload :: String
    }
  deriving (Generic, Show)

instance ToJSON Action where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-------
data Button =
  Button
    { buttonAction :: Action
    , buttonColor  :: String
    }
  deriving (Generic, Show)

instance ToJSON Button where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-------
data Keyboard =
  Keyboard
    { keyboardButtons :: [[Button]]
    , keyboardOneTime :: Bool
    , keyboardInline  :: Bool
    }
  deriving (Generic, Show)

instance ToJSON Keyboard where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-------
data Messagewithkb =
  Messagewithkb
    { messagewithkbReplyMarkup :: Keyboard
    }
  deriving (Generic, Show)

instance ToJSON Messagewithkb where
  toJSON = genericToJSON $ aesonPrefix snakeCase
