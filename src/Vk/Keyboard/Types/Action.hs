{-# LANGUAGE DeriveGeneric #-}

module Vk.Keyboard.Types.Action where

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
