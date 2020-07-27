{-# LANGUAGE DeriveGeneric #-}

module Telegram.Api.Types.Update where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Maybe                 (fromJust)
import           GHC.Generics               (Generic)
import           Telegram.Api.Types.Message

data Update =
  Update
    { updateUpdateId :: Integer
    , updateMessage  :: Maybe Message
    }
  deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
