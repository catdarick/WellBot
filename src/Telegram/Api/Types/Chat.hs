{-# LANGUAGE DeriveGeneric #-}

module Telegram.Api.Types.Chat where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)

newtype Chat =
  Chat
    { chatId :: Integer
    }
  deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
