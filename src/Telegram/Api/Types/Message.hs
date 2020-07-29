{-# LANGUAGE DeriveGeneric #-}

module Telegram.Api.Types.Message where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics            (Generic)
import           Telegram.Api.Types.Chat

data Message =
  Message
    { messageMessageId :: Integer
    , messageText      :: Maybe String
    , messageChat      :: Chat
    }
  deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
