{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics      (Generic)

import Data.Maybe (fromJust)
type ChatId = Integer
type MessageId = Integer
-------
newtype User =
  User
    { userId :: Integer
    }
  deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
newtype Chat =
  Chat
    { chatId :: Integer
    }
  deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Message =
  Message
    { messageMessageId :: Integer
    , messageFrom      :: User
    , messageText      :: Maybe String
    , messageChat      :: Chat
    }
  deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Update =
  Update
    { updateUpdateId :: Integer
    , updateMessage  :: Maybe Message
    }
  deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


-------
data Response respType =
  Response
    { responseOk     :: Bool
    , responseResult :: Maybe respType
    , responseErrorCode :: Maybe Int
    , responseDescription :: Maybe String
    }
  deriving (Generic, Show)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
