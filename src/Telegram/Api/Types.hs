{-# LANGUAGE DeriveGeneric #-}

module Telegram.Api.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)

type ChatId = Integer

type MessageId = Integer

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
    { responseOk          :: Bool
    , responseResult      :: Maybe respType
    , responseErrorCode   :: Maybe Int
    , responseDescription :: Maybe String
    }
  deriving (Generic, Show)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
