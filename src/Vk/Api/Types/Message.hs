{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Message where

import           Data.Aeson
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)
import           Vk.Api.Types.Synonyms

data Message =
  Message
    { messageFromId :: UserId
    , messageId     :: MessageId
    , messageText   :: Maybe String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
