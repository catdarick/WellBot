{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types where


import           Data.Aeson.Casing (aesonPrefix, snakeCase)
import           Data.Aeson.Types
import           Data.Function     ((&))
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)

newtype Response respType =
  Response
    { responseResponse :: respType
    }
  deriving (Generic, Show, Eq)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Longpoll =
  Longpoll
    { longpollKey    :: String
    , longpollServer :: String
    , longpollTs     :: String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Longpoll where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Update =
  Update
    { updateType   :: String
    , updateObject :: Objectt
    }
  deriving (Generic, Show, Eq)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase



-------
data Message =
  Message
    { messageFromId :: Integer
    , messageId     :: Integer
    , messageText   :: Maybe String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Updates =
  Updates
    { updatesTs      :: String
    , updatesUpdates :: [Update]
    }
  deriving (Generic, Show, Eq)

instance FromJSON Updates where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Objectt =
  Objectt
    { objecttMessage :: Maybe Message
    }
  deriving (Generic, Show, Eq)

instance FromJSON Objectt where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------
data Error =
  Error
    { errorFailed :: Integer
    }
  deriving (Generic, Show, Eq)

instance FromJSON Error where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
