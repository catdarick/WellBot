{-# LANGUAGE DeriveGeneric #-}
module Telegram.JsonTypes where
import Data.Aeson 
import Data.Aeson.Casing 
import GHC.Generics (Generic)

-------

data User = User 
    { userId::Integer
    } deriving (Generic, Show)

instance ToJSON User where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON User where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------

data Chat = Chat 
    { chatId::Integer
    } deriving (Generic, Show)

instance ToJSON Chat where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Chat where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------

data Message = Message 
    { messageMessageId::Integer
    , messageFrom::User
    , messageText::String
    , messageChat::Chat
    } deriving (Generic, Show)

instance ToJSON Message where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Message where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------

data Update = Update 
    { updateUpdateId::Integer
    , updateMessage::Message
    } deriving (Generic, Show)

instance ToJSON Update where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Update where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

-------

data Response respType = Response 
    { responseOk::Bool
    , responseResult::respType
    } deriving (Generic, Show)

instance ToJSON respType => ToJSON (Response respType) where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON respType => FromJSON (Response respType) where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase