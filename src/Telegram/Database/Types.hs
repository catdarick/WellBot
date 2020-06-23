{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Telegram.Database.Types where

import Data.Time (UTCTime, DiffTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
data Chat =
  Chat
    { chatID        :: Integer
    , repeatsAmount :: Integer
    }
  deriving (Show, Generic, ToJSON, FromJSON)

-------
data DB =
  DB
    { offset              :: Integer
    , defaultRepeatAmount :: Integer
    , chats               :: [Chat]
    , awaitingChatsID     :: [Integer]
    , loopsCount          :: Integer
    , prevTime            :: UTCTime
    }
  deriving (Show, Generic, ToJSON, FromJSON)
