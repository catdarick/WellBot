{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Telegram.Database.Types where

import Data.Time (UTCTime, DiffTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)

type ChatId = Integer
type RepeatsAmount = Integer 

{- data Chat =
  Chat
    { chatID        :: Integer
    , repeatsAmount :: Integer
    }
  deriving (Show, Generic, ToJSON, FromJSON) -}

-------
data DB =
  DB
    { offset              :: Integer
    , defaultRepeatAmount :: Integer
    , chats               :: Map ChatId RepeatsAmount
    , awaitingChatsID     :: [Integer]
    , loopsCount          :: Integer
    , prevTime            :: UTCTime
    }
  deriving (Show, Generic, ToJSON, FromJSON)
