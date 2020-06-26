{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Telegram.Database.Types where

import Data.Time (UTCTime, DiffTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)

type ChatId = Integer
type RepeatsAmount = Integer 
type Chats = Map ChatId RepeatsAmount
-------
data DB =
  DB
    { offset              :: Integer
    , defaultRepeatAmount :: Integer
    , chats               :: Chats
    , awaitingChatsID     :: [Integer]
    , prevTime            :: UTCTime
    }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)
