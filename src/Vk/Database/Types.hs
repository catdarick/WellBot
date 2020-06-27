{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Vk.Database.Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Map     (Map)
import           Data.Time    (DiffTime, UTCTime)
import           GHC.Generics (Generic)

type ChatId = Integer

type RepeatsAmount = Integer

type Chats = Map ChatId RepeatsAmount

-------
data DB =
  DB
    { offset              :: String
    , defaultRepeatAmount :: Integer
    , chats               :: Chats
    , awaitingChatsID     :: [Integer]
    , prevTime            :: UTCTime
    , accessToken         :: String
    , server              :: String
    }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)
