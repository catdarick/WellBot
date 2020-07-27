{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Longpoll where

import           Data.Aeson
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)

data Longpoll =
  Longpoll
    { longpollKey    :: String
    , longpollServer :: String
    , longpollTs     :: String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Longpoll where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
