{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Longpoll.Types.Container where

import           Data.Aeson
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)

data Container =
  Container
    { longpollKey    :: String
    , longpollServer :: String
    , longpollTs     :: String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Container where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
