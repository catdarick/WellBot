{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Longpoll.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing           (aesonPrefix)
import           Data.Aeson.Casing.Internal  (snakeCase)
import           GHC.Generics                (Generic)
import           Vk.Api.Longpoll.Types.Error

data Response respType =
  Response
    { responseResponse :: Maybe respType
    , responseError    :: Maybe Error
    }
  deriving (Generic, Show, Eq)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
