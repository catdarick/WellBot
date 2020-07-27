{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)

newtype Response respType =
  Response
    { responseResponse :: respType
    }
  deriving (Generic, Show, Eq)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
