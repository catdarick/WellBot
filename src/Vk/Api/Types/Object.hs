{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Object where

import           Data.Aeson                 hiding (Object)
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)
import           Vk.Api.Types.Message

data Object =
  Object
    { objectMessage :: Maybe Message
    }
  deriving (Generic, Show, Eq)

instance FromJSON Object where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
