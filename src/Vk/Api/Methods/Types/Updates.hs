{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Methods.Types.Updates where

import           Data.Aeson
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)
import           Vk.Api.Methods.Types.Update

data Updates =
  Updates
    { updatesTs      :: String
    , updatesUpdates :: [Update]
    }
  deriving (Generic, Show, Eq)

instance FromJSON Updates where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
