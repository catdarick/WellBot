{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Update where

import           Data.Aeson                 hiding (Object)
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)
import           Vk.Api.Types.Object

data Update =
  Update
    { updateType   :: String
    , updateObject :: Object
    }
  deriving (Generic, Show, Eq)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
