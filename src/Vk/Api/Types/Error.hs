{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Types.Error where

import           Data.Aeson                 hiding (Object)
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)

data Error =
  Error
    { errorFailed :: Integer
    }
  deriving (Generic, Show, Eq)

instance FromJSON Error where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase