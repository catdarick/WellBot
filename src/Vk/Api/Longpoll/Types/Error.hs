{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Longpoll.Types.Error where

import           Data.Aeson                 hiding (Object)
import           Data.Aeson.Casing          (aesonPrefix)
import           Data.Aeson.Casing.Internal (snakeCase)
import           GHC.Generics               (Generic)

data Error =
  Error
    { errorErrorCode :: Integer
    , errorErrorMsg  :: String
    }
  deriving (Generic, Show, Eq)

instance FromJSON Error where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
