{-# LANGUAGE DeriveGeneric #-}

module Telegram.Api.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Maybe        (fromJust)
import           GHC.Generics      (Generic)


data Response respType =
  Response
    { responseOk          :: Bool
    , responseResult      :: Maybe respType
    , responseErrorCode   :: Maybe Int
    , responseDescription :: Maybe String
    }
  deriving (Generic, Show)

instance FromJSON respType => FromJSON (Response respType) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase