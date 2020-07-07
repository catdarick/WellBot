module Database.Types where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Map         (Map)
import           Data.Time        (UTCTime)
import           GHC.Generics     (Generic)
import Control.Monad.Trans.State (StateT)

type ChatId = Integer

type RepeatsAmount = Integer

type Chats = Map ChatId RepeatsAmount

data Database offsetType additionalInfoType =
  Database
    { offset          :: offsetType
    , chats           :: Chats
    , awaitingChatsID :: [Integer]
    , prevTime        :: UTCTime
    , additionalInfo  :: Maybe additionalInfoType
    }
  deriving (Read, Show)
