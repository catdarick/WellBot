module Bot.State.Database.Types where

import           Bot.Synonyms
import           Control.Monad.Trans.State (StateT)
import           Data.Aeson.Types          (FromJSON, ToJSON)
import           Data.Map                  (Map)
import           Data.Time                 (UTCTime)
import           GHC.Generics              (Generic)

type Chats = Map UserOrChatId RepeatsAmount

data Database offsetType additionalInfoType =
  Database
    { offset          :: offsetType
    , chats           :: Chats
    , awaitingChatsID :: [Integer]
    , additionalInfo  :: Maybe additionalInfoType
    }
  deriving (Read, Show, Eq)
