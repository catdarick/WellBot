module Bot.State.Types where

import           Bot.State.Database.Types
import           Config
import           Data.Time                (UTCTime)

type UserOrChatId = Integer

type MesssageId = Integer

type RepeatsAmount = Integer

data BotState offsetType additionalType botType =
  BotState
    { config    :: Config
    , bot       :: botType
    , logOffset :: Int
    , prevTime  :: UTCTime
    , database  :: Database offsetType additionalType
    } deriving (Eq, Show)
