module Bot.State.Types where

import           Bot.State.Database.Types
import           Config
import           Data.Time                (UTCTime)

type UserOrChatId = Integer

type MesssageId = Integer

type RepeatsAmount = Integer

data BotState_ offsetType additionalType botType =
  BotState_
    { config    :: Config
    , bot       :: botType
    , logOffset :: Int
    , prevTime  :: UTCTime
    , database  :: Database offsetType additionalType
    } deriving (Eq, Show)
