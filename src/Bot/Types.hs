module Bot.Types where

import           Config
import           Database.Types

type UserOrChatId = Integer

type MesssageId = Integer

type RepeatsAmount = Integer

data BotState_ offsetType additionalType botType =
  BotState_
    { config   :: Config
    , bot :: botType
    , logOffset :: Int
    , database :: Database offsetType additionalType
    }
