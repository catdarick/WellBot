{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Logic.Helpers where

import           Bot.Classes
import           Bot.State.Database.Types hiding (RepeatsAmount)
import           Bot.State.Types
import           Config
import           Data.Map                 (fromList)
import           Data.Time                (UTCTime (UTCTime), utctDay,
                                           utctDayTime)
import           Logger.Types

type WithAwaiting = Bool

type WithAlreadySet = Bool

defChatOrUserId :: ChatId
defChatOrUserId = 10

defSettedRepAmount :: RepeatsAmount
defSettedRepAmount = 3

defMessageId :: Integer
defMessageId = 20

defOffset :: Integer
defOffset = 100

getStateWithDb ::
     Database offsetType additionalType
  -> BotState_ offsetType additionalType TestBot
getStateWithDb db =
  BotState_
    { prevTime = UTCTime {utctDay = toEnum 0, utctDayTime = 0}
    , bot = TestBot
    , config = defConfig
    , database = db
    , logOffset = 0
    }

getDb :: Integer -> WithAwaiting -> WithAlreadySet -> Database Integer ()
getDb offset withAwaiting withAlreadySet =
  Database
    { offset = offset
    , chats = fromList [(defChatOrUserId, defSettedRepAmount) | withAlreadySet]
    , awaitingChatsID = [defChatOrUserId | withAwaiting]
    , additionalInfo = Nothing
    }

defConfig :: Config
defConfig =
  Config
    { tgToken = ""
    , helpText = "helpText"
    , repeatText = "repeatText "
    , keysAmount = 5
    , defaultRepeatAmount = 1
    , secTimeout = 10
    , diffTimeBackupPeriod = 10
    , vkApiVersion = ""
    , vkToken = ""
    , vkGroupId = 444
    , backupPath = "./"
    , logPath = "./"
    , logSinceLevel = WARNING
    , vkEnabled = False
    , tgEnabled = False
    }

data UpdateTest =
  UpdateTest
    { messageId    :: Integer
    , maybeText    :: Maybe String
    , userOrChatId :: Integer
    }

instance Update UpdateTest where
  getMaybeText = maybeText
  getMessageId = messageId
  getUserOrChatId = userOrChatId

getUpdateWithTextAndOffset :: Maybe String -> UpdateTest
getUpdateWithTextAndOffset maybeText =
  UpdateTest
    { messageId = defMessageId
    , userOrChatId = defChatOrUserId
    , maybeText = maybeText
    }

data TestBot =
  TestBot
  deriving (Show, Eq)

instance Bot TestBot where
  type OffsetType TestBot = Integer
  type UpdateType TestBot = UpdateTest
  type RetType TestBot = String
  name = const "Test"
  defaultOffset = const 0
  sendMessage _ _ userOrChatId text =
    return $ "sendMessage " ++ show userOrChatId ++ " " ++ text
  forwardMessage _ _ userOrChatId messageId =
    return $ "forwardMessage " ++ show userOrChatId ++ " " ++ show messageId
  sendKeyboardWithText _ _ userOrChatId text =
    return $ "sendKeyboardWithText " ++ show userOrChatId ++ " " ++ text
  getUpdatesAndOffset = return ([], 0)
