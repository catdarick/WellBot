{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Logic.Helpers where

import           Class.Bot
import           Class.Update
import           Config
import           Data.Map       (fromList)
import           Data.Time      (UTCTime (UTCTime), utctDay, utctDayTime)
import           Database.Types
type WithAwaiting = Bool
type WithAlreadySet = Bool
defChatOrUserId = 10

defSettedRepAmount = 3

defMessageId = 20

defOffset = 100

getDb :: Integer -> WithAwaiting -> WithAlreadySet -> Database Integer ()
getDb offset withAwaiting withAlreadySet =
  Database
    { offset = offset
    , chats = fromList [(defChatOrUserId, defSettedRepAmount) | withAlreadySet]
    , awaitingChatsID = [defChatOrUserId | withAwaiting]
    , prevTime = UTCTime {utctDay = toEnum 0, utctDayTime = 0}
    , additionalInfo = Nothing
    }

config :: Config
config =
  Config
    { tgToken = ""
    , helpText = "helpText"
    , repeatText = "repeatText "
    , sadText = ""
    , keysAmount = 5
    , defaultRepeatAmount = 1
    , secTimeout = 10
    , diffTimeBackupPeriod = 10
    , vkApiVersion = ""
    , vkToken = ""
    , vkGroupId = 444
    , backupPath = "./"
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

instance Bot TestBot where
  type OffsetType TestBot = Integer
  type UpdateType TestBot = UpdateTest
  type ReturningType TestBot = String
  backupName = const ""
  defaultOffset = const 0
  sendMessage _ _ userOrChatId text =
    return $ "sendMessage " ++ show userOrChatId ++ " " ++ text
  forwardMessage _ _ userOrChatId messageId =
    return $ "forwardMessage " ++ show userOrChatId ++ " " ++ show messageId
  sendKeyboardWithText _ _ userOrChatId text =
    return $ "sendKeyboardWithText " ++ show userOrChatId ++ " " ++ text
  getUpdatesAndOffset _ _ = return ([], 0)
