module Vk.TestHelpers where

import           Config
import           Data.Map          (fromList)
import           Data.Time         (UTCTime (UTCTime), utctDay, utctDayTime)
import qualified Vk.Database.Types as DB
import           Vk.Interact
import           Vk.Types

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
    }

getDb withAwaiting withAlreadySettedRepeats repeatsAmount  =
  DB.DB
    { DB.offset = "3"
    , DB.defaultRepeatAmount = 1
    , DB.chats = fromList chats
    , DB.awaitingChatsID = awaitingChats
    , DB.prevTime = UTCTime {utctDay = toEnum 0, utctDayTime = 0}
    , DB.server = ""
    , DB.accessToken = ""
    }
  where
    awaitingChats = [userId_ | withAwaiting]
    chats = [(userId_, repeatsAmount) | withAlreadySettedRepeats]



userId_ = 111

updateId_ = 10

messageId_ = 1337

getUpdateWithMessage text =
  Update
    { updateType = "message_new"
    , updateObject =
        Objectt
          { objecttMessage =
              Just
                (Message
                   { messageFromId = userId_
                   , messageId = messageId_
                   , messageText = Just text
                   })
          }
    }
