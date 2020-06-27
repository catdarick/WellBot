module Telegram.TestHelpers where

import           Data.Map                (fromList)
import           Data.Time               (UTCTime (UTCTime), utctDay,
                                          utctDayTime)
import           Config
import qualified Telegram.Database.Types as DB
import           Telegram.Interact
import           Telegram.Types

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
    }

getDb withAwaiting withAlreadySettedRepeats repeatsAmount offset =
  DB.DB
    { DB.offset = offset
    , DB.defaultRepeatAmount = 1
    , DB.chats = fromList chats
    , DB.awaitingChatsID = awaitingChats
    , DB.prevTime = UTCTime {utctDay = toEnum 0, utctDayTime = 0}
    }
  where
    awaitingChats = [123 | withAwaiting]
    chats = [(123, repeatsAmount) | withAlreadySettedRepeats]

chatId_ = 123

userId_ = 111

updateId_ = 10

messageId_ = 1337

getUpdateWithMessage text =
  Update
    { updateUpdateId = updateId_
    , updateMessage =
        Just
          (Message
             { messageMessageId = messageId_
             , messageFrom = User {userId = userId_}
             , messageText = Just text
             , messageChat = Chat {chatId = chatId_}
             })
    }
