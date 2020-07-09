{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Telegram.Instances where

import qualified Bot.Classes                 as Class
import qualified Bot.State.Database.Interact as DB
import qualified Bot.State.Database.Types    as DB
import qualified Bot.State.Interact          as State
import           Control.Exception           (SomeException, try)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (gets, modify)
import           Data.Function               ((&))
import           Data.Maybe                  (fromJust, isJust)
import qualified Logger.Interact             as Log
import qualified Telegram.Api.Interact       as Api
import qualified Telegram.Api.Types          as Api

data TgBot =
  TgBot

instance Class.Update Api.Update where
  getMaybeText = Api.messageText . fromJust . Api.updateMessage
  getUserOrChatId = Api.chatId . Api.messageChat . fromJust . Api.updateMessage
  getMessageId = Api.messageMessageId . fromJust . Api.updateMessage

instance Class.Bot TgBot where
  type OffsetType TgBot = Integer
  type UpdateType TgBot = Api.Update
  name = const "TG"
  defaultOffset = const 0
  sendMessage a = Api.sendMessage
  forwardMessage a = Api.forwardMessage
  sendKeyboardWithText a = Api.sendKeyboardWithText
  getUpdatesAndOffset = do
    (bot, config) <- State.getBotAndConfig
    offset <- DB.getOffset
    updates <- Log.withErrorLogging $ Api.getUpdates config offset
    let filtredUpdates = filter isJustMessage updates
    let newOffset = getOffset offset updates
    return (filtredUpdates, newOffset)
    where
      getOffset defaultOffset [] = defaultOffset
      getOffset defaultOffset xs = 1 + (last xs & Api.updateUpdateId)
      isJustMessage Api.Update {Api.updateMessage = maybeMessage} =
        isJust maybeMessage
