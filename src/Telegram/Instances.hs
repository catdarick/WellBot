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
import           Data.Maybe                  (catMaybes, isJust)
import qualified Logger.Interact             as Log
import qualified Telegram.Api.Interact       as Api
import qualified Telegram.Api.Types          as Api

data TgBot =
  TgBot

instance Class.Message Api.Message where
  getMaybeText = Api.messageText 
  getUserOrChatId = Api.chatId . Api.messageChat 
  getMessageId = Api.messageMessageId 

instance Class.Bot TgBot where
  type OffsetType TgBot = Integer
  type MessageType TgBot = Api.Message
  name = const "TG"
  defaultOffset = const 0
  sendMessage a = Api.sendMessage
  forwardMessage a = Api.forwardMessage
  sendKeyboardWithText a = Api.sendKeyboardWithText
  getUpdatesAndOffset = do
    (bot, config) <- State.getBotAndConfig
    offset <- DB.getOffset
    updates <- lift $ Api.getUpdates config offset
    let maybeMessages = map Api.updateMessage updates
    let messages = catMaybes maybeMessages
    let newOffset = getOffset offset updates
    return (messages, newOffset)
    where
      getOffset defaultOffset [] = defaultOffset
      getOffset defaultOffset xs = 1 + (last xs & Api.updateUpdateId)
      isJustMessage  =
        isJust . Api.updateMessage
