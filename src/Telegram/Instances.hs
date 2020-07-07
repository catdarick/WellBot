{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Telegram.Instances where

import qualified Class.Bot                 as Class
import qualified Class.Update              as Class
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets, modify)
import           Data.Function             ((&))
import           Data.Maybe                (fromJust, isJust)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import           ErrorHandler
import qualified Telegram.Api              as TG
import qualified Telegram.Types            as TG

data TgBot =
  TgBot

instance Class.Update TG.Update where
  getMaybeText = TG.messageText . fromJust . TG.updateMessage
  getUserOrChatId = TG.chatId . TG.messageChat . fromJust . TG.updateMessage
  getMessageId = TG.messageMessageId . fromJust . TG.updateMessage

instance Class.Bot TgBot TG.Update where
  type OffsetType TgBot = Integer
  type AdditionalType TgBot = Bool
  backupName = const "backupTG.dat"
  defaultOffset = const 0
  sendMessage a = TG.sendMessage
  forwardMessage a = TG.forwardMessage
  sendKeyboardWithText a = TG.sendKeyboardWithText
  initBot a = const $ return ()
  getUpdatesAndOffset a config = do
    offset <- gets DB.offset
    updates <- withErrorPrinting $ TG.getUpdates config offset
    let filtredUpdates = filter isJustMessage updates
    let newOffset = getOffset offset updates
    return (filtredUpdates, newOffset)
    where
      getOffset defaultOffset [] = defaultOffset
      getOffset defaultOffset xs = 1 + (last xs & TG.updateUpdateId)
      isJustMessage TG.Update {TG.updateMessage = maybeMessage} =
        isJust maybeMessage
