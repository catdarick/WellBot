{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Telegram.Instances where

import           Class.Bot
import           Class.Update
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets, modify)
import           Data.Function             ((&))
import           Data.Maybe                (fromJust, isJust)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import qualified Telegram.Api              as Telegram
import qualified Telegram.Types            as Telegram

data TgBot =
  TgBot

instance Bot TgBot Telegram.Update where
  type OffsetType TgBot = Integer
  type AdditionalType TgBot = Bool
  backupName = const "backupTG.dat"
  defaultOffset = const 0
  sendMessage a = Telegram.sendMessage
  forwardMessage a = Telegram.forwardMessage
  sendKeyboardWithText a = Telegram.sendKeyboardWithText
  initBot a = const $ return ()
  getUpdatesAndOffset a config = do
    offset <- gets DB.offset
    updates <- lift $ Telegram.getUpdates config offset
    let filtredUpdates = filter isJustMessage updates
    let newOffset = getOffset offset updates
    return (filtredUpdates, newOffset )
    where
      getOffset defaultOffset [] = defaultOffset
      getOffset defaultOffset xs = 1 + (last xs & Telegram.updateUpdateId)
      isJustMessage Telegram.Update {Telegram.updateMessage = maybeMessage} =
        isJust maybeMessage
