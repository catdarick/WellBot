{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Vk.Instances where

import qualified Bot.Classes                 as Class
import qualified Bot.State.Database.Interact as DB
import qualified Bot.State.Database.Types    as DB
import qualified Bot.State.Interact          as State
import           Bot.State.Types
import           Config
import           Control.Exception           (SomeException, catch, try)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, gets, modify)
import           Data.Function               ((&))
import           Data.Maybe                  (fromJust, isJust)
import qualified Logger.Interact             as Log
import qualified Vk.Api.Interact             as Api
import qualified Vk.Api.Types                as Api
import           Vk.Types

instance Class.Update Api.Update where
  getMaybeText =
    Api.messageText . fromJust . Api.objecttMessage . Api.updateObject
  getUserOrChatId =
    Api.messageFromId . fromJust . Api.objecttMessage . Api.updateObject
  getMessageId =
    Api.messageId . fromJust . Api.objecttMessage . Api.updateObject

instance Class.Bot VkBot where
  type OffsetType VkBot = String
  type AdditionalType VkBot = Additional
  type UpdateType VkBot = Api.Update
  name = const "VK"
  defaultOffset = const "0"
  sendMessage a = Api.sendMessage
  forwardMessage a = Api.forwardMessage
  sendKeyboardWithText a = Api.sendKeyboardWithText
  initBot = do
    (bot, config) <- State.getBotAndConfig
    updateServerAndTokenAndOffset config
  getUpdatesAndOffset = do
    (bot, config) <- State.getBotAndConfig
    additionalInfo <- gets $ fromJust . DB.additionalInfo . database
    let server_ = additionalInfo & server
    let token_ = additionalInfo & longpollToken
    offset <- DB.getOffset
    maybeUpdatesAndOffset <-
      Log.withErrorLogging $
      Api.getUpdatesAndOffset config server_ offset token_
    case maybeUpdatesAndOffset of
      Just updatesAndOffset -> do
        let filtredUpdates = filter isJustMessage (fst updatesAndOffset)
        return (filtredUpdates, snd updatesAndOffset)
      Nothing -> do
        Log.info "Need to update server/accessToken/offset"
        Log.withDebugLogging "updateServerAndTokenAndOffset" $
          updateServerAndTokenAndOffset config
        return ([], offset)
    where
      isJustMessage = isJust . Api.objecttMessage . Api.updateObject

updateServerAndTokenAndOffset config = do
  eitherMaybeResponse <- lift $ try $ Api.getServerAndTokenAndOffset config
  case eitherMaybeResponse of
    Left (e :: SomeException) -> Log.warn $ show e
    Right maybeResponse ->
      case maybeResponse of
        Just response -> do
          let server = response & Api.responseResponse & Api.longpollServer
          let token = response & Api.responseResponse & Api.longpollKey
          let offset = response & Api.responseResponse & Api.longpollTs
          setServerAndToken server token
          DB.setOffset offset
        Nothing -> Log.warn "Cannot get server and token for LongPolling"
  where
    setServerAndToken server token = do
      db <- gets database
      DB.setDb
        db
          { DB.additionalInfo =
              Just Additional {server = server, longpollToken = token}
          }
