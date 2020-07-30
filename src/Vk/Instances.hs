{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Vk.Instances where

import qualified Bot.Classes                 as Class
import           Bot.ErrorException
import qualified Bot.State.Database.Interact as DB
import qualified Bot.State.Database.Types    as DB
import qualified Bot.State.Interact          as State
import           Bot.State.Types
import           Config
import           Control.Exception           (SomeException, catch, try)
import           Control.Monad.Catch         (MonadThrow (throwM))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT, gets, modify)
import           Data.Function               ((&))
import           Data.Maybe                  (catMaybes, fromMaybe, isJust)
import qualified Logger.Interact             as Log

import qualified Vk.Api.Longpoll.Interact    as Api
import qualified Vk.Api.Longpoll.Types       as Api
import qualified Vk.Api.Methods.Interact     as Api
import qualified Vk.Api.Methods.Types        as Api
import           Vk.Types

instance Class.Message Api.Message where
  getMaybeText = Api.messageText
  getUserOrChatId = Api.messageFromId
  getMessageId = Api.messageId

instance Class.Bot VkBot where
  type OffsetType VkBot = String
  type AdditionalType VkBot = LongpollInfo
  type MessageType VkBot = Api.Message
  name = const "VK"
  defaultOffset = const "0"
  sendMessage a = Api.sendMessage
  forwardMessage a = Api.forwardMessage
  sendKeyboardWithText a = Api.sendKeyboardWithText
  initBot = do
    (bot, config) <- State.getBotAndConfig
    updateServerAndTokenAndOffset config
  getUpdateMessagesAndOffset = do
    (bot, config) <- State.getBotAndConfig
    additionalInfo <-
      gets $ fromMaybe badLongpollInfo . DB.additionalInfo . database
    let server_ = additionalInfo & server
    let token_ = additionalInfo & longpollToken
    offset <- DB.getOffset
    maybeUpdatesAndOffset <-
      Log.withErrorLogging $
      Api.getUpdateMessagesAndOffset config server_ offset token_
    case maybeUpdatesAndOffset of
      Just (updates, offset) -> do
        let maybeMessages = map (Api.objectMessage . Api.updateObject) updates
        let messages = catMaybes maybeMessages
        return (messages, offset)
      Nothing -> do
        Log.info "Need to update server/accessToken/offset"
        Log.withDebugLogging "updateServerAndTokenAndOffset" $
          updateServerAndTokenAndOffset config
        return ([], offset)
    where
      isJustMessage = isJust . Api.objectMessage . Api.updateObject

updateServerAndTokenAndOffset :: Config -> Class.BotStateIO VkBot ()
updateServerAndTokenAndOffset config = do
  eitherMaybeResponse <- lift $ try $ Api.getServerAndTokenAndOffset config
  case eitherMaybeResponse of
    Left (e :: SomeException) -> Log.warn $ show e
    Right maybeResponse ->
      case maybeResponse of
        Just response -> do
          result <- Api.getResultOrThrow response
          let server = result & Api.longpollServer
          let token = result & Api.longpollKey
          let offset = result & Api.longpollTs
          setServerAndToken server token
          DB.setOffset offset
        Nothing -> throwM BadTokenException
  where
    setServerAndToken server token = do
      db <- gets database
      DB.setDb
        db
          { DB.additionalInfo =
              Just LongpollInfo {server = server, longpollToken = token}
          }
