{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Vk.Instances where

import           Class.Bot                 as Class
import qualified Class.Update              as Class
import           Config
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, gets, modify)
import           Data.Function             ((&))
import           Data.Maybe                (fromJust, isJust)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import           ErrorHandler
import qualified Vk.Api.Interact           as Api
import qualified Vk.Api.Types              as Api

data VkBot =
  VkBot

instance Class.Update Api.Update where
  getMaybeText =
    Api.messageText . fromJust . Api.objecttMessage . Api.updateObject
  getUserOrChatId =
    Api.messageFromId . fromJust . Api.objecttMessage . Api.updateObject
  getMessageId =
    Api.messageId . fromJust . Api.objecttMessage . Api.updateObject

data Additional =
  Additional
    { server        :: String
    , longpollToken :: String
    }
  deriving (Show, Read)

instance Class.Bot VkBot where
  type OffsetType VkBot = String
  type AdditionalType VkBot = Additional
  type UpdateType VkBot = Api.Update
  backupName = const "backupVK.dat"
  defaultOffset = const "0"
  sendMessage a = Api.sendMessage
  forwardMessage a = Api.forwardMessage
  sendKeyboardWithText a = Api.sendKeyboardWithText
  initBot a = updateServerAndTokenAndOffset
  getUpdatesAndOffset a config = do
    server <- gets $ server . fromJust . DB.additionalInfo
    token <- gets $ longpollToken . fromJust . DB.additionalInfo
    offset <- gets DB.offset
    maybeUpdatesAndOffset <-
      withErrorPrinting $ Api.getUpdatesAndOffset config server offset token
    case maybeUpdatesAndOffset of
      Just updatesAndOffset -> do
        let filtredUpdates = filter isJustMessage (fst updatesAndOffset)
        return (filtredUpdates, snd updatesAndOffset)
      Nothing -> do
        updateServerAndTokenAndOffset config
        return ([], offset)
    where
      isJustMessage = isJust . Api.objecttMessage . Api.updateObject

updateServerAndTokenAndOffset ::
     Config -> StateT (DB.Database String Additional) IO ()
updateServerAndTokenAndOffset config = do
  eitherMaybeResponse <- lift $ try $ Api.getServerAndTokenAndOffset config
  case eitherMaybeResponse of
    Left (e :: SomeException) -> lift $ print e
    Right maybeResponse ->
      case maybeResponse of
        Just response -> do
          let server = response & Api.responseResponse & Api.longpollServer
          let token = response & Api.responseResponse & Api.longpollKey
          let offset = response & Api.responseResponse & Api.longpollTs
          setServerAndToken server token
          DB.setOffset offset
        Nothing -> lift $ print "Cannot get server and token for LongPolling"
  where
    setServerAndToken server token =
      modify
        (\db ->
           db
             { DB.additionalInfo =
                 Just Additional {server = server, longpollToken = token}
             })
