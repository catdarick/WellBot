{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Vk.Instances where

import           Class.Bot
import           Class.Update
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets, modify)
import           Data.Function             ((&))
import           Data.Maybe                (fromJust, isJust)
import qualified Database.Interact         as DB
import qualified Database.Types            as DB
import           ErrorHandler
import qualified Vk.Api                    as Vk
import qualified Vk.Types                  as Vk

data VkBot =
  VkBot

data Additional =
  Additional
    { server        :: String
    , longpollToken :: String
    }
  deriving (Show, Read)

instance Bot VkBot Vk.Update where
  type OffsetType VkBot = String
  type AdditionalType VkBot = Additional
  backupName = const "backupVK.dat"
  defaultOffset = const "0"
  sendMessage a = Vk.sendMessage
  forwardMessage a = Vk.forwardMessage
  sendKeyboardWithText a = Vk.sendKeyboardWithText
  initBot a = updateServerAndTokenAndOffset
  getUpdatesAndOffset a config = do
    server <- gets $ server . fromJust . DB.additionalInfo
    token <- gets $ longpollToken . fromJust . DB.additionalInfo
    offset <- gets DB.offset
    maybeUpdatesAndOffset <-
      withErrorPrinting $ Vk.getUpdatesAndOffset config server offset token
    case maybeUpdatesAndOffset of
      Just updatesAndOffset -> do
        let filtredUpdates = filter isJustMessage (fst updatesAndOffset)
        return (filtredUpdates, snd updatesAndOffset)
      Nothing -> do
        updateServerAndTokenAndOffset config
        return ([], offset)
    where
      isJustMessage = isJust . Vk.objecttMessage . Vk.updateObject

updateServerAndTokenAndOffset config = do
  eitherMaybeResponse <- lift $ try $ Vk.getServerAndTokenAndOffset config
  case eitherMaybeResponse of
    Left (e :: SomeException) -> lift $ print e
    Right maybeResponse ->
      case maybeResponse of
        Just response -> do
          let server = response & Vk.responseResponse & Vk.longpollServer
          let token = response & Vk.responseResponse & Vk.longpollKey
          let offset = response & Vk.responseResponse & Vk.longpollTs
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
