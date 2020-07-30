{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Bot.Classes where

import           Bot.State.Database.Types
import           Bot.State.Types
import           Bot.Synonyms
import           Config
import           Control.Monad             (replicateM, replicateM_, void)
import           Control.Monad.Trans.State (StateT)

type ReadShow a b = (Read a, Show a, Read b, Show b)

type BotStateT a = (StateT (BotState (OffsetType a) (AdditionalType a) a))

type BotStateIO a b = BotStateT a IO b

class Message a where
  getMaybeText :: a -> Maybe String
  getUserOrChatId :: a -> Integer
  getMessageId :: a -> Integer

class ( Message (MessageType a)
      , Monoid (RetType a)
      , ReadShow (OffsetType a) (AdditionalType a)
      , Eq (OffsetType a)
      ) =>
      Bot a
  where
  type OffsetType a
  type MessageType a
  type AdditionalType a
  type AdditionalType a = ()
  type RetType a
  type RetType a = ()
  name :: a -> String
  defaultOffset :: a -> OffsetType a
  sendMessage :: a -> Config -> UserOrChatId -> String -> IO (RetType a)
  forwardMessage :: a -> Config -> UserOrChatId -> MesssageId -> IO (RetType a)
  sendKeyboardWithText ::
       a -> Config -> UserOrChatId -> String -> IO (RetType a)
  getUpdatesAndOffset :: BotStateIO a ([MessageType a], OffsetType a)
  initBot :: BotStateIO a ()
  initBot = return ()
  forwardMessageNTimes ::
       a -> Config -> UserOrChatId -> MesssageId -> Integer -> IO (RetType a)
  forwardMessageNTimes bot config userOrChatId messageId n = do
    res <-
      replicateM
        (fromInteger n)
        (forwardMessage bot config userOrChatId messageId)
    return $ mconcat res
