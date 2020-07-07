{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Class.Bot where

import           Class.Update
import           Config
import           Control.Monad.Trans.State (StateT)
import           Database.Types

type UserOrChatId = Integer

type MesssageId = Integer

type RepeatsAmount = Integer

type ReadShow a b = (Read a, Show a, Read b, Show b)

type BotFriendly a u = (Bot a u, ReadShow (OffsetType a) (AdditionalType a))

type BotState a m = StateT (Database (OffsetType a) (AdditionalType a)) m

class Update u =>
      Bot a u
  | a -> u
  where
  type OffsetType a
  type AdditionalType a
  backupName :: a -> String
  defaultOffset :: a -> OffsetType a
  sendMessage :: a -> Config -> UserOrChatId -> String -> IO ()
  forwardMessage :: a -> Config -> UserOrChatId -> MesssageId -> IO ()
  sendKeyboardWithText :: a -> Config -> UserOrChatId -> String -> IO ()
  getUpdatesAndOffset :: a -> Config -> BotState a IO ([u], OffsetType a)
  initBot :: a -> Config -> BotState a IO ()
