{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Class.Bot where

import           Class.Update
import           Config
import           Control.Monad             (replicateM, replicateM_, void)
import           Control.Monad.Trans.State (StateT)
import           Database.Types

type UserOrChatId = Integer

type MesssageId = Integer

type RepeatsAmount = Integer

type ReadShow a b = (Read a, Show a, Read b, Show b)

type BotFriendly a = (Bot a, ReadShow (OffsetType a) (AdditionalType a))

type BotState a  = StateT (Database (OffsetType a) (AdditionalType a)) 

class ( Update (UpdateType a)
      , Monoid (ReturningType a)
      , ReadShow (OffsetType a) (AdditionalType a)
      ) =>
      Bot a 
  where
  type OffsetType a
  type UpdateType a
  type AdditionalType a
  type AdditionalType a = ()
  type ReturningType a
  type ReturningType a = ()
  backupName :: a -> String
  defaultOffset :: a -> OffsetType a
  sendMessage :: a -> Config -> UserOrChatId -> String -> IO (ReturningType a)
  forwardMessage ::
       a -> Config -> UserOrChatId -> MesssageId -> IO (ReturningType a)
  sendKeyboardWithText ::
       a -> Config -> UserOrChatId -> String -> IO (ReturningType a)
  getUpdatesAndOffset ::
       a -> Config -> BotState a IO ([UpdateType a], OffsetType a)
  initBot :: a -> Config -> BotState a IO ()
  initBot _ _ = return ()
  forwardMessageNTimes ::
       a
    -> Config
    -> UserOrChatId
    -> MesssageId
    -> Integer
    -> IO (ReturningType a)
  forwardMessageNTimes bot config userOrChatId messageId n = do
    res <-
      replicateM
        (fromInteger n)
        (forwardMessage bot config userOrChatId messageId)
    return $ mconcat res
