{-# LANGUAGE OverloadedStrings #-}

module Telegram.RuntimeDB where

import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (get, modify, put)
import           Data.Configurator         as Configurator
import           Data.Configurator.Types   as Configurator.Types
import           Data.Function             ((&))
import           Data.List                 (find)

data Chat =
  Chat
    { chatID        :: Integer
    , repeatsAmount :: Integer
    }
  deriving (Show)

-------
data DB =
  DB
    { offset              :: Integer
    , defaultRepeatAmount :: Integer
    , chats               :: [Chat]
    , awaitingChatsID     :: [Integer]
    }
  deriving (Show)

-------------------------------------------------------
setOffset newOffset = do
  db <- get
  put (db {offset = newOffset})

addAwaitingChat chatId = do
  modify (\db@DB {awaitingChatsID = xs} -> db {awaitingChatsID = chatId : xs})

isAwaiting chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let res = find ((==) chatId) awChatsVal
  case res of
    Nothing -> return False
    Just _  -> return True

delAwaitingChat chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let newChatsVal = [x | x <- awChatsVal, x /= chatId]
  put (db {awaitingChatsID = newChatsVal})

addChat chatId repAmount = do
  let newChat = Chat {chatID = chatId, repeatsAmount = repAmount}
  modify (\db@DB {chats = chatsVal} -> db {chats = newChat : chatsVal})

delChat chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  let newChatsVal = [x | x <- chatsVal, (x & chatID) /= chatId]
  put (db {chats = newChatsVal})

getChatById chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  return (find (\Chat {chatID = chatIdVal} -> chatIdVal == chatId) chatsVal)

setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

getRepeatsAmount chatId = do
  db <- get
  res <- getChatById chatId
  lift $ print res
  case res of
    Nothing -> return (db & defaultRepeatAmount)
    Just Chat {repeatsAmount = repAmaount} -> return repAmaount
