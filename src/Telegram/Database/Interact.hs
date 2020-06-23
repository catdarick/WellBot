{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Database.Interact where

import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, get, modify, put)
import           Data.Aeson                (decode, encode)
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Function             ((&))
import           Data.List                 (find)
import           Data.Time                 (getCurrentTime, getTime_resolution)
import           Telegram.Database.Types

setOffset newOffset = do
  db <- get
  put (db {offset = newOffset})

addAwaitingChat chatId = do
  modify (\db@DB {awaitingChatsID = xs} -> db {awaitingChatsID = chatId : xs})

isAwaiting chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  return $ chatId `elem` awChatsVal

delAwaitingChat chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let newChatsVal = [x | x <- awChatsVal, x /= chatId]
  put (db {awaitingChatsID = newChatsVal})

addChat chatId repAmount = do
  let newChat = Chat {chatID = chatId, repeatsAmount = repAmount}
  modify (addNewChat newChat)
  where
    addNewChat newChat (db@DB {chats = chatsVal}) =
      db {chats = newChat : chatsVal}

delChat chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  let newChatsVal = [x | x <- chatsVal, (x & chatID) /= chatId]
  put (db {chats = newChatsVal})

getChatById chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  return $ find (\Chat {chatID = chatIdVal} -> chatIdVal == chatId) chatsVal

setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

getRepeatsAmount :: Integer -> StateT DB IO Integer
getRepeatsAmount chatId = do
  db <- get
  res <- getChatById chatId
  lift $ print res
  let defaultRepeatsAmount = (db & defaultRepeatAmount)
  return $ maybe defaultRepeatsAmount getRepeatsAmountFromChat res
  where
    getRepeatsAmountFromChat (Chat {repeatsAmount = repAmaount}) = repAmaount

getLoopsCount :: StateT DB IO Integer
getLoopsCount = do
  db <- get
  return (db & loopsCount)

updateTime = do
  db <- get
  newTime <- lift $ getCurrentTime
  put (db {prevTime = newTime})

backup path = do
  db <- get
  let bsDB = encode db
  lift $ DataByteString.writeFile path bsDB

getRestoredOrNewDatabase defaultRepeatsAmount = do
  curTime <- getCurrentTime
  eitherData <- try $ DataByteString.readFile "./backup.dat"
  let defaultDB = getInitialDatabase defaultRepeatsAmount curTime
  either
    (retDefalut defaultDB)
    (retRestoredOrDefaultIfCantParse defaultDB curTime)
    eitherData
  where
    setRepAmountAndTime defaultRepeatsAmount curTime db =
      db {defaultRepeatAmount = defaultRepeatsAmount, prevTime = curTime}
    retDefalut defaultDB (e :: SomeException) = do
      return (defaultDB :: DB)
    retRestoredOrDefaultIfCantParse defaultDB curTime bsData = do
      let maybeDB = (decode bsData) :: Maybe DB
      return $
        maybe
          defaultDB
          (setRepAmountAndTime defaultRepeatsAmount curTime)
          maybeDB

getInitialDatabase defaultRepeatsAmount time =
  (DB
     { offset = 0
     , defaultRepeatAmount = defaultRepeatsAmount
     , chats = []
     , awaitingChatsID = []
     , loopsCount = 0
     , prevTime = time
     })
