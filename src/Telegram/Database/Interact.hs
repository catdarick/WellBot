{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Database.Interact where

import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, get, modify, put)
import           Data.Aeson                (ToJSON, decode, encode)
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Function             ((&))
import           Data.List                 (find)
import           Data.Time                 (UTCTime, getCurrentTime, getTime_resolution)
import           Telegram.Database.Types

setOffset :: Monad m => Integer -> StateT DB m ()
setOffset newOffset = do
  db <- get
  put (db {offset = newOffset})

addAwaitingChat :: Monad m => Integer -> StateT DB m ()
addAwaitingChat chatId = do
  modify (\db@DB {awaitingChatsID = xs} -> db {awaitingChatsID = chatId : xs})

isAwaiting :: Monad m => Integer -> StateT DB m Bool
isAwaiting chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  return $ chatId `elem` awChatsVal

delAwaitingChat :: Monad m => Integer -> StateT DB m ()
delAwaitingChat chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let newChatsVal = [x | x <- awChatsVal, x /= chatId]
  put (db {awaitingChatsID = newChatsVal})

addChat :: Monad m => Integer -> Integer -> StateT DB m ()
addChat chatId repAmount = do
  let newChat = Chat {chatID = chatId, repeatsAmount = repAmount}
  modify (addNewChat newChat)
  where
    addNewChat newChat (db@DB {chats = chatsVal}) =
      db {chats = newChat : chatsVal}

delChat :: Monad m => Integer -> StateT DB m ()
delChat chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  let newChatsVal = [x | x <- chatsVal, (x & chatID) /= chatId]
  put (db {chats = newChatsVal})

getChatById :: Monad m => Integer -> StateT DB m (Maybe Chat)
getChatById chatId = do
  db <- get
  let chatsVal = (db & chats) :: [Chat]
  return $ find (\Chat {chatID = chatIdVal} -> chatIdVal == chatId) chatsVal

setRepeatsAmount :: Monad m => Integer -> Integer -> StateT DB m ()
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

updateTime :: StateT DB IO ()
updateTime = do
  db <- get
  newTime <- lift $ getCurrentTime
  put (db {prevTime = newTime})

backup :: ToJSON a => FilePath -> StateT a IO ()
backup path = do
  db <- get
  let bsDB = encode db
  lift $ DataByteString.writeFile path bsDB

getRestoredOrNewDatabase :: Integer -> IO DB
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

getInitialDatabase :: Integer -> UTCTime -> DB
getInitialDatabase defaultRepeatsAmount time =
  (DB
     { offset = 0
     , defaultRepeatAmount = defaultRepeatsAmount
     , chats = []
     , awaitingChatsID = []
     , loopsCount = 0
     , prevTime = time
     })
