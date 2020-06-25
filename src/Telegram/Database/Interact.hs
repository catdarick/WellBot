{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram.Database.Interact where

import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, get, modify, put)
import           Data.Aeson                (ToJSON, decode, encode)
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Function             ((&))
import           Data.List                 (find)
import           Data.Map                  (Map, adjust, empty, insert)
import           Data.Map                  (delete, lookup)
import           Data.Time                 (UTCTime, getCurrentTime,
                                            getTime_resolution)
import           Telegram.Database.Types
import Data.Maybe (fromMaybe)

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

getFromDatabase :: (a -> b) -> StateT a IO b
getFromDatabase field = do
  db <- get
  return $ db & field

setNewChats :: Map ChatId RepeatsAmount -> StateT DB IO ()
setNewChats newChats = do
  db <- get
  put (db {chats = newChats})

delAwaitingChat :: Integer -> StateT DB IO ()
delAwaitingChat chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let newChatsVal = [x | x <- awChatsVal, x /= chatId]
  put (db {awaitingChatsID = newChatsVal})

addChat :: Integer -> Integer -> StateT DB IO ()
addChat chatId repAmount = do
  chatsOld <- getFromDatabase chats
  setNewChats $ insert chatId repAmount chatsOld

delChat :: Integer -> StateT DB IO ()
delChat chatId = do
  chatsOld <- getFromDatabase chats
  setNewChats $ delete chatId chatsOld

setRepeatsAmount :: Integer -> Integer -> StateT DB IO ()
setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

getRepeatsAmount :: Integer -> StateT DB IO Integer
getRepeatsAmount chatId = do
  chats <- getFromDatabase chats
  defaultRepeatsAmount <- getFromDatabase defaultRepeatAmount
  let res = Data.Map.lookup chatId chats
  return $ fromMaybe defaultRepeatsAmount res

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
    retDefalut defaultDB (e :: SomeException) = return (defaultDB :: DB)
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
     , chats = empty
     , awaitingChatsID = []
     , prevTime = time
     })
