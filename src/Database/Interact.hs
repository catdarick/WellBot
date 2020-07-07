{-# LANGUAGE ScopedTypeVariables #-}

module Database.Interact where

import           Config
import           Control.Monad.Trans.State (StateT, get, gets, modify, put)
import           Data.Function             ((&))
import           Data.Map                  (Map, delete, lookup)
import           Data.Map.Strict           (insert)
import           Data.Maybe                (fromMaybe)
import           Database.Types

import           Control.Exception         (SomeException, try)
import           Data.Aeson                (FromJSON, decode)

import           Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Map                  (fromList)
import           Data.Time                 (getCurrentTime)
import           Text.Read                 (readMaybe)

setOffset newOffset = do
  db <- get
  put (db {offset = newOffset})

--isAwaiting :: Monad m => Integer -> StateT (Database a b) m Bool
isAwaiting chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  return $ chatId `elem` awChatsVal

--getRepeatsAmount :: Config -> Integer -> StateT (Database a b) IO Integer
getRepeatsAmount config userOrChatId = do
  chats <- gets chats
  let defaultRepeatsAmount = config & defaultRepeatAmount
  let res = Data.Map.lookup userOrChatId chats
  return $ fromMaybe defaultRepeatsAmount res

--delAwaitingChat :: Integer -> StateT (Database a b) IO ()
delAwaitingChat chatId = do
  db <- get
  let awChatsVal = (db & awaitingChatsID)
  let newChatsVal = [x | x <- awChatsVal, x /= chatId]
  put (db {awaitingChatsID = newChatsVal})

--addAwaitingChat :: Monad m => Integer -> StateT (Database a b) m ()
addAwaitingChat chatId =
  modify
    (\db@Database {awaitingChatsID = xs} -> db {awaitingChatsID = chatId : xs})

--addChat :: Integer -> Integer -> StateT (Database a b) IO ()
addChat chatId repAmount = do
  chatsOld <- gets chats
  setNewChats $ insert chatId repAmount chatsOld

--setNewChats :: Map ChatId RepeatsAmount -> StateT (Database a b) IO ()
setNewChats newChats = do
  db <- get
  put (db {chats = newChats})

--delChat :: Integer -> StateT (Database a b) IO ()
delChat chatId = do
  chatsOld <- gets chats
  setNewChats $ delete chatId chatsOld

--setRepeatsAmount :: Integer -> Integer -> StateT (Database a b) IO ()
setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

updateTime ::
     ( Read offsetType
     , Read additionalInfoType
     , Show offsetType
     , Show additionalInfoType
     )
  => StateT (Database offsetType additionalInfoType) IO ()
updateTime = do
  db <- get
  newTime <- lift $ getCurrentTime
  put (db {prevTime = newTime})

backup path = do
  db <- get
  let strDB = show db
  lift $ writeFile path strDB

getRestoredOrClearDatabase ::
     ( Read offsetType
     , Read additionalInfoType
     , Show offsetType
     , Show additionalInfoType
     )
  => FilePath
  -> offsetType
  -> IO (Database offsetType additionalInfoType)
getRestoredOrClearDatabase path defaultOffset = do
  curTime <- getCurrentTime
  eitherText <- try $ readFile path
  let defaultDB = getInitialDatabase defaultOffset curTime
  either
    (retDefalut defaultDB)
    (retRestoredOrDefaultIfCantParse defaultDB curTime)
    eitherText
  where
    setTime curTime db = db {prevTime = curTime}
    retDefalut (defaultDB :: (Database a b)) (e :: SomeException) =
      return defaultDB
    retRestoredOrDefaultIfCantParse defaultDB curTime text = do
      let maybeDB = (readMaybe text)
      return $ maybe defaultDB (setTime curTime) maybeDB

getInitialDatabase defaultOffset curTime =
  Database
    { offset = defaultOffset
    , chats = fromList []
    , awaitingChatsID = []
    , prevTime = curTime
    , additionalInfo = Nothing
    }
