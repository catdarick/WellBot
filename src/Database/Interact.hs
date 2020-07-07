{-# LANGUAGE ScopedTypeVariables #-}

module Database.Interact where

import           Class.Bot                 (ReadShow)
import           Config
import           Control.Exception         (SomeException, try)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, get, gets, modify, put)
import           Data.Aeson                (FromJSON, decode)
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Function             ((&))
import           Data.Map                  (Map, delete, fromList, lookup)
import           Data.Map.Strict           (insert)
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (UTCTime, getCurrentTime)
import           Database.Types
import           Text.Read                 (readMaybe)

type BotState a b = StateT (Database a b)

setOffset ::
     Monad m => offsetType -> BotState offsetType additionalInfoType m ()
setOffset newOffset = do
  db <- get
  put (db {offset = newOffset})

isAwaiting ::
     Monad m => Integer -> BotState offsetType additionalInfoType m Bool
isAwaiting chatId = do
  db <- get
  let awChatsVal = db & awaitingChatsID
  return $ chatId `elem` awChatsVal

getRepeatsAmount ::
     Monad m
  => Config
  -> Integer
  -> BotState offsetType additionalInfoType m Integer
getRepeatsAmount config userOrChatId = do
  chats <- gets chats
  let defaultRepeatsAmount = config & defaultRepeatAmount
  let res = Data.Map.lookup userOrChatId chats
  return $ fromMaybe defaultRepeatsAmount res

delAwaitingChat ::
     Monad m => Integer -> BotState offsetType additionalInfoType m ()
delAwaitingChat chatId = do
  db <- get
  let awChatsVal = db & awaitingChatsID
  let newChatsVal = filter (chatId /=) awChatsVal
  put (db {awaitingChatsID = newChatsVal})

addAwaitingChat ::
     Monad m => Integer -> BotState offsetType additionalInfoType m ()
addAwaitingChat chatId =
  modify
    (\db@Database {awaitingChatsID = xs} -> db {awaitingChatsID = chatId : xs})

addChat ::
     Monad m
  => ChatId
  -> RepeatsAmount
  -> BotState offsetType additionalInfoType m ()
addChat chatId repAmount = do
  chatsOld <- gets chats
  setNewChats $ insert chatId repAmount chatsOld

setNewChats :: Monad m => Chats -> BotState offsetType additionalInfoType m ()
setNewChats newChats = do
  db <- get
  put (db {chats = newChats})

delChat :: Monad m => ChatId -> BotState offsetType additionalInfoType m ()
delChat chatId = do
  chatsOld <- gets chats
  setNewChats $ delete chatId chatsOld

setRepeatsAmount ::
     Monad m
  => ChatId
  -> RepeatsAmount
  -> BotState offsetType additionalInfoType m ()
setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

updateTime :: StateT (Database a b) IO ()
updateTime = do
  db <- get
  newTime <- lift getCurrentTime
  put (db {prevTime = newTime})

backup :: Show a => FilePath -> StateT a IO ()
backup path = do
  db <- get
  let strDB = show db
  lift $ writeFile path strDB

getRestoredOrClearDatabase ::
     ReadShow offsetType additionalInfoType
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
      let maybeDB = readMaybe text
      return $ maybe defaultDB (setTime curTime) maybeDB

getInitialDatabase ::
     offsetType -> UTCTime -> Database offsetType additionalInfoType
getInitialDatabase defaultOffset curTime =
  Database
    { offset = defaultOffset
    , chats = fromList []
    , awaitingChatsID = []
    , prevTime = curTime
    , additionalInfo = Nothing
    }
