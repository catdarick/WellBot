{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Interact where

import           Bot.Types                 (BotState_, config, database, bot)
import           Class.Bot                 (name, Bot, ReadShow)
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

type BotState a b t= StateT (BotState_ a b t)

setDb :: Monad m => Database a b -> (BotState a b t )m ()
setDb db = do
  state <- get
  put $ state {database = db}

setOffset :: Monad m => offsetType -> (BotState offsetType b t) m ()
setOffset newOffset = do
  db <- gets database
  setDb $ db {offset = newOffset}

getOffset :: Monad m => (BotState a b t) m a
getOffset = gets $ offset . database

getPrevTime :: Monad m => (BotState a b t) m UTCTime
getPrevTime = gets $ prevTime . database

isAwaiting :: Monad m => Integer -> (BotState a b t) m Bool
isAwaiting chatId = do
  db <- gets database
  let awChatsVal = db & awaitingChatsID
  return $ chatId `elem` awChatsVal

getRepeatsAmount :: Monad m => Config -> Integer -> (BotState a b t) m Integer
getRepeatsAmount config userOrChatId = do
  chats <- gets $chats . database
  let defaultRepeatsAmount = config & defaultRepeatAmount
  let res = Data.Map.lookup userOrChatId chats
  return $ fromMaybe defaultRepeatsAmount res

delAwaitingChat :: Monad m => Integer -> (BotState a b t) m ()
delAwaitingChat chatId = do
  db <- gets database
  let awChatsVal = db & awaitingChatsID
  let newChatsVal = filter (chatId /=) awChatsVal
  setDb (db {awaitingChatsID = newChatsVal})

addAwaitingChat :: Monad m => Integer -> (BotState a b t) m ()
addAwaitingChat chatId = do
  db <- gets database
  let awaitingChats = db & awaitingChatsID
  setDb db {awaitingChatsID = chatId : awaitingChats}

addChat :: Monad m => ChatId -> RepeatsAmount -> (BotState a b t) m ()
addChat chatId repAmount = do
  chatsOld <- gets $ chats . database
  setNewChats $ insert chatId repAmount chatsOld

setNewChats :: Monad m => Chats -> (BotState a b t) m ()
setNewChats newChats = do
  db <- gets database
  setDb (db {chats = newChats})

delChat :: Monad m => ChatId -> (BotState a b t) m ()
delChat chatId = do
  chatsOld <- gets $ chats . database
  setNewChats $ delete chatId chatsOld

setRepeatsAmount :: Monad m => ChatId -> RepeatsAmount -> (BotState a b t) m ()
setRepeatsAmount chatId repAmount = do
  delChat chatId
  addChat chatId repAmount

updateTime :: (BotState a b t) IO ()
updateTime = do
  db <- gets database
  newTime <- lift getCurrentTime
  setDb (db {prevTime = newTime})

backup :: (Show a, Show b, Bot t) => (BotState a b t) IO ()
backup = do
  path <- gets $backupPath . config
  name <- gets $ name . bot
  let fullPath = path ++ name ++ ".backup"
  db <- gets database
  let strDB = show db
  lift $ writeFile fullPath strDB

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
