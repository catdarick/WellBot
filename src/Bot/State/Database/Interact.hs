{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.State.Database.Interact where

import           Bot.Classes               (AdditionalType, Bot, BotStateIO,
                                            BotStateT, OffsetType, ReadShow,
                                            name)
import           Bot.State.Database.Types
import           Bot.State.Types           (BotState, bot, config, database)
import           Config
import           Control.Exception         (SomeException, try)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT, get, gets, modify, put)
import           Data.Aeson                (FromJSON, decode)
import qualified Data.ByteString.Lazy      as DataByteString
import           Data.Function             ((&))
import           Data.Map                  (Map, delete, fromList, lookup)
import           Data.Map.Strict           (insert)
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (UTCTime, getCurrentTime)
import qualified Logger.Interact           as Log
import           Text.Read                 (readMaybe)

setDb ::
     (Bot a, Monad m)
  => Database (OffsetType a) (AdditionalType a)
  -> (BotStateT a) m ()
setDb db = do
  state <- get
  put $ state {database = db}

setOffset :: Bot a => OffsetType a -> BotStateIO a ()
setOffset newOffset = do
  offset <- getOffset
  when (offset /= newOffset) $
    Log.info $ "Offset updated to: " ++ show newOffset
  db <- gets database
  setDb $ db {offset = newOffset}

getOffset :: Bot a => BotStateIO a (OffsetType a)
getOffset = gets $ offset . database

isAwaiting :: Bot a => Integer -> BotStateIO a Bool
isAwaiting chatId = do
  db <- gets database
  let awChatsVal = db & awaitingChatsID
  return $ chatId `elem` awChatsVal

getRepeatsAmount :: Bot a => Config -> Integer -> BotStateIO a Integer
getRepeatsAmount config userOrChatId = do
  chats <- gets $chats . database
  let defaultRepeatsAmount = config & defaultRepeatAmount
  let res = Data.Map.lookup userOrChatId chats
  return $ fromMaybe defaultRepeatsAmount res

delAwaitingChat :: Bot a => Integer -> BotStateIO a ()
delAwaitingChat chatId = do
  isAwaiting <- isAwaiting chatId
  when isAwaiting $
    Log.info $ "Removed awaiting for keyboard response: " ++ show chatId
  db <- gets database
  let awChatsVal = db & awaitingChatsID
  let newChatsVal = filter (chatId /=) awChatsVal
  setDb (db {awaitingChatsID = newChatsVal})

addAwaitingChat :: Bot a => Integer -> BotStateIO a ()
addAwaitingChat chatId = do
  Log.info $ "Added awaiting for keyboard response: " ++ show chatId
  db <- gets database
  let awaitingChats = db & awaitingChatsID
  setDb db {awaitingChatsID = chatId : awaitingChats}

addChat :: Bot a => ChatId -> RepeatsAmount -> BotStateIO a ()
addChat chatId repAmount = do
  chatsOld <- gets $ chats . database
  setNewChats $ insert chatId repAmount chatsOld

setNewChats :: Bot a => Chats -> BotStateIO a ()
setNewChats newChats = do
  db <- gets database
  setDb (db {chats = newChats})

delChat :: Bot a => ChatId -> BotStateIO a ()
delChat chatId = do
  chatsOld <- gets $ chats . database
  setNewChats $ delete chatId chatsOld

setRepeatsAmount :: Bot a => ChatId -> RepeatsAmount -> BotStateIO a ()
setRepeatsAmount chatId repAmount = do
  Log.info $ "Repeats for " ++ show chatId ++ " changed to " ++ show repAmount
  delChat chatId
  addChat chatId repAmount

backup :: Bot a => BotStateIO a ()
backup = do
  path <- gets $backupPath . config
  name <- gets $ name . bot
  let fullPath = path ++ name ++ ".backup"
  db <- gets database
  let strDB = show db
  lift $ writeFile fullPath strDB

getRestoredOrClearDatabase ::
     (Read a, Read b) => FilePath -> a -> IO (Database a b)
getRestoredOrClearDatabase path defaultOffset = do
  curTime <- getCurrentTime
  eitherText <- try $ readFile path
  let defaultDB = getInitialDatabase defaultOffset curTime
  either
    (retDefalut defaultDB)
    (retRestoredOrDefaultIfCantParse defaultDB curTime)
    eitherText
  where
    retDefalut (defaultDB :: (Database a b)) (e :: SomeException) =
      return defaultDB
    retRestoredOrDefaultIfCantParse defaultDB curTime text = do
      let maybeDB = readMaybe text
      return $ fromMaybe defaultDB maybeDB

getInitialDatabase :: offsetType -> p -> Database offsetType additionalInfoType
getInitialDatabase defaultOffset curTime =
  Database
    { offset = defaultOffset
    , chats = fromList []
    , awaitingChatsID = []
    , additionalInfo = Nothing
    }
