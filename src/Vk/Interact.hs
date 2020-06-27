{-# LANGUAGE ScopedTypeVariables #-}

module Vk.Interact where

import           Config
import           Control.Exception         (try)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (StateT (runStateT), gets, runState)
import           Data.Char                 (isDigit)
import qualified Data.Configurator.Types   as DataConfigurator
import           Data.Function             ((&))
import           Data.Maybe                (fromJust, isJust)
import           Data.Time                 (diffUTCTime, getCurrentTime)
import           GHC.Exception             (SomeException)
import qualified Vk.Api                    as Api
import qualified Vk.Database.Interact      as DB
import qualified Vk.Database.Types         as DB
import           Vk.Types

data Handle m a =
  Handle
    { hSendMessage          :: Config -> Integer -> String -> m a
    , hForwardMessage       :: Config -> Integer -> Integer -> m a
    , hForwardMessageNTimes :: Config -> Integer -> Integer -> Integer -> m a
    , hSendKyboardWithText  :: Config -> Integer -> String -> m a
    } 
--handleUpdate :: Monoid b => Handle IO b -> Config -> Update -> StateT DB.DB IO b
withErrorPrinting f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      lift $ print e
      return mempty
    Right x -> return x

onText h config userId text messageId repAmount = do
  isKeyboardResponse <- isKeyboardResponse userId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount userId (getIntByChar (head text)) >>
           return mempty
      else withErrorPrinting $
           (h&hForwardMessageNTimes) config userId messageId repAmount
  DB.delAwaitingChat userId
  return res
  where
    isKeyboardResponse userId text = do
      isAwaiting <- DB.isAwaiting userId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

handleUpdate h config update = do
  --lift $ print update
  let message = fromJust $ update & updateObject & objecttMessage
  let maybeText = message & messageText
  let userId = message & messageFromId
  let helpText_ = config & helpText
  let messageId_ = message & messageId
  repAmount <- DB.getRepeatsAmount userId
  case maybeText of
    Just "/start" -> withErrorPrinting $ (h&hSendMessage) config userId helpText_
    Just "/help" -> withErrorPrinting $ (h&hSendMessage) config userId helpText_
    Just "/repeat" -> onRepeat config userId repAmount
    Just text -> onText h config userId text messageId_ repAmount
    Nothing ->
      withErrorPrinting $
      (h&hForwardMessageNTimes) config userId messageId_ repAmount
  where
    repeatText_ repAmount = (config & repeatText) ++ show repAmount
    onRepeat config userId repAmount = do
      DB.addAwaitingChat userId
      withErrorPrinting $
        (h&hSendKyboardWithText) config userId (repeatText_ repAmount)

loop h config = do
  offset <- gets DB.offset
  isTimeToBackup <- isTimeToBackup
  when isTimeToBackup backUpAndUpdateTimer
  server <- gets DB.server
  token <- gets DB.accessToken
  offset <- gets DB.offset
  updatesAndOffset <-
    withErrorPrinting $ Api.getUpdatesAndOffset config server offset token
  DB.setOffset (snd updatesAndOffset)
  let filtredUpdates = filter isJustMessage (fst updatesAndOffset)
  handleUpdates filtredUpdates
  loop h config
  where
    handleUpdates = mapM_ (handleUpdate h config)
    isJustMessage = isJust . objecttMessage . updateObject
    isTimeToBackup = do
      prevTime <- DB.getFromDatabase DB.prevTime
      curTime <- lift getCurrentTime
      return $ (diffUTCTime curTime prevTime) > (config & diffTimeBackupPeriod)
    backUpAndUpdateTimer = do
      DB.backup "./backupVK.dat"
      DB.updateTime

start :: DataConfigurator.Config -> IO ()
start configHandle = do
  config <- (parseConfig configHandle)
  initDB <-
    DB.getRestoredOrNewDatabase "./backupVK.dat" (config & defaultRepeatAmount)
  let h =
        Handle
          { hSendMessage = Api.sendMessage
          , hForwardMessage = Api.forwardMessage
          , hForwardMessageNTimes = Api.forwardMessageNTimes
          , hSendKyboardWithText = Api.sendKeyboardWithText
          } 
  --runStateT (loop h config) initDB
  res <- runStateT (initAndGoLoop h config) initDB
  --print res
  return ()
  where
    initAndGoLoop h config = do
      Api.updateServerAndTokenAndOffset config
      lift $ print "going to the loop"
      loop h config
