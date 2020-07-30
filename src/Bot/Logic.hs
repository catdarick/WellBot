{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Logic where

import           Bot.Classes
import           Bot.ErrorException
import qualified Bot.State.Database.Interact as DB
import qualified Bot.State.Database.Types    as DB
import qualified Bot.State.Interact          as State
import           Bot.State.Types
import           Bot.Synonyms
import           Config
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (replicateM_, void, when)
import           Control.Monad.Catch         (try)
import           Control.Monad.Extra         (whenM)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.State   (StateT (runStateT), gets)
import           Data.Char                   (isDigit)
import qualified Data.Configurator.Types     as Configurator
import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (UTCTime, diffUTCTime,
                                              getCurrentTime)
import qualified Logger.Interact             as Log

onRepeat :: Bot a => UserOrChatId -> BotStateIO a (RetType a)
onRepeat userOrChatId = do
  (bot, config) <- State.getBotAndConfig
  repAmount <- DB.getRepeatsAmount config userOrChatId
  let repeatText_ = (config & repeatText) ++ show repAmount
  DB.addAwaitingChat userOrChatId
  Log.withDebugLogging "sendKeyboardWithText" $
    Log.withErrorLogging $
    sendKeyboardWithText bot config userOrChatId repeatText_

onText ::
     Bot a => UserOrChatId -> String -> MesssageId -> BotStateIO a (RetType a)
onText userOrChatId text messageId = do
  isKeyboardResponse <- isKeyboardResponse userOrChatId text
  res <-
    if isKeyboardResponse
      then DB.setRepeatsAmount userOrChatId (getIntByChar (head text)) >>
           return mempty
      else echoWithLogging userOrChatId messageId
  DB.delAwaitingChat userOrChatId
  return res
  where
    isKeyboardResponse userOrChatId text = do
      isAwaiting <- DB.isAwaiting userOrChatId
      return $ isAwaiting && (length text == 1 && isDigit (head text))
    getIntByChar ch = toInteger $ fromEnum ch - fromEnum '0'

echoWithLogging :: Bot a => Integer -> MesssageId -> BotStateIO a (RetType a)
echoWithLogging userOrChatId messageId = do
  (bot, config) <- State.getBotAndConfig
  repAmount <- DB.getRepeatsAmount config userOrChatId
  Log.withDebugLogging "forwardMessageNTimes" $
    Log.withErrorLogging $
    forwardMessageNTimes bot config userOrChatId messageId repAmount

sendHelpWithLogging :: Bot a => UserOrChatId -> BotStateIO a (RetType a)
sendHelpWithLogging userOrChatId = do
  (bot, config) <- State.getBotAndConfig
  Log.withDebugLogging "sendMessage" $
    Log.withErrorLogging $
    sendMessage bot config userOrChatId (config & helpText)

handleUpdate :: Bot a => MessageType a -> BotStateIO a (RetType a)
handleUpdate update = do
  let maybeText = getMaybeText update
  let userOrChatId = getUserOrChatId update
  let messageId = getMessageId update
  case maybeText of
    Just "/start"  -> sendHelpWithLogging userOrChatId
    Just "/help"   -> sendHelpWithLogging userOrChatId
    Just "/repeat" -> onRepeat userOrChatId
    Just text      -> onText userOrChatId text messageId
    Nothing        -> echoWithLogging userOrChatId messageId

getAndHandleUpdates :: Bot a => BotStateIO a ()
getAndHandleUpdates = do
  (updates, newOffset) <-
    Log.withDebugLogging "getUpdateMessagesAndOffset" getUpdateMessagesAndOffset
  handleUpdates updates
  DB.setOffset newOffset
  whenM State.isTimeToBackup State.backupAndUpdateTimer
  where
    handleUpdates = mapM_ handleUpdateWithDebugLogging
    handleUpdateWithDebugLogging update =
      Log.withDebugLogging "handleUpdate" $ handleUpdate update

loop :: Bot a => BotStateIO a ()
loop = do
  res <- try getAndHandleUpdates
  case res of
    Left BadTokenException -> Log.fatal $ show BadTokenException
    Left NoConnectionException -> logAndGoLoopAfterDelay NoConnectionException
    Left warnException -> logAndGoLoop warnException
    Right _ -> loop
  where
    logAndGoLoopAfterDelay exc = do
      Log.warn $ show NoConnectionException
      lift $ threadDelay (10 ^ 7)
      loop
    logAndGoLoop exc = do
      Log.warn (show exc)
      loop

start :: Bot a => a -> Config -> IO ()
start bot config = do
  botState <- State.getInitialBotState bot config
  res <- runStateT initAndGoLoop botState
  return ()
  where
    initAndGoLoop = do
      initBot
      loop
