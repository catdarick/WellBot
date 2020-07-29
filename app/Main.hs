{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Bot.Logic
import           Config
import           Control.Concurrent
import           Control.Exception  (Exception, SomeException, try)
import           Control.Monad      (forM, void, when)
import           Data.Configurator
import           Data.Function      ((&))
import           Telegram.Instances
import           Vk.Instances
import           Vk.Types

main :: IO ()
main = do
  eitherCfg <- try $ load [Required "$(HOME)/configs/bot.cfg"]
  case eitherCfg of
    Left (e :: SomeException) -> print "Can't open config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      case checkConfig config of
        Left badField ->
          print $ "The field must be set in the config file: " ++ badField
        Right _ -> runBots config

runBots :: Config -> IO ()
runBots config = do
  doneTg <- newEmptyMVar
  doneVk <- newEmptyMVar
  when (config & tgEnabled) $
    void $ forkIO $ start TgBot config >> putMVar doneTg ()
  when (config & vkEnabled) $
    void $ forkIO $ start VkBot config >> putMVar doneVk ()
  when (config & tgEnabled) $ takeMVar doneTg
  when (config & vkEnabled) $ takeMVar doneVk
