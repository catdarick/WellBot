{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Bot.Logic
import           Config
import           Control.Concurrent
import           Control.Exception       (Exception, SomeException, try)
import           Control.Monad           (forM)
import           Data.Configurator
import           Data.Configurator.Types (Config)
import           Telegram.Instances
import           Vk.Instances
import           Vk.Types

main :: IO ()
main = do
  eitherCfg <- try $ load [Required "$(PWD)/app/bot.cfg"]
  case eitherCfg of
    Left (e :: SomeException) -> print "Can't find config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      doneTg <- newEmptyMVar
      doneVk <- newEmptyMVar
      forkIO $ start TgBot config >> putMVar doneTg ()
      forkIO $ start VkBot config >> putMVar doneVk ()
      takeMVar doneTg
      takeMVar doneVk
      return ()
