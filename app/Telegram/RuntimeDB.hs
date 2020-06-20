{-# LANGUAGE OverloadedStrings #-}

module Telegram.RuntimeDB where
import Data.Configurator as Configurator
import Data.Configurator.Types  as Configurator.Types


data Config = Config
    { defaultRepeatsAmount::Integer
    } deriving (Show)

-------

data ChatSettings = ChatSettings
    { chatID :: Integer
    , repeatAmount :: Integer
    } deriving (Show)

-------

data DB = DB 
    { offset :: Integer
    , chats :: [ChatSettings]
    } deriving (Show)


-------------------------------------------------------

parseConfig :: Configurator.Types.Config -> IO Telegram.RuntimeDB.Config
parseConfig cfg = do
    repeatsAmount <- require cfg "defaultRepeatAmount" :: IO Integer
    return Config 
            {defaultRepeatsAmount = repeatsAmount}

    
    