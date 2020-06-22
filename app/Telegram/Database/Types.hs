module Telegram.Database.Types where

data Chat =
  Chat
    { chatID        :: Integer
    , repeatsAmount :: Integer
    }
  deriving (Show)

-------
data DB =
  DB
    { offset              :: Integer
    , defaultRepeatAmount :: Integer
    , chats               :: [Chat]
    , awaitingChatsID     :: [Integer]
    }
  deriving (Show)
