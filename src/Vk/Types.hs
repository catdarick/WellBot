module Vk.Types where

data VkBot =
  VkBot

data LongpollInfo =
  LongpollInfo
    { server        :: String
    , longpollToken :: String
    }
  deriving (Show, Read)

badLongpollInfo :: LongpollInfo
badLongpollInfo = LongpollInfo "" ""