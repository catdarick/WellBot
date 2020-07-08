module Vk.Types where

data VkBot =
  VkBot

data Additional =
  Additional
    { server        :: String
    , longpollToken :: String
    }
  deriving (Show, Read)