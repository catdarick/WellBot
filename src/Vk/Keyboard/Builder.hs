module Vk.Keyboard.Builder where

import Vk.Keyboard.Types
import Config
import Data.Function ((&))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

getKeyboardJSON :: Config -> String
getKeyboardJSON config = do
  let kAmount = config & keysAmount
  unpack $
    encode $
    Keyboard
      { keyboardButtons = [map intToKey [1,2 .. kAmount]]
      , keyboardOneTime = True
      , keyboardInline = False
      }
  where
    intToKey x = Button {buttonAction = intToAction x, buttonColor = "secondary"}
    intToAction x = Action {actionType = "text", actionLabel = show x, actionPayload = show x} 