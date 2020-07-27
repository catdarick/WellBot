module Telegram.Keyboard.Builder where

import           Config
import           Data.Aeson                 (encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Function              ((&))
import           Telegram.Keyboard.Types

getKeyboardJSON :: Config -> String
getKeyboardJSON config = do
  let kAmount = config & keysAmount
  unpack $
    encode $
    Markup
      { markupKeyboard = [map intToKey [1,2 .. kAmount]]
      , markupOneTimeKeyboard = True
      , markupResizeKeyboard = True
      }
  where
    intToKey x = Button {buttonText = show x}
