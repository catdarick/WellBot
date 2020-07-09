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
    Replykeyboardmarkup
      { replykeyboardmarkupKeyboard = [map intToKey [1,2 .. kAmount]]
      , replykeyboardmarkupOneTimeKeyboard = True
      , replykeyboardmarkupResizeKeyboard = True
      }
  where
    intToKey x = Keyboardbutton {keyboardbuttonText = show x}
