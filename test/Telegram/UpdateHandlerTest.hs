module Telegram.UpdateHandlerTest where

import           Control.Monad.Trans.State (StateT (runStateT))
import           Data.Function             ((&))
import           Data.Functor.Identity     (Identity (Identity), runIdentity)
import           Data.Text.Unsafe          (inlinePerformIO)
import           Config
import qualified Telegram.Database.Types   as DB
import           Telegram.Interact
import           Telegram.TestHelpers
import           Telegram.Types
import           Test.HUnit

testSendMessage ::
     (Monad m) => Config -> Integer -> String -> m (String, String)
testSendMessage config chatId text = return ("send", show chatId ++ " " ++ text)

testSendKeyboard ::
     (Monad m) => Config -> Integer -> String -> m (String, String)
testSendKeyboard config chatId text =
  return ("send", show chatId ++ " " ++ text)

testForward :: (Monad m) => Config -> Integer -> Integer -> m (String, String)
testForward config chatId messageId =
  return ("forward", show chatId ++ " " ++ show messageId)

testForwardNTimes ::
     (Monad m) => Config -> Integer -> Integer -> Integer -> m (String, String)
testForwardNTimes config chatId messageId n =
  return
    ("forwardNTimes", show chatId ++ " " ++ show messageId ++ " " ++ show n)

h :: Handle IO (String, String)
h =
  Handle
    { hSendMessage = testSendMessage
    , hForwardMessage = testForward
    , hForwardMessageNTimes = testForwardNTimes
    , hSendKyboardWithText = testSendKeyboard
    }

tests :: Test
tests =
  TestList
    [ TestLabel "test: got /repeat" testRepeat
    , TestLabel "test: got response on /repeat " testKeyboardResponse
    , TestLabel "test: got message (not memorized user)" testForwardDefault
    , TestLabel "test: got message (already memorized user)" testForward2Times
    , TestLabel "test: got /help" testHelp
    ]

testRepeat :: Test
testRepeat = TestCase (assertEqual "testRepeat failed" expectedRes res)
  where
    res = inlinePerformIO $ runStateT (handleUpdate h config initUpdate) initDb
    initDb = getDb False False 0 0
    initUpdate = getUpdateWithMessage "/repeat"
    expectedDb = getDb True False 0 (updateId_ + 1)
    expectedAction =
      runIdentity $
      testSendKeyboard config chatId_ ((config & repeatText) ++ show 1)
    expectedRes = (expectedAction, expectedDb)

testKeyboardResponse :: Test
testKeyboardResponse =
  TestCase (assertEqual "testKeyboardResponse failed" expectedRes res)
  where
    res = inlinePerformIO $ runStateT (handleUpdate h config initUpdate) initDb
    initDb = getDb True False 0 0
    initUpdate = getUpdateWithMessage "2"
    expectedDb = getDb False True 2 (updateId_ + 1)
    expectedAction = ("", "")
    expectedRes = (expectedAction, expectedDb)

testForwardDefault :: Test
testForwardDefault =
  TestCase (assertEqual "testForwardDefault failed" expectedRes res)
  where
    res = inlinePerformIO $ runStateT (handleUpdate h config initUpdate) initDb
    initDb = getDb False False 0 0
    initUpdate = getUpdateWithMessage "smt"
    expectedDb = getDb False False 0 (updateId_ + 1)
    expectedAction =
      runIdentity $
      testForwardNTimes config chatId_ messageId_ (config & defaultRepeatAmount)
    expectedRes = (expectedAction, expectedDb)

testHelp :: Test
testHelp = TestCase (assertEqual "testForwardDefault failed" expectedRes res)
  where
    res = inlinePerformIO $ runStateT (handleUpdate h config initUpdate) initDb
    initDb = getDb False False 0 0
    initUpdate = getUpdateWithMessage "/help"
    expectedDb = getDb False False 0 (updateId_ + 1)
    expectedAction =
      runIdentity $ testSendMessage config chatId_ (config & helpText)
    expectedRes = (expectedAction, expectedDb)

testForward2Times :: Test
testForward2Times =
  TestCase (assertEqual "testForward2Times failed" expectedRes res)
  where
    res = inlinePerformIO $ runStateT (handleUpdate h config initUpdate) initDb
    initDb = getDb False True 2 0
    initUpdate = getUpdateWithMessage "smt"
    expectedDb = getDb False True 2 (updateId_ + 1)
    expectedAction = runIdentity $ testForwardNTimes config chatId_ messageId_ 2
    expectedRes = (expectedAction, expectedDb)
