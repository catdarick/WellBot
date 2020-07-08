module Logic.Test where

import           Class.Bot
import           Config
import           Control.Monad.Trans.State (StateT (runStateT))
import           Data.ByteString.Internal  (inlinePerformIO)
import           Data.Function             ((&))
import           Logic
import           Logic.Helpers
import           Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "test: got /repeat" testRepeat
    , TestLabel "test: got response on /repeat " testKeyboardResponse
    , TestLabel "test: got message (not memorized user)" testForwardDefault
    , TestLabel "test: got message (already memorized user)" testForwardWithAlreadySet
    , TestLabel "test: got /help" testHelp
    , TestLabel "test: got message without text" testNothing
    ]

testRepeat :: Test
testRepeat = TestCase (assertEqual "testRepeat failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset (Just "/repeat")
    expectedDb = getDb defOffset True False
    expectedAction =
      inlinePerformIO $
      sendKeyboardWithText
        TestBot
        config
        defChatOrUserId
        ((config & repeatText) ++ show (config & defaultRepeatAmount))
    expectedRes = (expectedAction, expectedDb)

testKeyboardResponse :: Test
testKeyboardResponse =
  TestCase (assertEqual "testKeyboardResponse failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset True False
    initUpdate = getUpdateWithTextAndOffset (Just "3")
    expectedDb = getDb defOffset False True
    expectedAction = ""
    expectedRes = (expectedAction, expectedDb)

testForwardDefault :: Test
testForwardDefault =
  TestCase (assertEqual "testForwardDefault failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset (Just "smth")
    expectedDb = getDb defOffset False False
    expectedAction =
      inlinePerformIO $
      forwardMessageNTimes
        TestBot
        config
        defChatOrUserId
        defMessageId
        (config & defaultRepeatAmount)
    expectedRes = (expectedAction, expectedDb)

testForwardWithAlreadySet :: Test
testForwardWithAlreadySet =
  TestCase (assertEqual "testForwardWithAlreadySet failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset False True
    initUpdate = getUpdateWithTextAndOffset (Just "smth")
    expectedDb = getDb defOffset False True
    expectedAction =
      inlinePerformIO $
      forwardMessageNTimes
        TestBot
        config
        defChatOrUserId
        defMessageId
        defSettedRepAmount
    expectedRes = (expectedAction, expectedDb)

testHelp :: Test
testHelp = TestCase (assertEqual "testHelp failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset False True
    initUpdate = getUpdateWithTextAndOffset (Just "/help")
    expectedDb = getDb defOffset False True
    expectedAction =
      inlinePerformIO $
      sendMessage TestBot config defChatOrUserId (config & helpText)
    expectedRes = (expectedAction, expectedDb)

testNothing :: Test
testNothing = TestCase (assertEqual "testNothing failed" expectedRes res)
  where
    res =
      inlinePerformIO $
      runStateT (handleUpdate TestBot config initUpdate) initDb
    initDb = getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset Nothing
    expectedDb = getDb defOffset False False
    expectedAction =
      inlinePerformIO $
       forwardMessageNTimes
        TestBot
        config
        defChatOrUserId
        defMessageId
        (config & defaultRepeatAmount)
    expectedRes = (expectedAction, expectedDb)