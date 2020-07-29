module Logic.Test where

import           Bot.Classes
import           Config
import           Control.Monad.Trans.State (StateT (runStateT))
import           Data.ByteString.Internal  (inlinePerformIO)
import           Data.Function             ((&))
import           Bot.Logic
import           Logic.Helpers
import           Test.HUnit
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

tests :: Test
tests =
  TestList
    [ TestLabel "test: got /repeat" testRepeat
    , TestLabel "test: got response on /repeat " testKeyboardResponse
    , TestLabel "test: got message (not memorized user)" testForwardDefault
    , TestLabel "test: got message (already memorized user)" testForwardWithAlreadySet
    , TestLabel "test: got /help" testHelp
    , TestLabel "test: got message without text" testEcho
    ]

testRepeat :: Test
testRepeat = TestCase (assertEqual "must send keyboard with repeat message" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset (Just "/repeat")
    expectedState = getStateWithDb $ getDb defOffset True False
    expectedAction =
      unsafeDupablePerformIO $
      sendKeyboardWithText
        TestBot
        defConfig
        defChatOrUserId
        ((defConfig & repeatText) ++ show (defConfig & defaultRepeatAmount))
    expectedRes = (expectedAction, expectedState)

testKeyboardResponse :: Test
testKeyboardResponse =
  TestCase (assertEqual "must update repeat value for user in database" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset True False
    initUpdate = getUpdateWithTextAndOffset (Just "3")
    expectedState = getStateWithDb $ getDb defOffset False True
    expectedAction = ""
    expectedRes = (expectedAction, expectedState)

testForwardDefault :: Test
testForwardDefault =
  TestCase (assertEqual "must forward user's message (default amount)" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset (Just "smth")
    expectedState = getStateWithDb $ getDb defOffset False False
    expectedAction =
      unsafeDupablePerformIO $
      forwardMessageNTimes
        TestBot
        defConfig
        defChatOrUserId
        defMessageId
        (defConfig & defaultRepeatAmount)
    expectedRes = (expectedAction, expectedState)

testForwardWithAlreadySet :: Test
testForwardWithAlreadySet =
  TestCase (assertEqual "must forward user's message (already set amount)" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset False True
    initUpdate = getUpdateWithTextAndOffset (Just "smth")
    expectedState = getStateWithDb $ getDb defOffset False True
    expectedAction =
      unsafeDupablePerformIO $
      forwardMessageNTimes
        TestBot
        defConfig
        defChatOrUserId
        defMessageId
        defSettedRepAmount
    expectedRes = (expectedAction, expectedState)

testHelp :: Test
testHelp = TestCase (assertEqual "must send help message" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset False True
    initUpdate = getUpdateWithTextAndOffset (Just "/help")
    expectedState = getStateWithDb $ getDb defOffset False True
    expectedAction =
      unsafeDupablePerformIO $
      sendMessage TestBot defConfig defChatOrUserId (defConfig & helpText)
    expectedRes = (expectedAction, expectedState)

testEcho :: Test
testEcho = TestCase (assertEqual "must forward message without text (sticker e.g.)" expectedRes res)
  where
    res =
      unsafeDupablePerformIO $
      runStateT (handleUpdate initUpdate) initState
    initState = getStateWithDb $ getDb defOffset False False
    initUpdate = getUpdateWithTextAndOffset Nothing
    expectedState = getStateWithDb $ getDb defOffset False False
    expectedAction =
      unsafeDupablePerformIO $
       forwardMessageNTimes
        TestBot
        defConfig
        defChatOrUserId
        defMessageId
        (defConfig & defaultRepeatAmount)
    expectedRes = (expectedAction, expectedState)