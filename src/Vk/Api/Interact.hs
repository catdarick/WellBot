module Vk.Api.Interact where

import           Config
import           Control.Exception          (throw)
import           Control.Monad              (replicateM_)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.State  (StateT, gets)
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function              ((&))
import           Data.Maybe                 (fromMaybe, isJust)
import           GHC.Exception              (errorCallException)
import qualified Network.HTTP.Client        as Conduit
import           Network.HTTP.Simple        (httpBS)
import           System.Random              (getStdRandom, randomR)
import           Vk.Api.Types
import           Vk.Keyboard.Builder

doGetRequest :: String -> String -> [(String, String)] -> IO LBS.ByteString
doGetRequest server method queryPairs = do
  initReq <- Conduit.parseRequest server
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
  bsResponse <- httpBS req
  returnResponseBody bsResponse
  where
    stringPairToByteStringPair (k, v) = (BS.pack k, Just $ BS.pack v)
    bsUrlPath = BS.pack method
    setPath reqVal path =
      reqVal {Conduit.path = (reqVal & Conduit.path) `mappend` path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    returnResponseBody x = return $ LBS.fromStrict $ Conduit.responseBody x

getUpdatesAndOffset ::
     Config -> String -> String -> String -> IO (Maybe ([Update], String))
getUpdatesAndOffset config server offset token = do
  let tokenPair = ("key", token)
  let timeoutPair = ("wait", show $ config & secTimeout)
  let offsetPair = ("ts", offset)
  let queryPairs = [actionPair, tokenPair, offsetPair, timeoutPair, versionPair]
  bsResponse <- doGetRequest server "" queryPairs
  let maybeUpdates = decode bsResponse :: Maybe Updates
  case maybeUpdates of
    Just updates ->
      return $ Just (updates & updatesUpdates, updates & updatesTs)
    Nothing -> do
      let maybeError = decode bsResponse :: Maybe Error
      if isJust maybeError
        then return Nothing
        else throw $ errorCallException "Cannot parse updates"
  where
    versionPair = ("v", config & vkApiVersion)
    actionPair = ("act", "a_check")

defaultServer :: String
defaultServer = "https://api.vk.com/"

sendPath :: String
sendPath = "method/messages.send"

getServerAndTokenAndOffset :: Config -> IO (Maybe (Response Longpoll))
getServerAndTokenAndOffset config = do
  let path = "method/groups.getLongPollServer"
  let groupIdPair = ("group_id", show $ config & vkGroupId)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [groupIdPair, tokenPair, versionPair]
  bsResponse <- doGetRequest defaultServer path queryPairs
  let maybeResponse = decode bsResponse :: Maybe (Response Longpoll)
  return maybeResponse

sendMessage :: Config -> UserId -> String -> IO ()
sendMessage config userId text = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", text)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [userIdPair, randomIdPair, textPair, tokenPair, versionPair]
  bsResponse <- doGetRequest defaultServer sendPath queryPairs
  return ()

forwardMessage :: Config -> UserId -> MessageId -> IO ()
forwardMessage config userId messageId = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", "")
  let forwardPair = ("forward_messages", show messageId)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs =
        [ userIdPair
        , forwardPair
        , randomIdPair
        , textPair
        , tokenPair
        , versionPair
        ]
  bsResponse <- doGetRequest defaultServer sendPath queryPairs
  return ()

sendKeyboardWithText :: Config -> UserId -> String -> IO ()
sendKeyboardWithText config userId text = do
  randomInt64 <- getRandomInt64
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", text)
  let keyboardPair = ("keyboard", getKeyboardJSON config)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs =
        [ userIdPair
        , keyboardPair
        , randomIdPair
        , textPair
        , tokenPair
        , versionPair
        ]
  bsResponse <- doGetRequest defaultServer sendPath queryPairs
  return ()

getRandomInt64 :: IO Integer
getRandomInt64 = getStdRandom (randomR (0, 2 ^ 64 - 1))
