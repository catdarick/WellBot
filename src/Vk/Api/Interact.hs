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
import           Vk.Keyboard.Builder
import           Vk.Api.Types

--doGetRequest :: Config -> String -> [(String, String)] -> IO LBS.ByteString
doGetRequest config server method queryPairs = do
  initReq <- Conduit.parseRequest server
  let req = setPathAndQueryString initReq bsUrlPath bsQueryPairs
 -- lift $ print req
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

--getUpdates :: (Show a) => Config -> a -> StateT DB.DB IO LBS.ByteString
--k = ZJust (Updates {updatesTs = "10", updatesUpdates = [Update {updateType = "message_new", updateObject = fromList [("client_info",Object (fromList [("lang_id",Number 0.0),("button_actions",Array [String "text",String "vkpay",String "open_app",String "location",String "open_link"]),("keyboard",Bool True),("inline_keyboard",Bool True)])),("message",Object (fromList [("attachments",Array []),("text",String "dfv"),("peer_id",Number 3.0651165e7),("conversation_message_id",Number 9.0),("random_id",Number 0.0),("date",Number 1.593206986e9),("from_id",Number 3.0651165e7),("is_hidden",Bool False),("fwd_messages",Array []),("id",Number 9.0),("important",Bool False),("out",Number 0.0)]))]}]})
getUpdatesAndOffset config server offset token = do
  let tokenPair = ("key", token)
  let timeoutPair = ("wait", show $ config & secTimeout)
  let offsetPair = ("ts", offset)
  let queryPairs = [actionPair, tokenPair, offsetPair, timeoutPair, versionPair]
  bsResponse <- doGetRequest config server "" queryPairs
  let maybeUpdates = decode bsResponse :: Maybe Updates
  print bsResponse
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

getServerAndTokenAndOffset config = do
  let server = "https://api.vk.com/"
  let path = "method/groups.getLongPollServer"
  let groupIdPair = ("group_id", show $ config & vkGroupId)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [groupIdPair, tokenPair, versionPair]

  bsResponse <- doGetRequest config server path queryPairs
  print bsResponse
  let maybeResponse = decode bsResponse :: Maybe (Response Longpoll)
  return maybeResponse


sendMessage config userId text = do
  randomInt64 <- (getStdRandom (randomR (0, 2 ^ 64 - 1)) :: IO Integer)
  let server = "https://api.vk.com/"
  let path = "method/messages.send"
  let randomIdPair = ("random_id", show randomInt64)
  let userIdPair = ("user_id", show userId)
  let textPair = ("message", text)
  let tokenPair = ("access_token", config & vkToken)
  let versionPair = ("v", config & vkApiVersion)
  let queryPairs = [userIdPair, randomIdPair, textPair, tokenPair, versionPair]
  bsResponse <- doGetRequest config server path queryPairs
  return ()

forwardMessage config userId messageId = do
  randomInt64 <- (getStdRandom (randomR (0, 2 ^ 64 - 1)) :: IO Integer)
  let server = "https://api.vk.com/"
  let path = "method/messages.send"
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
  bsResponse <- doGetRequest config server path queryPairs
    -- print bsResponse
  return ()

forwardMessageNTimes config chatId messageId n =
  replicateM_ (fromInteger n) (forwardMessage config chatId messageId)

sendKeyboardWithText config userId text = do
  randomInt64 <- (getStdRandom (randomR (0, 2 ^ 64 - 1)) :: IO Integer)
  let server = "https://api.vk.com/"
  let path = "method/messages.send"
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
  bsResponse <- doGetRequest config server path queryPairs
    -- print bsResponse
  return ()
