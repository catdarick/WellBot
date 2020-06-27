module Vk.Api where
import Config
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Client as Conduit
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Simple (httpBS)
import Data.Function ((&))
import qualified Vk.Database.Types as DB
import Control.Monad.Trans.State (StateT, gets)
import Vk.Types
import Data.Aeson (decode)
import qualified Vk.Database.Interact as DB
import GHC.Exception (errorCallException)
import Control.Exception (throw)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe (fromMaybe)
import System.Random (randomR, getStdRandom)
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
    setPath reqVal path = reqVal {Conduit.path = (reqVal & Conduit.path) `mappend` path}
    setPathAndQueryString req path query =
      Conduit.setQueryString query (setPath req bsUrlPath)
    bsQueryPairs = map stringPairToByteStringPair queryPairs
    returnResponseBody x = return $ LBS.fromStrict $ Conduit.responseBody x

--getUpdates :: (Show a) => Config -> a -> StateT DB.DB IO LBS.ByteString
--k = ZJust (Updates {updatesTs = "10", updatesUpdates = [Update {updateType = "message_new", updateObject = fromList [("client_info",Object (fromList [("lang_id",Number 0.0),("button_actions",Array [String "text",String "vkpay",String "open_app",String "location",String "open_link"]),("keyboard",Bool True),("inline_keyboard",Bool True)])),("message",Object (fromList [("attachments",Array []),("text",String "dfv"),("peer_id",Number 3.0651165e7),("conversation_message_id",Number 9.0),("random_id",Number 0.0),("date",Number 1.593206986e9),("from_id",Number 3.0651165e7),("is_hidden",Bool False),("fwd_messages",Array []),("id",Number 9.0),("important",Bool False),("out",Number 0.0)]))]}]})
getUpdates config = do
    server <- gets DB.server
    offset <- gets DB.offset
    token <- gets DB.accessToken
    let tokenPair = ("key", token)
    let timeoutPair = ("wait", show $ config & secTimeout)
    let offsetPair = ("ts", offset)
    let queryPairs = [actionPair, tokenPair, offsetPair, timeoutPair, versionPair]
    bsResponse <- doGetRequest config server "" queryPairs
    let maybeUpdates = decode bsResponse :: Maybe Updates
    --return $ fromMaybe [] updatesUpdates maybeUpdates
    lift $ print maybeUpdates
    case maybeUpdates of
                Just updates -> do
                    DB.setOffset $ updates & updatesTs
                    return $ updates & updatesUpdates
                Nothing -> do
                    throw $ errorCallException  "Cannot parse updates"
                    return []
        where
            versionPair = ("v", config & vkApiVersion)
            actionPair = ("act","a_check")

updateServerAndTokenAndOffset config = do
    let server = "https://api.vk.com/"    
    let path = "method/groups.getLongPollServer"
    let groupIdPair = ("group_id", show $ config & vkGroupId)
    let tokenPair = ("access_token", config & vkToken)
    let versionPair = ("v", config & vkApiVersion)
    let queryPairs = [groupIdPair, tokenPair, versionPair]
    bsResponse <- doGetRequest config server path queryPairs
    let maybeResponse = decode bsResponse :: Maybe (Response Longpoll)
    case maybeResponse of 
        Just response -> do
            DB.setServer $ response & responseResponse & longpollServer
            DB.setToken $ response & responseResponse & longpollKey
            DB.setOffset $ response & responseResponse & longpollTs
        Nothing -> throw $ errorCallException  "Cannot get server and token for LongPolling"

sendMessage config userId text = do
    randomInt64 <- lift (getStdRandom (randomR (0, 2^64 - 1)) :: IO Integer)
    let server = "https://api.vk.com/"    
    let path = "method/messages.send"
    let randomIdPair = ("random_id", show randomInt64)
    let userIdPair = ("user_id", show userId)
    let textPair = ("message", text)
    let tokenPair = ("access_token", config & vkToken)
    let versionPair = ("v", config & vkApiVersion)
    let queryPairs = [userIdPair,randomIdPair, textPair, tokenPair, versionPair]
    bsResponse <- doGetRequest config server path queryPairs
    lift $ print bsResponse
    return ()

    

