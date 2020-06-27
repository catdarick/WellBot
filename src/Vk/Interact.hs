module Vk.Interact where
import qualified Vk.Api as Api
import qualified Data.Configurator.Types as DataConfigurator
import qualified Vk.Database.Types as DB
import qualified Vk.Database.Interact as DB
import Config
import Control.Monad.Trans.State (gets, StateT(runStateT))
import Data.Function ((&))
{- data Handle m a =
  Handle
    { hSendMessage          :: Config -> Integer -> String -> m a
    , hForwardMessage       :: Config -> Integer -> Integer -> m a
    , hForwardMessageNTimes :: Config -> Integer -> Integer -> Integer -> m a
    , hSendKyboardWithText  :: Config -> Integer -> String -> m a
    } -}

tmp config = do
    Api.updateServerAndTokenAndOffset config
    Api.sendMessage config 30651165 "456"
    offset <- gets DB.offset
    Api.getUpdates config 

start :: DataConfigurator.Config -> IO ()
start configHandle = do
  config <- (parseConfig configHandle)
  initDB <- DB.getRestoredOrNewDatabase "./backupVK.dat" (config & defaultRepeatAmount)
{-   let h =
        Handle
          { hSendMessage = Api.sendMessage
          , hForwardMessage = Api.forwardMessage
          , hForwardMessageNTimes = Api.forwardMessageNTimes
          , hSendKyboardWithText = Api.sendKyboardWithText
          } -}
  --runStateT (loop h config) initDB
  res<-runStateT (tmp config) initDB
  print res
  return ()