import qualified Telegram.UpdateHandlerTest as TG
import           Test.HUnit                 (runTestTT)
import qualified Vk.UpdateHandlerTest       as VK

main :: IO ()
main = do
  runTestTT TG.tests
  runTestTT VK.tests
  return ()
