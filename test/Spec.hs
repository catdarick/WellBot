import           Telegram.UpdateHandlerTest
import           Test.HUnit                 (runTestTT)

main :: IO ()
main = do
  runTestTT tests
  return ()
