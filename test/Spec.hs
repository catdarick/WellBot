import qualified Logic.Test as Logic
import           Test.HUnit                 (runTestTT)


main :: IO ()
main = do
  runTestTT Logic.tests
  --runTestTT VK.tests
  return ()
