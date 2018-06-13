import qualified Parser.Tests
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    Parser.Tests.tests
  ]
