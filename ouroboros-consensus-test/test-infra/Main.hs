module Main (main) where

import           Test.Tasty

import qualified Test.ThreadNet.Util.Tests (tests)
import qualified Test.Util.ChainUpdates.Tests (tests)
import qualified Test.Util.Schedule.Tests (tests)
import qualified Test.Util.Split.Tests (tests)
import           Test.Util.TestMode (defaultMainWithIohkTestMode)

main :: IO ()
main = defaultMainWithIohkTestMode tests

tests :: TestTree
tests =
  testGroup "test-infra"
  [ Test.ThreadNet.Util.Tests.tests
  , Test.Util.ChainUpdates.Tests.tests
  , Test.Util.Schedule.Tests.tests
  , Test.Util.Split.Tests.tests
  ]
