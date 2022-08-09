module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Byron.Golden (tests)
import qualified Test.Consensus.Byron.Serialisation (tests)
import qualified Test.ThreadNet.Byron (tests)
import qualified Test.ThreadNet.DualByron (tests)
import           Test.Util.TestMode (defaultMainWithIohkTestMode)

main :: IO ()
main = defaultMainWithIohkTestMode tests

tests :: TestTree
tests =
  testGroup "byron"
  [ Test.Consensus.Byron.Golden.tests
  , Test.Consensus.Byron.Serialisation.tests
  , Test.ThreadNet.Byron.tests
  , Test.ThreadNet.DualByron.tests
  ]
