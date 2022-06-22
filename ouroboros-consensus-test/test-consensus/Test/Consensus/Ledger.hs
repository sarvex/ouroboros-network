module Test.Consensus.Ledger (tests) where

import           Test.QuickCheck hiding (elements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ledger"
  [ testGroup "DbChangelog"
      [ testProperty "tautology" prop_Tautology
      ]
  ]

prop_Tautology :: Property
prop_Tautology = property True
