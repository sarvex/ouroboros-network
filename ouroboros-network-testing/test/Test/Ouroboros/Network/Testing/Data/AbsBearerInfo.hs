module Test.Ouroboros.Network.Testing.Data.AbsBearerInfo
  ( tests
  ) where

import qualified Data.List.NonEmpty as NonEmpty

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                   ( AbsBearerInfo,
                     BearerInfoScript(..),
                     NonFailingBearerInfoScript(..), canFail )
import           Ouroboros.Network.Testing.Data.Script (Script(Script))

import           Test.Tasty ( testGroup, TestTree )
import           Test.QuickCheck (Fixed (..), Arbitrary (..))
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Ouroboros.Testing.Data.AbsBearerInfo"
    [ testGroup "generators"
      [ testProperty "shrinker AbsBearerInfo" prop_shrinker_AbsBearerInfo
      , testProperty "shrinker BearerInfoScript" prop_shrinker_BearerInfoScript
      , testProperty "generator NonFailingBearerInfoScript"
          prop_generator_NonFailingBeararInfoScript
      ]
    ]

prop_shrinker_AbsBearerInfo :: Fixed AbsBearerInfo -> Bool
prop_shrinker_AbsBearerInfo (Fixed abi) =
    abi `notElem` shrink abi

prop_shrinker_BearerInfoScript :: Fixed BearerInfoScript -> Bool
prop_shrinker_BearerInfoScript (Fixed bis) =
    all (\bis'@(BearerInfoScript (Script s)) ->
                  bis /= bis'
               && not (canFail (NonEmpty.last s))
        )
        (shrink bis)

prop_generator_NonFailingBeararInfoScript :: NonFailingBearerInfoScript -> Bool
prop_generator_NonFailingBeararInfoScript (NonFailingBearerInfoScript s) = not (any canFail s)
