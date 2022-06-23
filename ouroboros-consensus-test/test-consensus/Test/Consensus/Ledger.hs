{-# LANGUAGE FlexibleInstances #-}
module Test.Consensus.Ledger (tests) where

import           Ouroboros.Consensus.Ledger.Basics hiding (LedgerState)
import           Ouroboros.Network.Block (Point, genesisPoint)
import           Test.QuickCheck hiding (elements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "Ledger"
  [ testGroup "DbChangelog"
      [ testProperty "tautology" prop_Tautology
      ]
  ]

prop_Tautology :: Property
prop_Tautology = property True

initAnchor :: LedgerState TestBlock EmptyMK
initAnchor = forgetLedgerTables testInitLedger




-- instance Arbitrary (DbChangelog (LedgerState TestBlock)) where




-- LedgerState TestBlock :: LedgerStateKind
-- LedgerStateKind :: MapKind -> Type

example :: DbChangelog (LedgerState TestBlock)
-- LedgerState EmptyBlock EmptyMK
-- l EmptyMK -> DbChangelog l --> LedgerState TestBlock EmptyMK -> _
example = emptyDbChangeLog initAnchor

    -- TestLedger {
    --     -- | The ledger state simply consists of the last applied block
    --     lastAppliedPoint      :: Point TestBlock
    --     -- | State that depends on the application of the block payload to the
    --     -- state.
    --   , payloadDependentState :: PayloadDependentState () mk -- TODO
    --   }

-- INVARIANTS:
--   changeLogDiffAnchor is the anchor of changeLogImmutableStates
--   the tip of changeLogImmutableStates is the anchor of changeLogVolatileStates

-- | data DbChangelog l = DbChangelog {
--     changelogDiffAnchor      :: !(WithOrigin SlotNo)
--   , changelogDiffs           :: !(LedgerTables l SeqDiffMK)
--   , changelogImmutableStates ::
--       !(AnchoredSeq
--           (WithOrigin SlotNo)
--           (DbChangelogState l)
--           (DbChangelogState l)
--        )
--   , changelogVolatileStates  ::
--       !(AnchoredSeq
--           (WithOrigin SlotNo)
--           (DbChangelogState l)
--           (DbChangelogState l)
--        )
--   }
--   deriving (Generic)
--
-- Combinators to test
--
-- emptyDbChangeLog
-- extendDbChangeLog
-- pruneVolatilePartDbChangelog (sic)
-- flushDbChangelog
-- prefixDbChangelog
-- prefixBackToAnchorDbChangelog
-- rollbackDbChangelog
-- youngestImmutableSlotDbChangelog
