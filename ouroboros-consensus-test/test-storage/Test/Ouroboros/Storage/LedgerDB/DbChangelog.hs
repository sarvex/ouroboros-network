{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Test.Ouroboros.Storage.LedgerDB.DbChangelog (tests) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import qualified Data.List.NonEmpty as NE
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ledger.Basics hiding (LedgerState)
import           Ouroboros.Consensus.Storage.LedgerDB.HD
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (Point (..))
import qualified Ouroboros.Network.Point as Point
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()
import           Test.QuickCheck hiding (elements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "Ledger"
  [ testGroup "DbChangelog"
      [ testProperty "empty changelog satisfies invariants"
        prop_emptyDbSatisfiesInvariants
      , testProperty "pruning changelog does not discard states"
        prop_pruneKeepsStates
      , testProperty "pruning changelog keeps at most maxRollback volatile states"
        prop_pruneKeepsAtMostMaxRollbacksVolatileStates
      , testProperty "pruning keeps changelog invariants"
        prop_pruneKeepsInvariants
      , testProperty "flushing keeps changelog invariants"
        prop_flushingKeepsInvariants
      ]

  ]

-- | Generators

genAnchoredSequence :: AS.Anchorable v a a => a -> (a -> Gen a) -> Gen (AS.AnchoredSeq v a a)
genAnchoredSequence anchor genNext = sized $ \n -> do
    k <- chooseInt (0, n)
    step (pure $ AS.Empty anchor) k anchor
  where
    step !acc k x =
      if k == 0 then acc else do
        acc' <- acc
        y <- genNext x
        step (pure $ acc' AS.:> y) (k - 1) y

genPoint :: Point TestBlock -> Gen (Point TestBlock)
genPoint = pure . Point . nextPoint' . getPoint
  where
    nextPoint' Origin = At (Point.Block 1 (TestHash $ 0 NE.:| []))
    nextPoint' (At (Point.Block slotNo hash)) = At (Point.Block (succ slotNo) hash)

genTestLedgerDbChangelogState :: DbChangelogState (LedgerState TestBlock)
  -> Gen (DbChangelogState (LedgerState TestBlock))
genTestLedgerDbChangelogState (DbChangelogState ledger) = do
  point <- genPoint $ lastAppliedPoint ledger
  pure $ DbChangelogState $ ledger { lastAppliedPoint = point }

genDbChangelog :: (GetTip (l EmptyMK), TableStuff l) =>
  l EmptyMK -> (DbChangelogState l -> Gen (DbChangelogState l)) -> Gen (DbChangelog l)
genDbChangelog anchor gen = do
  imm <- genAnchoredSequence (DbChangelogState anchor) gen
  vol <- genAnchoredSequence (AS.headAnchor imm) gen
  pure $ DbChangelog
    { changelogDiffAnchor = getTipSlot anchor
    , changelogDiffs = pureLedgerTables (ApplySeqDiffMK emptySeqUtxoDiff)
    , changelogImmutableStates = imm
    , changelogVolatileStates = vol
    }

instance Arbitrary (DbChangelog (LedgerState TestBlock)) where
  arbitrary = genDbChangelog (forgetLedgerTables $ testInitLedger) genTestLedgerDbChangelogState

-- | Properties

prop_emptyDbSatisfiesInvariants :: Property
prop_emptyDbSatisfiesInvariants =
  let dblog = emptyDbChangeLog $ forgetLedgerTables testInitLedger
  in property $ immutableAnchored dblog && volatileTipAnchorsImmutable dblog

prop_pruneKeepsStates ::
  SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
prop_pruneKeepsStates sp dblog = property $ AS.unsafeJoin imm vol == AS.unsafeJoin imm' vol'
  where imm = changelogImmutableStates dblog
        vol = changelogVolatileStates dblog
        dblog' = pruneVolatilePartDbChangelog sp dblog
        imm' = changelogImmutableStates dblog'
        vol' = changelogVolatileStates dblog'

-- TODO: Whether this checks anything might depend too much on the distribution of maxRollbacks
prop_pruneKeepsAtMostMaxRollbacksVolatileStates ::
  SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
prop_pruneKeepsAtMostMaxRollbacksVolatileStates (sp@SecurityParam { maxRollbacks }) dblog =
  property $ fromIntegral (AS.length vol') <= maxRollbacks
  where DbChangelog { changelogVolatileStates = vol' } = pruneVolatilePartDbChangelog sp dblog

prop_pruneKeepsInvariants ::
  SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
prop_pruneKeepsInvariants sp dblog =
  property $ checkInvariants $ pruneVolatilePartDbChangelog sp dblog

prop_flushingKeepsInvariants :: DbChangelog (LedgerState TestBlock) -> Property
prop_flushingKeepsInvariants dblog =
  let (toFlush, toKeep) = flushDbChangelog DbChangelogFlushAllImmutable dblog
  in property $ checkInvariants toFlush && checkInvariants toKeep


-- prop_extendDbChangelogKeepsImmutableStates :: Int -> Property
-- prop_extendDbChangelogKeepsImmutableStates i = undefined

-- prop_pruneTrimsVolatile :: SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
-- prop_pruneTrimsVolatile sp log = vol' AS.isPrefixOf vol
--   where log' = pruneVolatilePartDbChangelog sp log
--         vol = changelogVolatileStates log
--         vol' = changelogVolatileStates log'


-- | Invariants

volatileTipAnchorsImmutable :: (GetTip (l EmptyMK), Eq (l EmptyMK)) => DbChangelog l -> Bool
volatileTipAnchorsImmutable DbChangelog { changelogImmutableStates, changelogVolatileStates } =
  AS.anchor changelogVolatileStates == AS.headAnchor changelogImmutableStates

immutableAnchored :: DbChangelog (LedgerState TestBlock) -> Bool
immutableAnchored DbChangelog { changelogDiffAnchor, changelogImmutableStates } =
  changelogDiffAnchor == fmap Point.blockPointSlot point
  where
    point = getPoint $ lastAppliedPoint $ unDbChangelogState $ AS.anchor $ changelogImmutableStates

checkInvariants :: DbChangelog (LedgerState TestBlock) -> Bool
checkInvariants dblog = volatileTipAnchorsImmutable dblog && immutableAnchored dblog

-- | Generators:
-- Either
--  1. Via the internals <- this one!
--  2. Via the public API
--
-- | Invariants:
--   1. changeLogDiffAnchor is the anchor of changeLogImmutableStates
--   2. the tip of changeLogImmutableStates is the anchor of changeLogVolatileStates
--   3. something with increasing slot numbers (?)
--
-- | Combinators to test
--
-- emptyDbChangeLog
-- pruneVolatilePartDbChangelog (sic) :: GetTip (l EmptyMK) => SecurityParam -> DbChangelog l -> DbChangelog l
-- extendDbChangeLog :: (TableStuff l, GetTip (l EmptyMK)) => DbChangelog l -> l DiffMK -> DbChangelog l
-- flushDbChangelog :: => DbChangelogFlushPolicy -> DbChangelog l -> (DbChangelog l, DbChangelog l)
--
-- TODO: Play around with AS.rollback to understand this function
-- prefixDbChangelog
--
-- prefixBackToAnchorDbChangelog
-- rollbackDbChangelog
-- youngestImmutableSlotDbChangelog
--

