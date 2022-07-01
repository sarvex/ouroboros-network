{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Ouroboros.Storage.LedgerDB.DbChangelog (
    apply
  , run
  , tests
  ) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import qualified Data.FingerTree.Strict as FT
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Ledger.Basics hiding (LedgerState)
import           Ouroboros.Consensus.Storage.LedgerDB.HD
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (HeaderHash, Point (..), SlotNo (..),
                     StandardHash, pattern BlockPoint, pattern GenesisPoint)
import qualified Ouroboros.Network.Point as Point
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()
import           Test.QuickCheck hiding (elements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock
import           Text.Show.Pretty (ppShow)



tests :: TestTree
tests = testGroup "Ledger" [ testGroup "DbChangelog"
      [ testProperty "empty changelog satisfies invariants"
        prop_emptySatisfiesInvariants
      , testProperty "constructor generated changelog satisfies invariants"
        prop_generatedSatisfiesInvariants
      -- , testProperty "pruning changelog does not discard states"
      --   prop_pruneKeepsStates
      -- , testProperty "pruning changelog keeps at most maxRollback volatile states"
      --   prop_pruneKeepsAtMostMaxRollbacksVolatileStates
      -- , testProperty "pruning keeps changelog invariants"
      --   prop_pruneKeepsInvariants
      -- , testProperty "flushing keeps changelog invariants"
      --   prop_flushingKeepsInvariants
      ]

  ]

-- | Invariants

volatileTipAnchorsImmutable :: (GetTip (l EmptyMK), Eq (l EmptyMK)) => DbChangelog l -> Bool
volatileTipAnchorsImmutable DbChangelog { changelogImmutableStates, changelogVolatileStates } =
  AS.anchor changelogVolatileStates == AS.headAnchor changelogImmutableStates

immutableAnchored :: DbChangelog MinLedger -> Bool
immutableAnchored DbChangelog { changelogDiffAnchor, changelogImmutableStates } =
  changelogDiffAnchor == fmap Point.blockPointSlot point
  where point = getPoint $ pt $ unDbChangelogState $ AS.anchor $ changelogImmutableStates

sameNumberOfDiffsAsStates :: DbChangelog MinLedger -> Bool
sameNumberOfDiffsAsStates dblog = AS.length imm + AS.length vol == count diffs
  where imm = changelogImmutableStates dblog
        vol = changelogVolatileStates dblog
        ApplySeqDiffMK (SeqUtxoDiff diffs) = unT $ changelogDiffs dblog
        count = foldr (const (+ 1)) 0

checkInvariants :: DbChangelog MinLedger -> Bool
checkInvariants dblog = volatileTipAnchorsImmutable dblog &&
                        immutableAnchored dblog &&
                        sameNumberOfDiffsAsStates dblog

-- | Properties

prop_emptySatisfiesInvariants :: Property
prop_emptySatisfiesInvariants = property $ checkInvariants initDbChangelog

prop_generatedSatisfiesInvariants :: DbChangelog MinLedger -> Property
prop_generatedSatisfiesInvariants dblog = property $ checkInvariants dblog

-- | Generators

initDbChangelog :: DbChangelog MinLedger
initDbChangelog = emptyDbChangeLog anchor
  where anchor = MinLedger ApplyEmptyMK point
        point = Point Origin

data Op l = Extend (l DiffMK) | Prune SecurityParam
deriving instance Show (l DiffMK) => Show (Op l)

newtype ValidOpSeq l = ValidOpSeq [Op l]

-- TODO: This doesn't work since the points in the diff sequence need to be non-decreasing:
-- see Ouroboros.Consensus.Storage.LedgerDB.HD:397
apply :: (TableStuff l, GetTip (l EmptyMK)) => [Op l] -> DbChangelog l -> DbChangelog l
apply ops dblog = foldr' apply' dblog ops
  where apply' (Extend newState) dblog = extendDbChangelog dblog newState
        apply' (Prune sp) dblog        = pruneVolatilePartDbChangelog sp dblog

instance Arbitrary (l DiffMK) => Arbitrary (Op l) where
  arbitrary = oneof [Extend <$> arbitrary,
                     Prune <$> arbitrary]

instance (TableStuff l, GetTip (l EmptyMK),
          Arbitrary (l EmptyMK), Arbitrary (Op l)) => Arbitrary (DbChangelog l) where
  arbitrary = apply <$> arbitrary <*> (emptyDbChangeLog <$> arbitrary)

instance Arbitrary H where
  arbitrary = pure H

-- REVIEW: Instance exists in Test.Consensus.Ledger.Mock.Generators
-- NOTE: Attempting to extend a changelog with a state at tip GenesisPoint fails
instance Arbitrary (HeaderHash blk) => Arbitrary (Point blk) where
  arbitrary = BlockPoint <$> (SlotNo <$> arbitrary) <*> arbitrary

instance Arbitrary (MinLedger EmptyMK) where
  arbitrary = MinLedger ApplyEmptyMK <$> arbitrary

instance Arbitrary (UtxoEntryDiff Int) where
  arbitrary = UtxoEntryDiff <$> arbitrary <*> diffState
    where diffState = oneof [pure UedsDel, pure UedsIns, pure UedsInsAndDel]

instance Arbitrary (MinLedger DiffMK) where
  arbitrary = MinLedger <$> diff <*> arbitrary
    where diff = ApplyDiffMK . UtxoDiff <$> arbitrary



-- genAnchoredSequence :: AS.Anchorable v a a => a -> (a -> Gen a) -> Gen (AS.AnchoredSeq v a a)
-- genAnchoredSequence anchor genNext = sized $ \n -> do
--     k <- chooseInt (0, n)
--     step (pure $ AS.Empty anchor) k anchor
--   where
--     step !acc k x =
--       if k == 0 then acc else do
--         acc' <- acc
--         y <- genNext x
--         step (pure $ acc' AS.:> y) (k - 1) y

-- genPoint :: Point TestBlock -> Gen (Point TestBlock)
-- genPoint = pure . Point . nextPoint' . getPoint
--   where
--     nextPoint' Origin = At (Point.Block 1 (TestHash $ 0 NE.:| []))
--     nextPoint' (At (Point.Block slotNo hash)) = At (Point.Block (succ slotNo) hash)

-- genTestLedgerDbChangelogState :: DbChangelogState (LedgerState TestBlock)
--   -> Gen (DbChangelogState (LedgerState TestBlock))
-- genTestLedgerDbChangelogState (DbChangelogState ledger) = do
--   point <- genPoint $ lastAppliedPoint ledger
--   pure $ DbChangelogState $ ledger { lastAppliedPoint = point }

-- genDbChangelog :: (GetTip (l EmptyMK), TableStuff l) =>
--   l EmptyMK -> (DbChangelogState l -> Gen (DbChangelogState l)) -> Gen (DbChangelog l)
-- genDbChangelog anchor gen = do
--   imm <- genAnchoredSequence (DbChangelogState anchor) gen
--   vol <- genAnchoredSequence (AS.headAnchor imm) gen
--   pure $ DbChangelog
--     { changelogDiffAnchor = getTipSlot anchor
--     , changelogDiffs = pureLedgerTables (ApplySeqDiffMK emptySeqUtxoDiff)
--     , changelogImmutableStates = imm
--     , changelogVolatileStates = vol
--     }

-- instance Arbitrary (DbChangelog (LedgerState TestBlock)) where
--   arbitrary = genDbChangelog (forgetLedgerTables $ testInitLedger) genTestLedgerDbChangelogState

-- instance Arbitrary (DbChangelog l) where
--   arbitrary = genDbChangelog (forgetLedgerTables $ testInitLedger) genTestLedgerDbChangelogState
-- isSuffix :: AS.AnchoredSeq (DbChangelog l) -> AS.AnchoredSeq (DbChangelog l)
-- isSuffix = undefined


-- prop_pruneKeepsStates ::
--   SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
-- prop_pruneKeepsStates sp dblog = property $ AS.unsafeJoin imm vol == AS.unsafeJoin imm' vol'
--   where imm = changelogImmutableStates dblog
--         vol = changelogVolatileStates dblog
--         dblog' = pruneVolatilePartDbChangelog sp dblog
--         imm' = changelogImmutableStates dblog'
--         vol' = changelogVolatileStates dblog'

-- -- TODO: Whether this checks anything might depend too much on the distribution of maxRollbacks
-- prop_pruneKeepsAtMostMaxRollbacksVolatileStates ::
--   SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
-- prop_pruneKeepsAtMostMaxRollbacksVolatileStates (sp@SecurityParam { maxRollbacks }) dblog =
--   property $ fromIntegral (AS.length vol') <= maxRollbacks
--   where DbChangelog { changelogVolatileStates = vol' } = pruneVolatilePartDbChangelog sp dblog

-- prop_pruneKeepsInvariants ::
--   SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
-- prop_pruneKeepsInvariants sp dblog =
--   property $ checkInvariants $ pruneVolatilePartDbChangelog sp dblog

-- prop_flushingKeepsInvariants :: DbChangelog (LedgerState TestBlock) -> Property
-- prop_flushingKeepsInvariants dblog =
--   let (toFlush, toKeep) = flushDbChangelog DbChangelogFlushAllImmutable dblog
--   in property $ checkInvariants toFlush && checkInvariants toKeep


-- prop_extendDbChangelogKeepsImmutableStates :: Int -> Property
-- prop_extendDbChangelogKeepsImmutableStates i = undefined

-- prop_pruneTrimsVolatile :: SecurityParam -> DbChangelog (LedgerState TestBlock) -> Property
-- prop_pruneTrimsVolatile sp log = vol' AS.isPrefixOf vol
--   where log' = pruneVolatilePartDbChangelog sp log
--         vol = changelogVolatileStates log
--         vol' = changelogVolatileStates log'


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

-- mk :: Type -> Type -> Type
data MinLedger (mk :: MapKind)
  = MinLedger { unMinLedger :: ApplyMapKind mk Char Int, pt :: Point (MinLedger EmptyMK) }

instance Show (MinLedger mk) where
  show _ = "MinLedger mk"

instance GetTip (MinLedger EmptyMK) where
  getTip = pt

data H = H deriving (Eq, Ord, Show, Generic)
deriving anyclass instance NoThunks H
type instance HeaderHash (MinLedger EmptyMK) = H

instance StandardHash (MinLedger EmptyMK)

deriving instance Eq (MinLedger EmptyMK)
deriving instance Eq (LedgerTables MinLedger DiffMK)
deriving instance Eq (LedgerTables MinLedger ValuesMK)

instance ShowLedgerState (LedgerTables MinLedger) where
  showsLedgerState _ (T t) = showString "T " . shows t

instance Show (ApplyMapKind' mk' Char Int) where
  show ap = showsApplyMapKind ap ""

instance ShowLedgerState MinLedger where
  showsLedgerState _ (MinLedger _ _) = showString "L"

instance TableStuff MinLedger where
  data LedgerTables MinLedger mk = T { unT :: ApplyMapKind mk Char Int }
  projectLedgerTables = T . unMinLedger
  -- withLedgerTables :: IsApplyMapKind mk => l any -> LedgerTables l mk -> l mk
  withLedgerTables st (T x) = st { unMinLedger = x }
  pureLedgerTables = T
  mapLedgerTables f (T x) = T (f x)
  traverseLedgerTables f (T x) = T <$> f x
  zipLedgerTables f (T x) (T y) = T (f x y)
  zipLedgerTables2 f (T x) (T y) (T z) = T (f x y z)
  zipLedgerTablesA f (T x) (T y) = T <$> f x y
  zipLedgerTables2A f (T x) (T y) (T z) = T <$> f x y z
  foldLedgerTables f (T x) = f x
  foldLedgerTables2 f (T x) (T y) = f x y
  namesLedgerTables = T $ NameMK "T"


example' :: DbChangelog MinLedger
example' = extendDbChangelog initDbChangelog (MinLedger {unMinLedger = diff, pt = point})
  where point = Point $ At $ Point.Block 2 H
        diff = ApplyDiffMK $ UtxoDiff $ Map.empty


-- instance Show (LedgerTables MinLedger mk) where
--   show (T m) = "Bla" ++ show m

run = do
  sample $ (arbitrary :: Gen (DbChangelog MinLedger))

-- the thing you push is at the tip of the changelog
