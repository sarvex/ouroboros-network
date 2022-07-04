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

module Test.Ouroboros.Storage.LedgerDB.DbChangelog (tests) where

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
      ]

  ]

-- | Invariants

volatileTipAnchorsImmutable :: (GetTip (l EmptyMK), Eq (l EmptyMK)) => DbChangelog l -> Bool
volatileTipAnchorsImmutable DbChangelog { changelogImmutableStates, changelogVolatileStates } =
  AS.anchor changelogVolatileStates == AS.headAnchor changelogImmutableStates

immutableAnchored :: DbChangelog TestLedger -> Bool
immutableAnchored DbChangelog { changelogDiffAnchor, changelogImmutableStates } =
  changelogDiffAnchor == fmap Point.blockPointSlot point
  where point = getPoint $ pt $ unDbChangelogState $ AS.anchor $ changelogImmutableStates

sameNumberOfDiffsAsStates :: DbChangelog TestLedger -> Bool
sameNumberOfDiffsAsStates dblog = AS.length imm + AS.length vol == count diffs
  where imm = changelogImmutableStates dblog
        vol = changelogVolatileStates dblog
        ApplySeqDiffMK (SeqUtxoDiff diffs) = unTestLedger $ changelogDiffs dblog
        count = foldr (const (+ 1)) 0

checkInvariants :: DbChangelog TestLedger -> Bool
checkInvariants dblog = volatileTipAnchorsImmutable dblog &&
                        immutableAnchored dblog &&
                        sameNumberOfDiffsAsStates dblog

-- | Properties

prop_emptySatisfiesInvariants :: Property
prop_emptySatisfiesInvariants = property $ checkInvariants initDbChangelog

prop_generatedSatisfiesInvariants :: DbChangelog TestLedger -> Property
prop_generatedSatisfiesInvariants dblog = property $ checkInvariants dblog

-- | Generators

initDbChangelog :: DbChangelog TestLedger
initDbChangelog = emptyDbChangeLog anchor
  where anchor = TestLedger ApplyEmptyMK point
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

instance Arbitrary (HeaderHash blk) => Arbitrary (Point blk) where
  arbitrary = BlockPoint <$> (SlotNo <$> arbitrary) <*> arbitrary

instance Arbitrary (TestLedger EmptyMK) where
  arbitrary = TestLedger ApplyEmptyMK <$> arbitrary

instance Arbitrary (UtxoEntryDiff Int) where
  arbitrary = UtxoEntryDiff <$> arbitrary <*> diffState
    where diffState = oneof [pure UedsDel, pure UedsIns, pure UedsInsAndDel]

instance Arbitrary (TestLedger DiffMK) where
  arbitrary = TestLedger <$> diff <*> arbitrary
    where diff = ApplyDiffMK . UtxoDiff <$> arbitrary

data TestLedger (mk :: MapKind) = TestLedger {
  unTestLedger :: ApplyMapKind mk Char Int,
  pt           :: Point (TestLedger EmptyMK)
}

instance Show (TestLedger mk) where
  show _ = "TestLedger mk"

instance GetTip (TestLedger EmptyMK) where
  getTip = pt

data H = H deriving (Eq, Ord, Show, Generic)
deriving anyclass instance NoThunks H
type instance HeaderHash (TestLedger EmptyMK) = H

instance StandardHash (TestLedger EmptyMK)

deriving instance Eq (TestLedger EmptyMK)
deriving instance Eq (LedgerTables TestLedger DiffMK)
deriving instance Eq (LedgerTables TestLedger ValuesMK)

instance ShowLedgerState (LedgerTables TestLedger) where
  showsLedgerState _ (TestLedger t) = showString "TestLedger " . shows t

instance Show (ApplyMapKind' mk' Char Int) where
  show ap = showsApplyMapKind ap ""

instance ShowLedgerState TestLedger where
  showsLedgerState _ (TestLedger _ _) = showString "L"

instance TableStuff TestLedger where
  data LedgerTables TestLedger mk = TestLedger { unTestLedger :: ApplyMapKind mk Char Int }
  projectLedgerTables = TestLedger . unTestLedger
  withLedgerTables st (TestLedger x) = st { unTestLedger = x }
  pureLedgerTables = T
  mapLedgerTables f (TestLedger x) = TestLedger (f x)
  traverseLedgerTables f (TestLedger x) = TestLedger <$> f x
  zipLedgerTables f (TestLedger x) (TestLedger y) = TestLedger (f x y)
  zipLedgerTables2 f (TestLedger x) (TestLedger y) (TestLedger z) = TestLedger (f x y z)
  zipLedgerTablesA f (TestLedger x) (TestLedger y) = TestLedger <$> f x y
  zipLedgerTables2A f (TestLedger x) (TestLedger y) (TestLedger z) = TestLedger <$> f x y z
  foldLedgerTables f (TestLedger x) = f x
  foldLedgerTables2 f (TestLedger x) (TestLedger y) = f x y
  namesLedgerTables = TestLedger $ NameMK "T"

example :: DbChangelog TestLedger
example = extendDbChangelog initDbChangelog (TestLedger {unTestLedger = diff, pt = point})
  where point = Point $ At $ Point.Block 2 H
        diff = ApplyDiffMK $ UtxoDiff $ Map.empty

run = do
  sample $ (arbitrary :: Gen (DbChangelog TestLedger))
