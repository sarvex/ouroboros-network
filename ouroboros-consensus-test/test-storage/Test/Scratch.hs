{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Scratch () where

import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Network.AnchoredSeq (AnchoredSeq ((:>)))
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Test.Ouroboros.Storage.LedgerDB.DbChangelog
import           Test.QuickCheck
import           Test.Util.TestBlock




-- newtype X = X Int deriving (Eq, Ord, Bounded, Show)

-- instance AS.Anchorable X X X where
--   asAnchor = id
--   getAnchorMeasure _ x = x

-- -- example :: AS.AnchoredSeq X X X
-- -- example = AS.Empty (X 1) :> X 2 :> X 3 :> X 4


-- result :: Maybe (AnchoredSeq X X X)
-- result = AS.rollback (X 2) p example
--   where p x = either id id x == X 2


-- mkDblog :: IO (DbChangelog (LedgerState TestBlock))
-- mkDblog = generate arbitrary


-- display :: DbChangelog (LedgerState TestBlock) -> IO ()
-- display DbChangelog { changelogImmutableStates = imm, changelogDiffs = diffs } = do
--   print diffs

-- scratch :: IO ()
-- scratch = mkDblog >>= display


