{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bench.Consensus.Mempool.Scenario (
    -- * Scenario
    Scenario
  , fromScenario
    -- * Scenario builder
  , ScBuilder
  , build
  , finishScenario
    -- ** Primitives
  , gtx1
  , stx1
    -- ** Combinators
  , linkedTxs
  ) where

import           Bench.Consensus.Mempool (MempoolCmd, mkSimpleTryAdd)
import           Bench.Consensus.Mempool.Params (InitialMempoolAndModelParams,
                     ledgerStateFromTokens, mkParams, testBlocksFromTxs)
import           Bench.Consensus.Mempool.TestBlock (GenTx (TestBlockGenTx),
                     TestBlock, Token, Tx (..), mkTx)
import           Control.Exception (assert)
import           Control.Monad.State.Strict (MonadState, State, StateT (StateT),
                     execState, gets, modify, replicateM)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector (..))
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
                     (LedgerDbCfg (..))

{-------------------------------------------------------------------------------
  Scenario
-------------------------------------------------------------------------------}

-- | A benchmark scenario, see 'InitialMempoolAndModelParams'.
data Scenario = Scenario {
    -- | 'Token's to be included in the backing store.
    scBackingTokens   :: ![Token]
    -- | 'TestBlock's to be included in the changelog.
  , scChangelogBlocks :: ![TestBlock]
    -- | Mempool commands to run.
  , scMempoolCommands :: ![MempoolCmd TestBlock]
  }

-- | Convert a 'Scenario' into benchmark parameters and a benchmark workload
-- (commands).
fromScenario ::
     LedgerDbCfg (LedgerState TestBlock)
  -> BackingStoreSelector m
  -> Scenario
  -> (InitialMempoolAndModelParams m TestBlock, [MempoolCmd TestBlock])
fromScenario ldbCfg bss sc =
  (mkParams
    (ledgerStateFromTokens $ scBackingTokens sc)
    (scChangelogBlocks sc)
    ldbCfg
    bss
  , scMempoolCommands sc
  )

{-------------------------------------------------------------------------------
  Scenario builder
-------------------------------------------------------------------------------}

-- | A monadic interface to constructing benchmark scenarios.
newtype ScBuilder a = ScBuilder { runScBuilder :: State St a }
  deriving newtype (Functor, Applicative, Monad, MonadState St)

-- | Internal state for the 'ScenarioBuilder' monad.
data St = St {
    -- | A new, unique 'Token'. Reading this field should always be followed by
    -- an increment.
    stNextToken :: !Token
    -- | The scenario being built.
  , stScenario  :: !Scenario
  }

initialSt :: St
initialSt = St 0 (Scenario [] [] [])

execScBuilder :: ScBuilder a -> St -> St
execScBuilder = execState . runScBuilder

build :: ScBuilder () -> Scenario
build m = stScenario (execScBuilder m initialSt)

-- | Finish the 'Scenario' under construction by populating it with 'Tx's.
finishScenario ::
     MonadState St m
  => [Tx] -- ^ 'Tx's to put in the backing store
  -> [Tx] -- ^ 'Tx's to put in the changelog (each is converted to a single
          --   block)
  -> [Tx] -- ^ 'Tx's to create mempool commands from (each is converted to a
          --   single command)
  -> m ()
finishScenario bsTxs clTxs mTxs = do
  putBackingTxs bsTxs
  putChangelogBlocks $ testBlocksFromTxs clTxs
  putMempoolCommands $ fmap (mkSimpleTryAdd . TestBlockGenTx) mTxs

{-------------------------------------------------------------------------------
  Scenario builder: 'MonadState' extras
-------------------------------------------------------------------------------}

modifyScenario :: MonadState St m => (Scenario -> Scenario) -> m ()
modifyScenario f = modify (\st -> st {
      stScenario = f $ stScenario st
    })

modifyBackingTokens :: MonadState St m => ([Token] -> [Token]) -> m ()
modifyBackingTokens f = modifyScenario (\sc -> sc {
      scBackingTokens = f $ scBackingTokens sc
    })

putBackingTxs :: MonadState St m => [Tx] -> m ()
putBackingTxs txs =
    let
      fullProduced = foldMap produced txs
      fullConsumed = foldMap consumed txs
    in
        assert (Set.null fullConsumed)
      $ modifyBackingTokens (const $ Set.toList fullProduced)

modifyChangelogBlocks :: MonadState St m => ([TestBlock] -> [TestBlock]) -> m ()
modifyChangelogBlocks f = modifyScenario (\sc -> sc {
      scChangelogBlocks = f $ scChangelogBlocks sc
    })

putChangelogBlocks :: MonadState St m => [TestBlock] -> m ()
putChangelogBlocks blks = modifyChangelogBlocks (const blks)

modifyMempoolCommands ::
     MonadState St m
  => ([MempoolCmd TestBlock] -> [MempoolCmd TestBlock]) -> m ()
modifyMempoolCommands f = modifyScenario (\sc -> sc {
      scMempoolCommands = f $ scMempoolCommands sc
    })

putMempoolCommands :: MonadState St m => [MempoolCmd TestBlock] -> m ()
putMempoolCommands cmds = modifyMempoolCommands (const cmds)

{-------------------------------------------------------------------------------
  Scenario builder: utilities
-------------------------------------------------------------------------------}

-- | Monadic equivalent to 'iterate'. The 'Int' parameter ensures termination.
--
-- Note: based on 'iterateM' from @monad-extras-0.6.0@.
iterateM :: forall m a. Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateM n0 f xm0
  | n0 <= 0   = error "iterateM: n <= 0"
  | otherwise = go n0 xm0
  where
    go :: Int -> m a -> m [a]
    go 0 _  = pure []
    go n xm = do
      x <- xm
      (x:) <$> go (n-1) (f x)

{-------------------------------------------------------------------------------
  Scenario builder: primitives
-------------------------------------------------------------------------------}

newToken :: MonadState St m => m Token
newToken = do
  nxtTok <- gets stNextToken
  modify (\st -> st { stNextToken = nxtTok + 1 })
  pure nxtTok

-- | @'txN' txsTC n@ creates a 'Tx' that consumes all 'Token's
-- produced by the 'Tx's in @txsTC@, and produces @n@ new 'Token's.
txN :: MonadState St m => [Tx] -> Int -> m Tx
txN txsTC n = do
  let toksTC = Set.toList $ foldMap produced txsTC
  toksTP <- replicateM n newToken
  pure $ mkTx toksTC toksTP

-- | @'tx1' txTC n@ creates a 'Tx' that consumes all 'Token's
-- produced by @txTC@, and produces @n@ new 'Token's.
_tx1 :: MonadState St m => Tx -> Int -> m Tx
_tx1 txTC = txN [txTC]

-- | @'stxN' txsTC@ creates a 'Tx' that consumes all 'Token's produced by the
-- 'Tx's in @txsTC@, and produces only @1@ new 'Token'.
_stxN :: MonadState St m => [Tx] -> m Tx
_stxN txTC = txN txTC 1

-- | @'stx1' txTC@ creates a 'Tx' that consumes all 'Token's produced
-- by @txTC@, and produces only @1@ new 'Token'.
stx1 :: MonadState St m => Tx -> m Tx
stx1 txTC = txN [txTC] 1

-- | @'gtxN' txTC@ creates a 'Tx' that consumes no 'Token's, and produces @n@
-- new 'Token's.
_gtxN :: MonadState St m => Int -> m Tx
_gtxN = txN []

-- | @'gtx1' txTC@ creates a 'Tx' that consumes no 'Token's, and produces only
-- @1@ new 'Token's.
gtx1 :: MonadState St m => m Tx
gtx1 = txN [] 1

{-------------------------------------------------------------------------------
  Scenario builder: combinators
-------------------------------------------------------------------------------}

-- | @'linkedTxs' prevMay n@ creates a list of linked 'Tx's, where each 'Tx'
-- consumes tokens that the previous 'Tx' produces.
--
-- >    linkedTxs (Just $ mkTx [0] [1]) 3
-- > =~ [mkTx [1] [2], mkTx [2] [3], mkTx [4] [5]]
--
-- >    linkedTxs Nothing 2
-- > =~ [mkTx [] [1], 'mkTx' [1] [2]]
linkedTxs ::
     forall m. MonadState St m
  => Maybe Tx -- ^ A possible 'Tx' to link the first 'Tx' to.
  -> Int
  -> m [Tx]
linkedTxs _ 0         = pure []
linkedTxs prevtxMay n = iterateM n stx1 $ maybe gtx1 stx1 prevtxMay
