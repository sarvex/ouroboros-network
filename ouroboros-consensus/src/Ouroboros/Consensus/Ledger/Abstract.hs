{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Type-level validation marker
    Validated
    -- * Apply block
  , ApplyBlock (..)
  , UpdateLedger
    -- * Derived
  , applyLedgerBlock
  , foldLedger
  , reapplyLedgerBlock
  , refoldLedger
  , tickThenApply
  , tickThenApplyLedgerResult
  , tickThenReapply
  , tickThenReapplyLedgerResult
    -- ** Short-hand
  , ledgerTipHash
  , ledgerTipPoint
  , ledgerTipSlot
    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Basics
  ) where

import           Control.Monad.Except
import           Data.Kind (Type)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Convenience
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM, (..:))
import           Data.Map.Diff.Strict
-- | " Validated " transaction or block
--
-- The ledger defines how to validate transactions and blocks. It's possible the
-- type before and after validation may be distinct (eg Alonzo transactions),
-- which originally motivated this family.
--
-- We also gain the related benefit that certain interface functions, such as
-- those that /reapply/ blocks, can have a more precise type now. TODO
--
-- Similarly, the Node-to-Client mini protocols can explicitly indicate that the
-- client trusts the blocks from the local server, by having the server send
-- 'Validated' blocks to the client. TODO
--
-- Note that validation has different implications for a transaction than for a
-- block. In particular, a validated transaction can be " reapplied " to
-- different ledger states, whereas a validated block must only be " reapplied "
-- to the exact same ledger state (eg as part of rebuilding from an on-disk
-- ledger snapshot).
--
-- Since the ledger defines validation, see the ledger details for concrete
-- examples of what determines the validity (wrt to a 'LedgerState') of a
-- transaction and/or block. Example properties include: a transaction's claimed
-- inputs exist and are still unspent, a block carries a sufficient
-- cryptographic signature, etc.
data family Validated x :: Type

{-------------------------------------------------------------------------------
  Apply block to ledger state
-------------------------------------------------------------------------------}

-- | @ApplyBlock@ is parametrized by both @l@ and @blk@ because for each @blk@
-- we have at least @LedgerState blk@ and @ExtLedgerState blk@.
class ( IsLedger l
      , HeaderHash l ~ HeaderHash blk
      , HasHeader blk
      , HasHeader (Header blk)
      ) => ApplyBlock l blk where

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked with the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  applyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked2 l ValuesMK EmptyMK
    -> Except (LedgerErr l) (LedgerResult l (l DiffMK EmptyMK))

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked2 l ValuesMK EmptyMK
    -> LedgerResult l (l DiffMK EmptyMK)

  -- | Given a block, get the key-sets that we need to apply it to a ledger
  -- state.
  getBlockKeySets :: blk -> LedgerTables l KeysMK EmptyMK

-- | Interaction with the ledger layer
class (ApplyBlock (LedgerState blk) blk, TickedTableStuff (LedgerState blk)) => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

{-| The following diagram describes the current flow during block ticking and
  application. The two provided arguments are the Block and the LedgerDB:


@
Key:  ┏━━━━━━━━━━━━┓  ╔════════════╗  ╔════════════╗
      ┃  FUNCTION  ┃  ║   LEDGER   ║  ║   LEDGER   ║╗
      ┗━━━━━━━━━━━━┛  ║   STATE    ║  ║   TABLES   ║║
                      ╚════════════╝  ╚════════════╝║
                                       ╚════════════╝
@

@
                  ┌────────────────┐                      │
                  ▼                │                      │
            ╔══════════╗           │                      ▼
            ║          ║           │                              l Empty
            ║ l Empty  ║           └──────────────    LedgerDB   ◄────────────────────────────────┐
            ║          ║                                                                          │
            ╚══════════╝                                                                          │
                  │                                                                               │
                  ▼                                                                               │
                 ┌─┐                                                                              │
      ┌──────────│↣│                                                                              │
      │          └─┘                                                                              │
      │           │                                                                               │
      │           ▼                                                                               │forget
      │     ╔══════════╗                         Ticked l Values                            ╔══════════╗
      │     ║          ║                    ┌──┐          ┌───────────┐           ┌───┐     ║          ║
      │     ║ l Values ║───────────────────▶│◁*│─────────▶│   apply   │──────────▶│◁*+│─────║ l Diffs  ║
      │     ║          ║                    └──┘          └───────────┘ l Diffs   └───┘     ║          ║
      │     ╚══════════╝                     ▲                     ▲                ▲       ╚══════════╝
      │           │                          │                     │                │             │
      │        ┌───┐                         │                     │                │             │
      │        │||∅│                         │                     │                │             │
 ╔════════╗    └───┘        ┌───────────┐    │                     \                │             │
 ║        ║╗      └────────▶│  ticking  │────┴──────────────────────────────────────┘             │
 ║ values ║║        l Empty └───────────┘ Ticked l Diff            /                              │
 ║        ║║                                                       │                              │
 ╚════════╝║                                                       │                              │
  ╚════════╝                                                       │                              │
      │    readDB                                                  \                              │
      │◄──────────────────────── DbChangelog + BackendStore ◄─────────────────────────────────────┘
      │                                                            /
 ╔════════╗                                                        │
 ║        ║╗                                                       │
 ║  keys  ║║                                                       │
 ║        ║║                                                       │
 ╚════════╝║                                                       │
  ╚════════╝                                                       │
      │ getNeededTxInputs                                          │
      │                                                            │
  ┌───┴───┐                                                        │
  │       │                                                        │
  │ Block ├────────────────────────────────────────────────────────┘
  │       │
  └───▲───┘
      │
@

In particular:
- ↣ is @withLedgerTables@
- ◁ is @applyDiffsLedgerTables@
- <> is @prependDiffs@

TODO: Elaborate more this comment

-}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked2 l ValuesMK EmptyMK
  -> Except (LedgerErr l) (l DiffMK EmptyMK)
applyLedgerBlock = fmap lrResult ..: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked2 l ValuesMK EmptyMK
  -> l DiffMK EmptyMK
reapplyLedgerBlock = lrResult ..: reapplyBlockLedgerResult

forgetLedgerTables1 :: forall (l :: LedgerStateKind) mk mk2. TableStuff l => l mk mk2 -> l EmptyMK mk2
forgetLedgerTables1 = mapOverLedgerTables @l (\_ -> ApplyEmptyMK) id

forgetLedgerTables2 :: forall (l :: LedgerStateKind) mk1 mk. TableStuff l => l mk1 mk -> l mk1 EmptyMK
forgetLedgerTables2 = mapOverLedgerTables @l id (\_ -> ApplyEmptyMK)

forgetLedgerTablesTicked1 :: forall (l :: LedgerStateKind) mk mk2. TickedTableStuff l => Ticked2 l mk mk2 -> Ticked2 l EmptyMK mk2
forgetLedgerTablesTicked1 = mapOverLedgerTablesTicked @l (\_ -> ApplyEmptyMK) id

forgetLedgerTablesTicked2 :: forall (l :: LedgerStateKind) mk1 mk. TickedTableStuff l => Ticked2 l mk1 mk -> Ticked2 l mk1 EmptyMK
forgetLedgerTablesTicked2 = mapOverLedgerTablesTicked @l id (\_ -> ApplyEmptyMK)

applyDiffsTicking :: forall (l :: LedgerStateKind) mk1 mk. TickedTableStuff l => l ValuesMK mk -> Ticked2 l DiffMK mk -> Ticked2 l ValuesMK mk
applyDiffsTicking = flip (zipOverLedgerTablesTicked @l (\(ApplyValuesMK v) (ApplyDiffMK d) -> ApplyValuesMK (applyDiff v d)) (\x _ -> x)) . projectLedgerTables

tickThenApplyLedgerResult ::
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l
  -> blk
  -> l ValuesMK ValuesMK
  -> Except (LedgerErr l) (LedgerResult l (l DiffMK DiffMK))
tickThenApplyLedgerResult cfg blk l = do
  let lrTick = applyChainTickLedgerResult cfg (blockSlot blk) (forgetLedgerTables1 l)
  lrBlock <-   applyBlockLedgerResult     cfg            blk  (applyDiffsTicking (forgetLedgerTables2 l) (fmap forgetLedgerTablesTicked2 lrResult lrTick))
  pure LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = prependDiffsTicked1 (lrResult lrTick) (lrResult lrBlock)
    }

prependDiffsTicked1 :: forall (l :: LedgerStateKind). TickedTableStuff l
  => Ticked2 l DiffMK DiffMK
  -> l DiffMK EmptyMK -- ^ keep this one
  -> l DiffMK DiffMK
prependDiffsTicked1 = flip (zipOverLedgerTables @l (\(ApplyDiffMK d1) (ApplyDiffMK d2) -> ApplyDiffMK (d1 <> d2)) (\_ tbs -> tbs)) . projectLedgerTablesTicked

tickThenReapplyLedgerResult :: forall l blk.
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l
  -> blk
  -> l ValuesMK ValuesMK
  -> LedgerResult l (l DiffMK DiffMK)
tickThenReapplyLedgerResult cfg blk l =
  let lrTick    = applyChainTickLedgerResult cfg (blockSlot blk) (forgetLedgerTables1 l)
      lrBlock   = reapplyBlockLedgerResult   cfg            blk  (applyDiffsTicking (forgetLedgerTables2 l) (fmap forgetLedgerTablesTicked2 lrResult lrTick))
  in LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = prependDiffsTicked1 (lrResult lrTick) (lrResult lrBlock)
    }

tickThenApply ::
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l
  -> blk
  -> l ValuesMK ValuesMK
  -> Except (LedgerErr l) (l DiffMK DiffMK)
tickThenApply = fmap lrResult ..: tickThenApplyLedgerResult

tickThenReapply ::
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l
  -> blk
  -> l ValuesMK ValuesMK
  -> l DiffMK DiffMK
tickThenReapply = lrResult ..: tickThenReapplyLedgerResult

foldLedger ::
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l -> [blk] -> l ValuesMK ValuesMK -> Except (LedgerErr l) (l ValuesMK ValuesMK)
foldLedger cfg = repeatedlyM (\blk state -> fmap (applyBoth state) $ tickThenApply cfg blk state)

applyBoth ::
  forall l. TableStuff l => l ValuesMK ValuesMK -> l DiffMK DiffMK -> l ValuesMK ValuesMK
applyBoth before after = zipOverLedgerTables @l (\(ApplyDiffMK d) (ApplyValuesMK v) -> ApplyValuesMK (applyDiff v d)) (\(ApplyDiffMK d) (ApplyValuesMK v) -> ApplyValuesMK (applyDiff v d)) after (projectLedgerTables before)

refoldLedger ::
     (ApplyBlock l blk, TickedTableStuff l)
  => LedgerCfg l -> [blk] -> l ValuesMK ValuesMK -> l ValuesMK ValuesMK
refoldLedger cfg = repeatedly (\blk state -> applyBoth state $ tickThenReapply cfg blk state)

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
     UpdateLedger blk
  => LedgerState blk mk1 mk2 -> Point blk
ledgerTipPoint = castPoint . getTip

ledgerTipHash ::
     UpdateLedger blk
  => LedgerState blk mk1 mk2 -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
     UpdateLedger blk
  => LedgerState blk mk1 mk2 -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
