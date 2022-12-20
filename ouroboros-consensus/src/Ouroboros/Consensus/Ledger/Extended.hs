{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ExtValidationError (..)
    -- * Serialisation
  , decodeExtLedgerState
  , encodeExtLedgerState
    -- * Casts
  , castExtLedgerState
    -- * Type family instances
  , LedgerTables (..)
  , Ticked1 (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Show (showCommaSpace, showSpace)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import Data.Map.Diff.Strict

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk (mk1 :: MapKind) (mk2 :: MapKind) = ExtLedgerState {
      ledgerState :: !(LedgerState blk mk1 mk2)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

instance InMemory (LedgerState blk) => InMemory (ExtLedgerState blk) where
  convertMapKind est =
    ExtLedgerState {
        ledgerState = convertMapKind $ ledgerState est
      , headerState = headerState est
      }

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)

-- TODO this might be wrong but I'd like to understand why.
deriving instance (Show (LedgerState blk mk1 mk2), Show (HeaderState blk))
  => Show (ExtLedgerState blk mk1 mk2)

deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError    blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError    blk)

instance LedgerSupportsProtocol blk => ShowLedgerState (ExtLedgerState blk) where
  showsLedgerState mk st =
      showParen True $ showString "ExtLedgerState {"
        . showSpace      . showString "headerState = " . shows               headerState
        . showCommaSpace . showString "ledgerState = " . showsLedgerState mk ledgerState
        . showString " }"
    where
      ExtLedgerState _dummy _ = st
      ExtLedgerState {
          headerState
        , ledgerState
        } = st

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance (Typeable mk1, Typeable mk2, LedgerSupportsProtocol blk, NoThunks (LedgerState blk mk1 mk2)) => NoThunks (ExtLedgerState blk mk1 mk2) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk mk1 mk2))

deriving instance ( LedgerSupportsProtocol blk
                  , Eq (ChainDepState (BlockProtocol blk))
                  , Eq (LedgerState blk mk1 mk2)
                  ) => Eq (ExtLedgerState blk mk1 mk2)

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

data instance Ticked2 (ExtLedgerState blk) mk1 mk2 = TickedExtLedgerState {
      tickedLedgerState :: Ticked2 (LedgerState blk) mk1 mk2
    , tickedLedgerView  :: Ticked (LedgerView (BlockProtocol blk))
    , tickedHeaderState :: Ticked (HeaderState blk)
    }

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
newtype ExtLedgerCfg blk = ExtLedgerCfg {
      getExtLedgerCfg :: TopLevelConfig blk
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoThunks (BlockConfig   blk)
         , NoThunks (CodecConfig   blk)
         , NoThunks (LedgerConfig  blk)
         , NoThunks (StorageConfig blk)
         ) => NoThunks (ExtLedgerCfg blk)

type instance LedgerCfg (ExtLedgerState blk) = ExtLedgerCfg blk

type instance HeaderHash (ExtLedgerState blk)    = HeaderHash (LedgerState blk)
type instance HeaderHash (ExtLedgerState blk mk1 mk2) = HeaderHash (LedgerState blk mk1 mk2)

instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk)
instance StandardHash (LedgerState blk) => StandardHash (ExtLedgerState blk mk1 mk2)

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk mk1 mk2) where
  getTip = castPoint . getTip . ledgerState

instance IsLedger (LedgerState blk) => GetTip (Ticked2 (ExtLedgerState blk) mk1 mk2) where
  getTip = castPoint . getTip . tickedLedgerState

instance ( IsLedger (LedgerState  blk)
         , LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  type AuxLedgerEvent (ExtLedgerState blk) = AuxLedgerEvent (LedgerState blk)

  applyChainTickLedgerResult cfg slot (ExtLedgerState ledger header) =
      castLedgerResult ledgerResult <&> \tickedLedgerState ->
      let tickedLedgerView :: Ticked (LedgerView (BlockProtocol blk))
          tickedLedgerView = protocolLedgerView lcfg $ applyDiff2 ledger tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                tickedLedgerView
                slot
                header
      in TickedExtLedgerState {..}
    where
      lcfg :: LedgerConfig blk
      lcfg = configLedger $ getExtLedgerCfg cfg

      ledgerResult = applyChainTickLedgerResult lcfg slot ledger

applyDiff2 :: forall blk. TickedTableStuff (LedgerState blk) => LedgerState blk EmptyMK ValuesMK
           -> TickedLedgerState blk DiffMK DiffMK
           -> TickedLedgerState blk DiffMK ValuesMK
applyDiff2 before after = zipOverLedgerTablesTicked @(LedgerState blk) const (\(ApplyDiffMK d) (ApplyValuesMK v) -> ApplyValuesMK $ applyDiff v d) after (projectLedgerTables before)


instance (LedgerSupportsProtocol blk, TableStuff (LedgerState blk)) => TableStuff (ExtLedgerState blk) where

  newtype LedgerTables (ExtLedgerState blk) mk1 mk2 =
    ExtLedgerStateTables { unExtLedgerStateTables :: LedgerTables (LedgerState blk) mk1 mk2 }
    deriving (Generic)

  projectLedgerTables (ExtLedgerState lstate _) =
      ExtLedgerStateTables (projectLedgerTables lstate)
  withLedgerTables (ExtLedgerState lstate hstate) (ExtLedgerStateTables tables) =
      ExtLedgerState (lstate `withLedgerTables` tables) hstate

  traverseLedgerTables f g (ExtLedgerStateTables l) =
    ExtLedgerStateTables <$> traverseLedgerTables f g l

  pureLedgerTables  f g = coerce $ pureLedgerTables  @(LedgerState blk) f g
  mapLedgerTables   f g = coerce $ mapLedgerTables   @(LedgerState blk) f g
  zipLedgerTables   f g = coerce $ zipLedgerTables   @(LedgerState blk) f g
  zipLedgerTables2  f g = coerce $ zipLedgerTables2  @(LedgerState blk) f g
  foldLedgerTables  f g = coerce $ foldLedgerTables  @(LedgerState blk) f g
  foldLedgerTables2 f g = coerce $ foldLedgerTables2 @(LedgerState blk) f g
  namesLedgerTables   = coerce $ namesLedgerTables @(LedgerState blk)
  zipLedgerTablesA  f g (ExtLedgerStateTables l) (ExtLedgerStateTables r) =
    ExtLedgerStateTables <$> zipLedgerTablesA f g l r
  zipLedgerTables2A  f g (ExtLedgerStateTables l) (ExtLedgerStateTables c) (ExtLedgerStateTables r) =
     ExtLedgerStateTables <$> zipLedgerTables2A f g l c r

instance ( LedgerSupportsProtocol blk
         , SufficientSerializationForAnyBackingStore (LedgerState blk)
         )
      => SufficientSerializationForAnyBackingStore (ExtLedgerState blk) where
  codecLedgerTables = ExtLedgerStateTables codecLedgerTables

deriving instance ShowLedgerState (LedgerTables (LedgerState blk)) => ShowLedgerState (LedgerTables (ExtLedgerState blk))

deriving instance Eq        (LedgerTables (LedgerState blk) mk1 mk2)               => Eq       (LedgerTables (ExtLedgerState blk) mk1 mk2)
instance (NoThunks (LedgerTables (LedgerState blk) mk1 mk2), Typeable mk1, Typeable mk2) => NoThunks (LedgerTables (ExtLedgerState blk) mk1 mk2)

instance InMemory (LedgerTables (LedgerState blk)) => InMemory (LedgerTables (ExtLedgerState blk)) where
  convertMapKind (ExtLedgerStateTables st) =
      ExtLedgerStateTables $ convertMapKind st

instance (LedgerSupportsProtocol blk, TickedTableStuff (LedgerState blk)) => TickedTableStuff (ExtLedgerState blk) where
  projectLedgerTablesTicked (TickedExtLedgerState lstate _view _hstate) =
      ExtLedgerStateTables (projectLedgerTablesTicked lstate)
  withLedgerTablesTicked
    (TickedExtLedgerState lstate view hstate)
    (ExtLedgerStateTables tables) =
      TickedExtLedgerState (lstate `withLedgerTablesTicked` tables) view hstate

instance (LedgerSupportsProtocol blk) => ApplyBlock (ExtLedgerState blk) blk where
  applyBlockLedgerResult cfg blk TickedExtLedgerState{..} = do
    ledgerResult <-
        withExcept ExtValidationErrorLedger
      $ applyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          tickedLedgerState
    hdr <-
        withExcept ExtValidationErrorHeader
      $ validateHeader @blk
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState
    pure $ (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult

  reapplyBlockLedgerResult cfg blk TickedExtLedgerState{..} =
      (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult
    where
      ledgerResult =
        reapplyBlockLedgerResult
          (configLedger $ getExtLedgerCfg cfg)
          blk
          tickedLedgerState
      hdr      =
        revalidateHeader
          (getExtLedgerCfg cfg)
          tickedLedgerView
          (getHeader blk)
          tickedHeaderState

  getBlockKeySets = ExtLedgerStateTables . getBlockKeySets

instance
     ( LedgerSupportsProtocol blk
     , StowableLedgerTables (LedgerState blk)
     )
  => StowableLedgerTables (ExtLedgerState blk) where

  stowLedgerTables1 ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = stowLedgerTables1 ledgerState
        }

  unstowLedgerTables1 ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = unstowLedgerTables1 ledgerState
        }

  stowLedgerTables2 ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = stowLedgerTables2 ledgerState
        }

  unstowLedgerTables2 ExtLedgerState{headerState, ledgerState} =
      ExtLedgerState {
          headerState
        , ledgerState = unstowLedgerTables2 ledgerState
        }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk mk1 mk2 -> Encoding)
                     -> (ChainDepState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk    -> Encoding)
                     -> ExtLedgerState blk mk1 mk2 -> Encoding
encodeExtLedgerState encodeLedgerState
                     encodeChainDepState
                     encodeAnnTip
                     ExtLedgerState{..} = mconcat [
      encodeLedgerState  ledgerState
    , encodeHeaderState' headerState
    ]
  where
    encodeHeaderState' = encodeHeaderState
                           encodeChainDepState
                           encodeAnnTip

decodeExtLedgerState :: (forall s. Decoder s (LedgerState    blk mk1 mk2))
                     -> (forall s. Decoder s (ChainDepState  (BlockProtocol blk)))
                     -> (forall s. Decoder s (AnnTip         blk))
                     -> (forall s. Decoder s (ExtLedgerState blk mk1 mk2))
decodeExtLedgerState decodeLedgerState
                     decodeChainDepState
                     decodeAnnTip = do
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    return ExtLedgerState{..}
  where
    decodeHeaderState' = decodeHeaderState
                           decodeChainDepState
                           decodeAnnTip

{-------------------------------------------------------------------------------
  Casts
-------------------------------------------------------------------------------}

castExtLedgerState
  :: ( Coercible (LedgerState blk  mk1 mk2)
                 (LedgerState blk' mk1 mk2)
     , Coercible (ChainDepState (BlockProtocol blk))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => ExtLedgerState blk mk1 mk2 -> ExtLedgerState blk' mk1 mk2
castExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = coerce ledgerState
    , headerState = castHeaderState headerState
    }
