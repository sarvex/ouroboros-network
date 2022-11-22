{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Consensus.Ledger.MapKind where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind
import           GHC.Generics
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

-- | This is for backwards compatibility
type ApplyMapKind (mk :: MapKind) = mk

type MapKind         = {- key -} Type -> {- value -} Type -> Type

{-------------------------------------------------------------------------------
  Individual MKs
-------------------------------------------------------------------------------}

type DiffMK :: MapKind
newtype DiffMK k v    = ApplyDiffMK (Diff k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NoThunks
instance IsMapKind DiffMK

type EmptyMK :: MapKind
data EmptyMK k v      = ApplyEmptyMK
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass NoThunks
instance IsMapKind EmptyMK where emptyMK = ApplyEmptyMK

type KeysMK :: MapKind
newtype KeysMK k v    = ApplyKeysMK (Keys k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NoThunks
instance IsMapKind KeysMK

type SeqDiffMK :: MapKind
newtype SeqDiffMK k v = ApplySeqDiffMK (DiffSeq k v)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks
instance IsMapKind SeqDiffMK where
  emptyMK = ApplySeqDiffMK empty
  mapMK f (ApplySeqDiffMK ds) = ApplySeqDiffMK $ mapDiffSeq f ds

type TrackingMK :: MapKind
data TrackingMK k v   = ApplyTrackingMK !(Values k v) !(Diff k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass NoThunks
instance IsMapKind TrackingMK where emptyMK = ApplyTrackingMK mempty mempty

type ValuesMK :: MapKind
newtype ValuesMK k v  = ApplyValuesMK (Values k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass NoThunks
instance IsMapKind ValuesMK

type QueryAllMK :: MapKind
data QueryAllMK k v = ApplyQueryAllMK
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass NoThunks

type QuerySomeMK :: MapKind
newtype QuerySomeMK k v = ApplyQuerySomeMK (Keys k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass NoThunks

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String

{-------------------------------------------------------------------------------
  Interface to MKs required for MK-polymorphic operations
-------------------------------------------------------------------------------}

class IsMapKind mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v
  default emptyMK :: forall k v. (Monoid (mk k v)) => mk k v
  emptyMK = mempty

  mapMK :: forall k v v'. (Ord k, Eq v, Eq v') => (v -> v') -> mk k v -> mk k v'
  default mapMK :: forall k v v'. (Functor (mk k)) => (v -> v') -> mk k v -> mk k v'
  mapMK = fmap

  showMK :: forall k v. (Show k, Show v) => mk k v -> String
  default showMK :: forall k v. Show (mk k v) => mk k v -> String
  showMK = show

  showsMK :: forall k v. (Show k, Show v) => mk k v -> ShowS
  default showsMK :: forall k v. Show (mk k v) => mk k v -> ShowS
  showsMK = shows

{-------------------------------------------------------------------------------
  Grouping base MKs
-------------------------------------------------------------------------------}

data BaseMK (mk :: MapKind) k v where
  BEmptyMK    :: EmptyMK k v    -> BaseMK EmptyMK k v
  BDiffMK     :: DiffMK k v     -> BaseMK DiffMK k v
  BKeysMK     :: KeysMK k v     -> BaseMK KeysMK k v
  BValuesMK   :: ValuesMK k v   -> BaseMK ValuesMK k v
  BTrackingMK :: TrackingMK k v -> BaseMK TrackingMK k v

fromBaseMK :: BaseMK mk k v -> mk k v
fromBaseMK (BEmptyMK e)     = e
fromBaseMK (BDiffMK d)      = d
fromBaseMK (BKeysMK ks)     = ks
fromBaseMK (BValuesMK vs)   = vs
fromBaseMK (BTrackingMK tr) = tr

deriving stock instance (Show k, Show v) => Show (BaseMK mk k v)
deriving stock instance (Eq k, Eq v) => Eq (BaseMK mk k v)
deriving stock instance Functor (BaseMK mk k)
deriving anyclass instance Semigroup (BaseMK mk k v)
deriving anyclass instance Monoid (BaseMK mk k v)

instance IsMapKind (BaseMK mk)

class IsBaseMK (mk :: MapKind) where
  constr :: forall k v. mk k v -> BaseMK mk k v

instance IsBaseMK EmptyMK where constr = BEmptyMK

{-------------------------------------------------------------------------------
  Grouping meta MKs
-------------------------------------------------------------------------------}

data MetaMK (mk :: MapKind) k v where
  MSeqDiffMK :: SeqDiffMK k v     -> MetaMK SeqDiffMK k v
  MQueryAllMK :: QueryAllMK k v   -> MetaMK QueryAllMK k v
  MQuerySomeMK :: QuerySomeMK k v -> MetaMK QuerySomeMK k v

fromMetaMK :: MetaMK mk k v -> mk k v
fromMetaMK (MSeqDiffMK ds)   = ds
fromMetaMK (MQueryAllMK qa)  = qa
fromMetaMK (MQuerySomeMK qs) = qs
