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

module Ouroboros.Consensus.Ledger.MapKind where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind
import           GHC.Generics
import           NoThunks.Class (NoThunks (..))


import           Ouroboros.Consensus.Util.Singletons

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq


type MapKind         = {- key -} Type -> {- value -} Type -> Type

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

type QueryMK :: MapKind
newtype QueryMK k v = ApplyQuerySomeMK (Keys k v)
  deriving stock (Show, Eq, Generic, Functor)
  deriving anyclass NoThunks

class IsMapKind mk where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v
  default emptyMK :: forall k v. (Monoid (mk k v)) => mk k v
  emptyMK = mempty

  mapMK :: forall k v v'. (Ord k, Eq v, Eq v') => (v -> v') -> mk k v -> mk k v'
  default mapMK :: forall k v v'. (Functor (mk k)) => (v -> v') -> mk k v -> mk k v'
  mapMK = fmap

data instance Sing EmptyMK where SEmptyMK    :: Sing EmptyMK
data instance Sing KeysMK where SKeysMK :: Sing KeysMK
data instance Sing ValuesMK where SValuesMK   :: Sing ValuesMK
data instance Sing TrackingMK where STrackingMK :: Sing TrackingMK
data instance Sing DiffMK where SDiffMK     :: Sing DiffMK
data instance Sing SeqDiffMK where SSeqDiffMK  :: Sing SeqDiffMK
data instance Sing QueryMK where SQueryMK    :: Sing QueryMK

instance SingI EmptyMK where sing = SEmptyMK
instance SingI KeysMK where sing = SKeysMK
instance SingI ValuesMK where sing = SValuesMK
instance SingI TrackingMK where sing = STrackingMK
instance SingI DiffMK where sing = SDiffMK
instance SingI SeqDiffMK where sing = SSeqDiffMK
instance SingI QueryMK where sing = SQueryMK

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String

type ApplyMapKind' :: MapKind -> MapKind
type family ApplyMapKind' (mk :: MapKind) where
  ApplyMapKind' DiffMK     = DiffMK
  ApplyMapKind' EmptyMK    = EmptyMK
  ApplyMapKind' KeysMK     = KeysMK
  ApplyMapKind' SeqDiffMK  = SeqDiffMK
  ApplyMapKind' TrackingMK = TrackingMK
  ApplyMapKind' ValuesMK   = ValuesMK
  ApplyMapKind' QueryAllMK = QueryAllMK
  ApplyMapKind' QueryMK    = QueryMK

type SMapKind = Sing :: MapKind -> Type

sMapKind :: IsApplyMapKind mk => SMapKind (UnApplyMapKind mk)
sMapKind = sing

type ApplyMapKind (mk :: MapKind) = mk

type family UnApplyMapKind (mk :: MapKind) :: MapKind where
  UnApplyMapKind (ApplyMapKind mk) = mk

type IsApplyMapKind mk = (mk ~ ApplyMapKind' (UnApplyMapKind mk), SingI (UnApplyMapKind mk), IsMapKind mk)
