{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Ledger.MapKinds (
    ApplyMapKind
    -- * The map kind
  , IsMapKind (..)
  , MapKind
    -- * ApplyMapKind'
  , ApplyMapKind' (..)
  , DiffMK
  , EmptyMK
  , KeysMK
  , MapKind' (..)
  , QueryMK
  , SeqDiffMK
  , TrackingMK
  , ValuesMK
  , showsApplyMapKind
    -- * Other mapkinds
  , CodecMK (..)
  , NameMK (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Kind
import           NoThunks.Class (NoThunks (..))
import qualified NoThunks.Class as NoThunks

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

-- | TODO(jdral): remove this, since this only exists for backwards
-- compatibility.
type ApplyMapKind :: forall k. k -> k
type ApplyMapKind mk = mk

{-------------------------------------------------------------------------------
  The map kind
-------------------------------------------------------------------------------}

type MapKind = {- key -} Type -> {- value -} Type -> Type

-- | A general interface to mapkinds for mapkind-polymorphic operations.
--
-- In some cases where @mk@ is not specialised to a concrete mapkind like
-- @'ValuesMK'@, there are often still a number of operations that we can
-- perform for this @mk@ that make sense regardless of the concrete mapkind. For
-- example, we should always be able to map over a mapkind to change the type of
-- values that it contains. This class is an interface to mapkinds that provides
-- such common functions.
class IsMapKind (mk :: MapKind) where
  emptyMK :: forall k v. (Ord k, Eq v) => mk k v
  default emptyMK :: forall k v. Monoid (mk k v) => mk k v
  emptyMK = mempty

  mapMK :: forall k v v'. (Ord k, Eq v, Eq v') => (v -> v') -> mk k v -> mk k v'
  default mapMK :: forall k v v'. (Functor (mk k)) => (v -> v') -> mk k v -> mk k v'
  mapMK = fmap

{-------------------------------------------------------------------------------
  ApplyMapKind'
-------------------------------------------------------------------------------}

data MapKind' = DiffMK'
              | EmptyMK'
              | KeysMK'
              | QueryMK'
              | SeqDiffMK'
              | TrackingMK'
              | ValuesMK'

type DiffMK     = ApplyMapKind' DiffMK'
type EmptyMK    = ApplyMapKind' EmptyMK'
type KeysMK     = ApplyMapKind' KeysMK'
type QueryMK    = ApplyMapKind' QueryMK'
type SeqDiffMK  = ApplyMapKind' SeqDiffMK'
type TrackingMK = ApplyMapKind' TrackingMK'
type ValuesMK   = ApplyMapKind' ValuesMK'

data ApplyMapKind' :: MapKind' -> Type -> Type -> Type where
  ApplyDiffMK     :: !(Diff    k v)                -> ApplyMapKind' DiffMK'       k v
  ApplyEmptyMK    ::                                  ApplyMapKind' EmptyMK'      k v
  ApplyKeysMK     :: !(Keys    k v)                -> ApplyMapKind' KeysMK'       k v
  ApplySeqDiffMK  :: !(DiffSeq k v)                -> ApplyMapKind' SeqDiffMK'    k v
  ApplyTrackingMK :: !(Values  k v) -> !(Diff k v) -> ApplyMapKind' TrackingMK'   k v
  ApplyValuesMK   :: !(Values  k v)                -> ApplyMapKind' ValuesMK'     k v

  ApplyQueryAllMK  ::                ApplyMapKind' QueryMK' k v
  ApplyQuerySomeMK :: !(Keys k v) -> ApplyMapKind' QueryMK' k v

instance IsMapKind EmptyMK where
  emptyMK = ApplyEmptyMK
  mapMK _ ApplyEmptyMK = ApplyEmptyMK

instance IsMapKind KeysMK where
  emptyMK = ApplyKeysMK mempty
  mapMK f (ApplyKeysMK ks) = ApplyKeysMK $ fmap f ks

instance IsMapKind ValuesMK where
  emptyMK = ApplyValuesMK mempty
  mapMK f (ApplyValuesMK vs) = ApplyValuesMK $ fmap f vs

instance IsMapKind TrackingMK where
  emptyMK = ApplyTrackingMK mempty mempty
  mapMK f (ApplyTrackingMK vs d) = ApplyTrackingMK (fmap f vs) (fmap f d)

instance IsMapKind DiffMK where
  emptyMK = ApplyDiffMK mempty

instance IsMapKind SeqDiffMK where
  emptyMK = ApplySeqDiffMK empty
  mapMK f (ApplySeqDiffMK ds) = ApplySeqDiffMK $ mapDiffSeq f ds

instance IsMapKind (ApplyMapKind' QueryMK') where
  emptyMK = ApplyQuerySomeMK mempty
  mapMK f qmk = case qmk of
    ApplyQuerySomeMK ks -> ApplyQuerySomeMK $ fmap f ks
    ApplyQueryAllMK     -> ApplyQueryAllMK

instance Ord k => Semigroup (ApplyMapKind' KeysMK' k v) where
  ApplyKeysMK l <> ApplyKeysMK r = ApplyKeysMK (l <> r)

instance Ord k => Monoid (ApplyMapKind' KeysMK' k v) where
  mempty = ApplyKeysMK mempty

instance Functor (DiffMK k) where
  fmap f (ApplyDiffMK d) = ApplyDiffMK $ fmap f d

instance (Ord k, Eq v) => Eq (ApplyMapKind' mk k v) where
  ApplyEmptyMK          == _                     = True
  ApplyKeysMK   l       == ApplyKeysMK   r       = l == r
  ApplyValuesMK l       == ApplyValuesMK r       = l == r
  ApplyTrackingMK l1 l2 == ApplyTrackingMK r1 r2 = l1 == r1 && l2 == r2
  ApplyDiffMK l         == ApplyDiffMK r         = l == r
  ApplySeqDiffMK l      == ApplySeqDiffMK r      = l == r
  ApplyQueryAllMK       == ApplyQueryAllMK       = True
  ApplyQuerySomeMK l    == ApplyQuerySomeMK r    = l == r
  _                     == _                     = False

instance (Ord k, NoThunks k, NoThunks v) => NoThunks (ApplyMapKind' mk k v) where
  wNoThunks ctxt   = NoThunks.allNoThunks . \case
    ApplyEmptyMK         -> []
    ApplyKeysMK ks       -> [noThunks ctxt ks]
    ApplyValuesMK vs     -> [noThunks ctxt vs]
    ApplyTrackingMK vs d -> [noThunks ctxt vs, noThunks ctxt d]
    ApplyDiffMK d        -> [noThunks ctxt d]
    ApplySeqDiffMK ds    -> [noThunks ctxt ds]
    ApplyQueryAllMK      -> []
    ApplyQuerySomeMK ks  -> [noThunks ctxt ks]

  showTypeOf _ = "ApplyMapKind"

showsApplyMapKind :: (Show k, Show v) => ApplyMapKind' mk k v -> ShowS
showsApplyMapKind = \case
    ApplyEmptyMK             -> showString "ApplyEmptyMK"
    ApplyKeysMK keys         -> showParen True $ showString "ApplyKeysMK " . shows keys
    ApplyValuesMK values     -> showParen True $ showString "ApplyValuesMK " . shows values
    ApplyTrackingMK values d -> showParen True $ showString "ApplyTrackingMK " . shows values . showString " " . shows d
    ApplyDiffMK d            -> showParen True $ showString "ApplyDiffMK " . shows d
    ApplySeqDiffMK sq        -> showParen True $ showString "ApplySeqDiffMK " . shows sq

    ApplyQueryAllMK       -> showParen True $ showString "ApplyQueryAllMK"
    ApplyQuerySomeMK keys -> showParen True $ showString "ApplyQuerySomeMK " . shows keys

instance (Show k, Show v) => Show (ApplyMapKind' mk k v) where
  show = flip showsApplyMapKind ""

{-------------------------------------------------------------------------------
  Other mapkinds
-------------------------------------------------------------------------------}

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String
