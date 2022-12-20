{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |

module Ouroboros.Consensus.Ledger.Tables (
    LedgerStateKind
  , ShowLedgerState (..)
    -- * UTxO HD
    -- ** Isolating the tables
  , InMemory (..)
  , StowableLedgerTables (..)
  , TableStuff (..)
  , TickedTableStuff (..)
  , mapOverLedgerTables
  , mapOverLedgerTablesTicked
  , overLedgerTables
  , overLedgerTablesTicked
  , zipOverLedgerTables
  , zipOverLedgerTablesTicked
    -- ** Tables values
  , ApplyMapKind
  , ApplyMapKind' (..)
  , MapKind
  , SMapKind
  , Sing (..)
  , emptyAppliedMK
  , mapValuesAppliedMK
  , sMapKind
  , sMapKind'
  , showsApplyMapKind
  , toSMapKind
    -- *** Mediators
  , CodecMK (..)
  , DiffMK
  , EmptyMK
  , IsApplyMapKind
  , KeysMK
  , NameMK (..)
  , QueryMK
  , SeqDiffMK
  , TrackingMK
  , UnApplyMapKind
  , ValuesMK
    -- ** Serialization
  , SufficientSerializationForAnyBackingStore (..)
  , valuesMKDecoder
  , valuesMKEncoder
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Exception as Exn
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Monoid (Sum (..))
import           NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))
import qualified NoThunks.Class as NoThunks

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.Singletons

import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

class ShowLedgerState (l :: LedgerStateKind) where
  showsLedgerState :: SMapKind mk -> l (ApplyMapKind' mk) (ApplyMapKind' mk) -> ShowS

class TableTagC (tt :: TableTag) (l :: Type) mk where

instance TableTagC UTxO (l mk1 mk2) mk1
instance TableTagC SecondTable (l mk1 mk2) mk2

type family MappedTable tt l mk1 mk2 mkX where
  MappedTable UTxO l mk1 mk2 mkX = l mkX mk2
  MappedTable SecondTable l mk1 mk2 mkX = l mk1 mkX

type Fun1 mkA = forall k v. (Ord k, Eq v) => mkA k v
type Fun2 mkA mkB = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v
type Fun3 mkA mkB mkC = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v -> mkC k v
type Fun4 mkA mkB mkC mkD = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v -> mkC k v -> mkD k v
type Fun2F mkA mkB f = forall k v. (Ord k, Eq v) => mkA k v -> f (mkB k v)
type Fun3F mkA mkB mkC f = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v -> f (mkC k v)
type Fun4F mkA mkB mkC mkD f = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v -> mkC k v -> f (mkD k v)
type FunFold1 mkA m = forall k v. (Ord k, Eq v) => mkA k v -> m
type FunFold2 mkA mkB m = forall k v. (Ord k, Eq v) => mkA k v -> mkB k v -> m

type (~~>) mkA mkB = Fun2 mkA mkB
type (~~~>) mkA mkB mkC = Fun3 mkA mkB mkC
type (~~~~>) mkA mkB mkC mkD = Fun4 mkA mkB mkC mkD
type (~&~>) mkA mkB f = Fun2F mkA mkB f
type (~&~~>) mkA mkB mkC f = Fun3F mkA mkB mkC f
type (~&~~~>) mkA mkB mkC mkD f = Fun4F mkA mkB mkC mkD f
type (~+~>) mkA mkB m = FunFold2 mkA mkB m

class ( Eq (l EmptyMK EmptyMK)
      , Eq (LedgerTables l DiffMK DiffMK)
      , Eq (LedgerTables l ValuesMK DiffMK )
      ) => TableStuff (l :: LedgerStateKind) where

  data family LedgerTables l :: LedgerStateKind

  -- | Some values of @l mk@ do not determine @mk@, hence the 'SingI' constraint.
  --
  -- If it were always the case that @l mk@ not determing @mk@ implies
  -- @LedgerTables l mk@ also does not determine @mk@, then we would not need
  -- the 'SingI' constraint. Unfortunately, that is not always the case. The
  -- example we have found in our prototype UTxO HD implementat is that a Byron
  -- ledger state does not determine @mk@, but the Cardano ledger tables do.
  projectLedgerTables :: l mk1 mk2 -> LedgerTables l mk1 mk2

  -- | Overwrite the tables in some ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- TODO: reconsider the name: don't we use 'withX' in the context of bracket like functions?
  withLedgerTables :: l any1 any2 -> LedgerTables l mk1 mk2 -> l mk1 mk2

  pureLedgerTables ::
       Fun1 mkA
    -> Fun1 mkB
    -> LedgerTables l mkA mkB

  mapLedgerTables ::
       mkA ~~> mkX
    -> mkB ~~> mkY
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY

  traverseLedgerTables ::
       Applicative f
    => (mkA ~&~> mkX) f
    -> (mkB ~&~> mkY) f
    -> LedgerTables l mk1 mk2
    -> f (LedgerTables l mkX mkY)

  zipLedgerTables ::
       (mkA ~~~> mkX) mkP
    -> (mkB ~~~> mkY) mkQ
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY
    -> LedgerTables l mkP mkQ

  zipLedgerTables2 ::
       (mkA ~~~~> mkX) mkP mkM
    -> (mkB ~~~~> mkY) mkQ mkN
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY
    -> LedgerTables l mkP mkQ
    -> LedgerTables l mkM mkN

  zipLedgerTablesA ::
       Applicative f
    => (mkA ~&~~> mkX) mkP f
    -> (mkB ~&~~> mkY) mkQ f
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY
    -> f (LedgerTables l mkP mkQ)

  zipLedgerTables2A ::
       Applicative f
    => (mkA ~&~~~> mkX) mkP mkM f
    -> (mkB ~&~~~> mkY) mkQ mkN f
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY
    -> LedgerTables l mkP mkQ
    -> f (LedgerTables l mkM mkN)

  foldLedgerTables ::
       (Monoid m, Monoid n)
    => FunFold1 mkA m
    -> FunFold1 mkB n
    -> LedgerTables l mkA mkB
    -> (m, n)

  foldLedgerTables2 ::
       (Monoid m, Monoid n)
    => (mkA ~+~> mkX) m
    -> (mkB ~+~> mkY) n
    -> LedgerTables l mkA mkB
    -> LedgerTables l mkX mkY
    -> (m, n)

  namesLedgerTables :: LedgerTables l NameMK NameMK

overLedgerTables ::
     TableStuff l
  => (LedgerTables l mkA mkB -> LedgerTables l mkX mkY)
  -> l mkA mkB
  -> l mkX mkY
overLedgerTables f l = withLedgerTables l $ f $ projectLedgerTables l

mapOverLedgerTables :: forall l mkA mkX mkB mkY.
     TableStuff l
  => mkA ~~> mkX
  -> mkB ~~> mkY
  -> l mkA mkB
  -> l mkX mkY
mapOverLedgerTables f1 f2 = overLedgerTables $ mapLedgerTables f1 f2

zipOverLedgerTables ::
     TableStuff l
  => (mkA ~~~> mkX) mkP
  -> (mkB ~~~> mkY) mkQ
  ->              l mkA mkB
  -> LedgerTables l mkX mkY
  ->              l mkP mkQ
zipOverLedgerTables f1 f2 l tables2 =
    overLedgerTables
      (\tables1 -> zipLedgerTables f1 f2 tables1 tables2)
      l

class TableStuff l => TickedTableStuff (l :: LedgerStateKind) where
  -- | NOTE: The 'IsApplyMapKind mk2' constraint is here for the same reason
  -- it's on 'projectLedgerTables'
  projectLedgerTablesTicked :: Ticked2 l mk1 mk2  -> LedgerTables l mk1 mk2
  -- | NOTE: The 'IsApplyMapKind mk2' constraint is here for the same reason
  -- it's on 'withLedgerTables'
  withLedgerTablesTicked    :: Ticked2 l any1 any2 -> LedgerTables l mk1 mk2 -> Ticked2 l mk1 mk2

overLedgerTablesTicked ::
     TickedTableStuff l
  => (LedgerTables l mk1 mk2 -> LedgerTables l mk1' mk2')
  -> Ticked2 l mk1 mk2
  -> Ticked2 l mk1' mk2'
overLedgerTablesTicked f l =
    withLedgerTablesTicked l $ f $ projectLedgerTablesTicked l

mapOverLedgerTablesTicked ::
     TickedTableStuff l
  => mkA ~~> mkX
  -> mkB ~~> mkY
  -> Ticked2 l mkA mkB
  -> Ticked2 l mkX mkY
mapOverLedgerTablesTicked f1 f2 = overLedgerTablesTicked $ mapLedgerTables f1 f2

zipOverLedgerTablesTicked ::
     TickedTableStuff l
  => (mkA ~~~> mkX) mkP
  -> (mkB ~~~> mkY) mkQ
  -> Ticked2      l mkA mkB
  -> LedgerTables l mkX mkY
  -> Ticked2      l mkP mkQ
zipOverLedgerTablesTicked f1 f2 l tables2 =
    overLedgerTablesTicked
      (\tables1 -> zipLedgerTables f1 f2 tables1 tables2)
      l

class StowableLedgerTables (l :: LedgerStateKind) where
  stowLedgerTables1     :: l ValuesMK mk2 -> l EmptyMK mk2
  unstowLedgerTables1   :: l EmptyMK mk2 -> l ValuesMK mk2
  stowLedgerTables2     :: l mk1 ValuesMK -> l mk1 EmptyMK
  unstowLedgerTables2   :: l mk1 EmptyMK -> l mk1 ValuesMK

{-------------------------------------------------------------------------------
  Concrete ledger tables
-------------------------------------------------------------------------------}

type MapKind         = {- key -} Type -> {- value -} Type -> Type
type TableKind       = TableTag -> MapKind -> Type
type LedgerStateKind = MapKind -> MapKind -> Type

data TableTag = UTxO | SecondTable

data STableTag tt where
  SUTxO        :: STableTag UTxO
  SSecondTable :: STableTag SecondTable

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

-- | A codec 'MapKind' that will be used to refer to @'LedgerTables' l CodecMK@
-- as the codecs that can encode every key and value in the @'LedgerTables' l
-- mk@.
data CodecMK k v = CodecMK
                     (k -> CBOR.Encoding)
                     (v -> CBOR.Encoding)
                     (forall s . CBOR.Decoder s k)
                     (forall s . CBOR.Decoder s v)

newtype NameMK k v = NameMK String

type ApplyMapKind mk = mk

data ApplyMapKind' :: MapKind' -> Type -> Type -> Type where
  ApplyDiffMK     :: !(Diff    k v)                -> ApplyMapKind' DiffMK'       k v
  ApplyEmptyMK    ::                                  ApplyMapKind' EmptyMK'      k v
  ApplyKeysMK     :: !(Keys    k v)                -> ApplyMapKind' KeysMK'       k v
  ApplySeqDiffMK  :: !(DiffSeq k v)                -> ApplyMapKind' SeqDiffMK'    k v
  ApplyTrackingMK :: !(Values  k v) -> !(Diff k v) -> ApplyMapKind' TrackingMK'   k v
  ApplyValuesMK   :: !(Values  k v)                -> ApplyMapKind' ValuesMK'     k v

  ApplyQueryAllMK  ::                ApplyMapKind' QueryMK' k v
  ApplyQuerySomeMK :: !(Keys k v) -> ApplyMapKind' QueryMK' k v

emptyAppliedMK :: (Ord k, Eq v) => SMapKind mk -> ApplyMapKind' mk k v
emptyAppliedMK = \case
    SEmptyMK    -> ApplyEmptyMK
    SKeysMK     -> ApplyKeysMK      mempty
    SValuesMK   -> ApplyValuesMK    mempty
    STrackingMK -> ApplyTrackingMK  mempty mempty
    SDiffMK     -> ApplyDiffMK      mempty
    SSeqDiffMK  -> ApplySeqDiffMK   empty
    SQueryMK    -> ApplyQuerySomeMK mempty

instance Ord k => Semigroup (ApplyMapKind' KeysMK' k v) where
  ApplyKeysMK l <> ApplyKeysMK r = ApplyKeysMK (l <> r)

instance Ord k => Monoid (ApplyMapKind' KeysMK' k v) where
  mempty = ApplyKeysMK mempty

instance Functor (DiffMK k) where
  fmap f (ApplyDiffMK d) = ApplyDiffMK $ fmap f d

mapValuesAppliedMK :: (Ord k, Eq v, Eq v') => (v -> v') -> ApplyMapKind' mk k v ->  ApplyMapKind' mk k v'
mapValuesAppliedMK f = \case
  ApplyEmptyMK         -> ApplyEmptyMK
  ApplyKeysMK ks       -> ApplyKeysMK     (castKeys ks)
  ApplyValuesMK vs     -> ApplyValuesMK   (fmap f vs)
  ApplyTrackingMK vs d -> ApplyTrackingMK (fmap f vs)   (fmap f d)
  ApplyDiffMK d        -> ApplyDiffMK     (fmap f d)
  ApplySeqDiffMK ds    -> ApplySeqDiffMK  (mapDiffSeq f ds)

  ApplyQueryAllMK      -> ApplyQueryAllMK
  ApplyQuerySomeMK vs  -> ApplyQuerySomeMK (fmap f vs)

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

data instance Sing (mk :: MapKind') :: Type where
  SEmptyMK    :: Sing EmptyMK'
  SKeysMK     :: Sing KeysMK'
  SValuesMK   :: Sing ValuesMK'
  STrackingMK :: Sing TrackingMK'
  SDiffMK     :: Sing DiffMK'
  SSeqDiffMK  :: Sing SeqDiffMK'
  SQueryMK    :: Sing QueryMK'

type SMapKind = Sing :: MapKind' -> Type

type family UnApplyMapKind (mk :: MapKind) :: MapKind' where
  UnApplyMapKind (ApplyMapKind' mk') = mk'

type IsApplyMapKind mk = (mk ~ ApplyMapKind' (UnApplyMapKind mk), SingI (UnApplyMapKind mk))

instance SingI EmptyMK'    where sing = SEmptyMK
instance SingI KeysMK'     where sing = SKeysMK
instance SingI ValuesMK'   where sing = SValuesMK
instance SingI TrackingMK' where sing = STrackingMK
instance SingI DiffMK'     where sing = SDiffMK
instance SingI SeqDiffMK'  where sing = SSeqDiffMK
instance SingI QueryMK'    where sing = SQueryMK

sMapKind :: IsApplyMapKind mk => SMapKind (UnApplyMapKind mk)
sMapKind = sing

sMapKind' :: IsApplyMapKind mk => proxy mk -> SMapKind (UnApplyMapKind mk)
sMapKind' _ = sMapKind

toSMapKind :: ApplyMapKind' mk k v -> SMapKind mk
toSMapKind = \case
    ApplyEmptyMK{}     -> SEmptyMK
    ApplyKeysMK{}      -> SKeysMK
    ApplyValuesMK{}    -> SValuesMK
    ApplyTrackingMK{}  -> STrackingMK
    ApplyDiffMK{}      -> SDiffMK
    ApplySeqDiffMK{}   -> SSeqDiffMK

    ApplyQueryAllMK{}  -> SQueryMK
    ApplyQuerySomeMK{} -> SQueryMK

instance Eq (Sing (mk :: MapKind')) where
  _ == _ = True

instance Show (Sing (mk :: MapKind')) where
  show = \case
    SEmptyMK    -> "SEmptyMK"
    SKeysMK     -> "SKeysMK"
    SValuesMK   -> "SValuesMK"
    STrackingMK -> "STrackingMK"
    SDiffMK     -> "SDiffMK"
    SSeqDiffMK  -> "SSeqDiffMK"
    SQueryMK    -> "SQueryMK"

deriving via OnlyCheckWhnfNamed "Sing @MapKind'" (Sing (mk :: MapKind')) instance NoThunks (Sing mk)

-- TODO include a tag, for some self-description
instance ToCBOR (Sing EmptyMK')    where toCBOR SEmptyMK    = CBOR.encodeNull
instance ToCBOR (Sing KeysMK')     where toCBOR SKeysMK     = CBOR.encodeNull
instance ToCBOR (Sing ValuesMK')   where toCBOR SValuesMK   = CBOR.encodeNull
instance ToCBOR (Sing TrackingMK') where toCBOR STrackingMK = CBOR.encodeNull
instance ToCBOR (Sing DiffMK')     where toCBOR SDiffMK     = CBOR.encodeNull
instance ToCBOR (Sing SeqDiffMK')  where toCBOR SSeqDiffMK  = CBOR.encodeNull
instance ToCBOR (Sing QueryMK')    where toCBOR SQueryMK    = CBOR.encodeNull

-- TODO include a tag, for some self-description
instance FromCBOR (Sing EmptyMK')    where fromCBOR = SEmptyMK    <$ CBOR.decodeNull
instance FromCBOR (Sing KeysMK')     where fromCBOR = SKeysMK     <$ CBOR.decodeNull
instance FromCBOR (Sing ValuesMK')   where fromCBOR = SValuesMK   <$ CBOR.decodeNull
instance FromCBOR (Sing TrackingMK') where fromCBOR = STrackingMK <$ CBOR.decodeNull
instance FromCBOR (Sing DiffMK')     where fromCBOR = SDiffMK     <$ CBOR.decodeNull
instance FromCBOR (Sing SeqDiffMK')  where fromCBOR = SSeqDiffMK  <$ CBOR.decodeNull
instance FromCBOR (Sing QueryMK')    where fromCBOR = SQueryMK    <$ CBOR.decodeNull

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | This class provides a 'CodecMK' that can be used to encode/decode keys and
-- values on @'LedgerTables' l mk@
class SufficientSerializationForAnyBackingStore (l :: LedgerStateKind) where
  codecLedgerTables :: LedgerTables l CodecMK CodecMK

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
     ( TableStuff l
     , SufficientSerializationForAnyBackingStore l
     )
  => LedgerTables l ValuesMK ValuesMK
  -> CBOR.Encoding
valuesMKEncoder tables =
       CBOR.encodeListLen (getSum (uncurry (+) $ foldLedgerTables (\_ -> Sum 1) (\_ -> Sum 1) tables))
    <> (uncurry (<>) (foldLedgerTables2 go go codecLedgerTables tables))
  where
    go :: CodecMK k v -> ApplyMapKind ValuesMK k v -> CBOR.Encoding
    go (CodecMK encK encV _decK _decV) (ApplyValuesMK (Values m)) =
         CBOR.encodeMapLen (fromIntegral $ Map.size m)
      <> Map.foldMapWithKey (\k v -> encK k <> encV v) m

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
--
-- TODO: we need to make sure there are tests that exercise this function.
valuesMKDecoder ::
     ( TableStuff l
     , SufficientSerializationForAnyBackingStore l
     )
  => CBOR.Decoder s (LedgerTables l ValuesMK ValuesMK)
valuesMKDecoder = do
    numTables <- CBOR.decodeListLen
    if numTables == 0
      then
        return $ pureLedgerTables (emptyAppliedMK sMapKind) (emptyAppliedMK sMapKind)
      else do
        ret    <- traverseLedgerTables
                     (\tb -> CBOR.decodeMapLen >>= \mapLen -> go mapLen tb)
                     (\tb -> CBOR.decodeMapLen >>= \mapLen -> go mapLen tb)
                     codecLedgerTables
        Exn.assert ((getSum (uncurry (+) $ foldLedgerTables (\_ -> Sum 1) (\_ -> Sum 1) ret)) == numTables)
          $ return ret
 where
  go :: Ord k
     => Int
     -> CodecMK k v
     -> CBOR.Decoder s (ApplyMapKind ValuesMK k v)
  go len (CodecMK _encK _encV decK decV) =
        ApplyValuesMK . Values . Map.fromList
    <$> sequence (replicate len ((,) <$> decK <*> decV))

{-------------------------------------------------------------------------------
  Special classes of ledger states
-------------------------------------------------------------------------------}

class InMemory (l :: LedgerStateKind) where

  -- | If the ledger state is always in memory, then l mk will be isomorphic to
  -- l mk' for all mk, mk'. As a result, we can convert between ledgers states
  -- indexed by different map kinds.
  --
  -- This function is useful to combine functions that operate on functions that
  -- transform the map kind on a ledger state (eg applyChainTickLedgerResult).
  convertMapKind :: l mk1 mk2 -> l mk1' mk2'
