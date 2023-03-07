{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Concurrent.Class.MonadSTM.TMVar.Strict.Internal (
    -- * StrictTMVar with invariant checking
    LazyTMVar
  , StrictTMVar
  , castStrictTMVar
  , fromLazyTMVar
  , isEmptyTMVar
  , newEmptyTMVar
  , newEmptyTMVarIO
  , newEmptyTMVarWithInvariant
  , newEmptyTMVarWithInvariantIO
  , newTMVar
  , newTMVarIO
  , newTMVarWithInvariantIO
  , putTMVar
  , putTMVarIO
  , readTMVar
  , swapTMVar
  , swapTMVarIO
  , takeTMVar
  , toLazyTMVar
  , tryPutTMVar
  , tryPutTMVarIO
  , tryReadTMVar
  , tryTakeTMVar
    -- * MonadLabelledSTM
  , labelTMVar
  , labelTMVarIO
    -- * MonadTraceSTM
  , traceTMVar
  , traceTMVarIO
  ) where

import qualified Control.Concurrent.Class.MonadSTM.TMVar as Lazy
import           Control.Monad.Class.MonadSTM

import           GHC.Stack

--
-- StrictTMVar with invariant checking
--

type LazyTMVar m = Lazy.TMVar m

#if CHECK_TMVAR_INVARIANT
data StrictTMVar m a = StrictTMVar {
    invariant :: !(a -> Maybe String)
  , tmvar     :: !(LazyTMVar m a)
  }
#else
newtype StrictTMVar m a = StrictTMVar
   { tmvar      :: LazyTMVar m a
   }
#endif

toLazyTMVar :: StrictTMVar m a -> LazyTMVar m a
toLazyTMVar StrictTMVar { tmvar } = tmvar

fromLazyTMVar :: LazyTMVar m a -> StrictTMVar m a
fromLazyTMVar tmvar =
#if CHECK_TMVAR_INVARIANT
  StrictTMVar { invariant = const Nothing
              , tmvar
              }
#else
  StrictTMVar { tmvar }
#endif

castStrictTMVar :: (LazyTMVar m ~ LazyTMVar n)
                => StrictTMVar m a -> StrictTMVar n a
castStrictTMVar v@StrictTMVar { tmvar } = mkStrictTMVar (getInvariant v) tmvar

newTMVar :: MonadSTM m => a -> STM m (StrictTMVar m a)
newTMVar !a = mkStrictTMVar (const Nothing) <$> Lazy.newTMVar a

newTMVarIO :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarIO !a = mkStrictTMVar (const Nothing) <$> Lazy.newTMVarIO a

newTMVarWithInvariantIO ::
      (MonadSTM m, HasCallStack)
   => (a -> Maybe String)
   -> a
   -> m (StrictTMVar m a)
newTMVarWithInvariantIO invariant !a =
    checkInvariant (invariant a) $
    mkStrictTMVar invariant <$> Lazy.newTMVarIO a

newEmptyTMVar :: MonadSTM m => STM m (StrictTMVar m a)
newEmptyTMVar = mkStrictTMVar (const Nothing) <$> Lazy.newEmptyTMVar

newEmptyTMVarIO :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarIO = mkStrictTMVar (const Nothing) <$> Lazy.newEmptyTMVarIO

-- This is probably not very useful, since writing to the mvar must be done in
-- IO, so you'll probably use atomically on this anyway. Unless the invariant is
-- @const Nothing@
newEmptyTMVarWithInvariant :: MonadSTM m
                           => (a -> Maybe String)
                           -> STM m (StrictTMVar m a)
newEmptyTMVarWithInvariant invariant =
    mkStrictTMVar invariant <$> Lazy.newEmptyTMVar

newEmptyTMVarWithInvariantIO :: MonadSTM m
                             => (a -> Maybe String)
                             -> m (StrictTMVar m a)
newEmptyTMVarWithInvariantIO invariant =
    mkStrictTMVar invariant <$> Lazy.newEmptyTMVarIO

takeTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
takeTMVar StrictTMVar { tmvar } = Lazy.takeTMVar tmvar

tryTakeTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryTakeTMVar StrictTMVar { tmvar } = Lazy.tryTakeTMVar tmvar

-- Do not use if @v@ has an invariant.
putTMVar :: forall m a. MonadSTM m => StrictTMVar m a -> a -> STM m ()
putTMVar v !a = Lazy.putTMVar (tmvar v) a

putTMVarIO :: forall m a. MonadSTM m => StrictTMVar m a -> a -> m ()
putTMVarIO v !a = do
    atomically $ Lazy.putTMVar (tmvar v) a
    checkInvariant (getInvariant v a) $ pure ()

-- Do not use if @v@ has an invariant.
tryPutTMVar :: forall m a. MonadSTM m => StrictTMVar m a -> a -> STM m Bool
tryPutTMVar v !a = Lazy.tryPutTMVar (tmvar v) a

tryPutTMVarIO :: forall m a. MonadSTM m => StrictTMVar m a -> a -> m Bool
tryPutTMVarIO v !a = do
    didPut <- atomically $ Lazy.tryPutTMVar (tmvar v) a
    -- TODO: should we only check the invariant of putting failed?
    checkInvariant (getInvariant v a) $ pure didPut

readTMVar :: MonadSTM m => StrictTMVar m a -> STM m a
readTMVar v = Lazy.readTMVar (tmvar v)

tryReadTMVar :: MonadSTM m => StrictTMVar m a -> STM m (Maybe a)
tryReadTMVar v = Lazy.tryReadTMVar (tmvar v)

-- Do not use if @v@ has an invariant.
swapTMVar :: forall m a. MonadSTM m => StrictTMVar m a -> a -> STM m a
swapTMVar v !a = Lazy.swapTMVar (tmvar v) a

swapTMVarIO :: forall m a. MonadSTM m => StrictTMVar m a -> a -> m a
swapTMVarIO v !a = do
    oldValue <- atomically $ Lazy.swapTMVar (tmvar v) a
    checkInvariant (getInvariant v a) $ pure oldValue

isEmptyTMVar :: MonadSTM m => StrictTMVar m a -> STM m Bool
isEmptyTMVar v = Lazy.isEmptyTMVar (tmvar v)

--
-- MonadLabelledSTM
--

labelTMVar :: MonadLabelledSTM m => StrictTMVar m a -> String -> STM m ()
labelTMVar StrictTMVar { tmvar } = Lazy.labelTMVar tmvar

labelTMVarIO :: MonadLabelledSTM m => StrictTMVar m a -> String -> m ()
labelTMVarIO v = atomically . labelTMVar v

--
-- MonadTraceSTM
--

traceTMVar :: MonadTraceSTM m
           => proxy m
           -> StrictTMVar m a
           -> (Maybe (Maybe a) -> Maybe a -> InspectMonad m TraceValue)
           -> STM m ()
traceTMVar p StrictTMVar { tmvar } = Lazy.traceTMVar p tmvar

traceTMVarIO :: MonadTraceSTM m
             => StrictTMVar m a
             -> (Maybe (Maybe a) -> Maybe a -> InspectMonad m TraceValue)
             -> m ()
traceTMVarIO StrictTMVar { tmvar } = Lazy.traceTMVarIO tmvar

--
-- Dealing with invariants
--

getInvariant :: StrictTMVar m a -> a -> Maybe String
mkStrictTMVar :: (a -> Maybe String) -> Lazy.TMVar m a -> StrictTMVar m a


-- | Check invariant (if enabled) before continuing
--
-- @checkInvariant mErr x@ is equal to @x@ if @mErr == Nothing@, and throws
-- an error @err@ if @mErr == Just err@.
--
-- This is exported so that other code that wants to conditionally check
-- invariants can reuse the same logic, rather than having to introduce new
-- per-package flags.
checkInvariant :: HasCallStack => Maybe String -> a -> a

#if CHECK_TMVAR_INVARIANT
getInvariant StrictTMVar {invariant} = invariant
mkStrictTMVar invariant  tmvar = StrictTMVar {invariant, tmvar}

checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "Invariant violation: " ++ err
#else
getInvariant _             _   = Nothing
mkStrictTMVar _invariant tmvar = StrictTMVar {tmvar}

checkInvariant _err       k  = k
#endif
