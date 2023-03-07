{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Concurrent.Class.MonadSTM.TMVar.Strict.Checked (
    -- * StrictTMVar with invariant checking
    LazyTMVar
  , StrictTMVar
  , castStrictTMVar
  , fromLazyTMVar
  , isEmptyTMVar
  , newEmptyTMVarWithInvariant
  , newEmptyTMVarWithInvariantIO
  , newTMVarWithInvariantIO
  , putTMVarIO
  , readTMVar
  , swapTMVarIO
  , takeTMVar
  , toLazyTMVar
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

import           Control.Concurrent.Class.MonadSTM.TMVar.Strict.Internal
