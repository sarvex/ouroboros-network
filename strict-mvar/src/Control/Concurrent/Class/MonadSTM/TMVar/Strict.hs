module Control.Concurrent.Class.MonadSTM.TMVar.Strict (
    -- * StrictTMVar
    LazyTMVar
  , StrictTMVar
  , castStrictTMVar
  , fromLazyTMVar
  , isEmptyTMVar
  , newEmptyTMVar
  , newEmptyTMVarIO
  , newTMVar
  , newTMVarIO
  , putTMVar
  , readTMVar
  , swapTMVar
  , takeTMVar
  , toLazyTMVar
  , tryPutTMVar
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
