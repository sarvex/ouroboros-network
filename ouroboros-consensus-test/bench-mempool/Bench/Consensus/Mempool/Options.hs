{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}

module Bench.Consensus.Mempool.Options (
    -- * Custom options
    customBenchIngredients
  , customOpts
    -- * Running commands
  , Ns (..)
    -- * Backing store selector
  , BSS
  , fromOptionBSS
  ) where

import           Bench.Consensus.Mempool.Params (defaultInMemoryBSS,
                     defaultLMDB_BSS)
import           Data.Char (toLower)
import           Data.Proxy (Proxy (..))
import           Options.Applicative (metavar)
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector)
import           Test.Tasty (includingOptions)
import           Test.Tasty.Bench (benchIngredients)
import           Test.Tasty.Ingredients (Ingredient)
import           Test.Tasty.Options (IsOption (..), OptionDescription (..),
                     mkOptionCLParser, safeRead)

{-------------------------------------------------------------------------------
  Custom options
-------------------------------------------------------------------------------}

customBenchIngredients :: [Ingredient]
customBenchIngredients = includingOptions customOpts : benchIngredients

customOpts :: [OptionDescription]
customOpts = [Option (Proxy :: Proxy BSS), Option (Proxy :: Proxy Ns)]

{-------------------------------------------------------------------------------
  Running commands
-------------------------------------------------------------------------------}

-- | Number of commands to run in the mempool benchmarks
--
-- Note: as this is a list of 'Int', the benchmarks will be run for each of
-- these values.
newtype Ns = Ns { getNs :: [Int] }
  deriving newtype Read

instance IsOption Ns where
  defaultValue = Ns [10_000]
  parseValue = safeRead
  optionName = pure "ns"
  optionHelp = pure "Number of commands to run in the mempool benchmarks"
  optionCLParser = mkOptionCLParser $ metavar "[INT]"
  showDefaultValue = Just . show . getNs

{-------------------------------------------------------------------------------
  Backing store selection
-------------------------------------------------------------------------------}

-- | Backing store to use in the mempool benchmarks: in-memory or LMDB.
data BSS = InMem | LMDB

instance IsOption BSS where
  defaultValue = InMem
  parseValue s = case map toLower s of
    "mem"  -> Just InMem
    "lmdb" -> Just LMDB
    _      -> Nothing
  optionName = pure "backingstore"
  optionHelp = pure "Which backing store to use for the LedgerDB"
  optionCLParser = mkOptionCLParser $ metavar "mem|lmdb"
  showDefaultValue = Just . \case
    InMem -> "mem"
    LMDB  -> "lmdb"

fromOptionBSS :: BSS -> BackingStoreSelector IO
fromOptionBSS = \case
  InMem -> defaultInMemoryBSS
  LMDB  -> defaultLMDB_BSS
