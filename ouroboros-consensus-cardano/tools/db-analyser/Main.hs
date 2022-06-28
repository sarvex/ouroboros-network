{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database analyse tool.
module Main (main) where

import           Data.Foldable (asum)
import           Data.Maybe (fromMaybe)
import           Options.Applicative
import           System.IO

import           Control.Tracer (Tracer (..), nullTracer)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB (LMDBLimits (..))
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk
                     (BackingStoreSelector (..), DiskSnapshot (..))
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Analysis
import           Block.Byron (ByronBlockArgs)
import           Block.Cardano (CardanoBlockArgs)
import           Block.Shelley (ShelleyBlockArgs)
import           HasAnalysis

main :: IO ()
main = do
    cmdLine <- getCmdLine
    case blockType cmdLine of
      ByronBlock   args -> analyse cmdLine args
      ShelleyBlock args -> analyse cmdLine args
      CardanoBlock args -> analyse cmdLine args

data SelectDB =
    SelectChainDB
  | SelectImmutableDB (Maybe DiskSnapshot)

data BackingStore = LMDB (Maybe Int) | MEM
  deriving Eq

data CmdLine = CmdLine {
    dbDir      :: FilePath
  , verbose    :: Bool
  , selectDB   :: SelectDB
  , validation :: Maybe ValidateBlocks
  , blockType  :: BlockType
  , analysis   :: AnalysisName
  , limit      :: Maybe Int
  , bsSelector :: BackingStore
  }

data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

data BlockType =
    ByronBlock   ByronBlockArgs
  | ShelleyBlock ShelleyBlockArgs
  | CardanoBlock CardanoBlockArgs

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> strOption (mconcat [
            long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ])
    <*> switch (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> parseSelectDB
    <*> parseValidationPolicy
    <*> blockTypeParser
    <*> parseAnalysis
    <*> parseLimit
    <*> parseSelector

parseSelector :: Parser BackingStore
parseSelector = fromMaybe MEM <$> parseMaybe (asum [
      MEM <$ parseMEM
    , LMDB <$ parseLMDB <*> parseMapSize
    ])
  where
    parseMEM :: Parser ()
    parseMEM = flag' () $ mconcat [
        long "inmem-backingstore"
      , help "Choose the in-memory backing store for the LedgerDB."
      ]
    parseLMDB :: Parser ()
    parseLMDB = flag' () $ mconcat [
        long "lmdb-backingstore"
      , help "Choose the LMDB backing store for the LedgerDB."
      ]
    parseMapSize :: Parser (Maybe Int)
    parseMapSize = optional $ read <$> strOption (mconcat [
        long "mapsize"
      , metavar "NR_BYTES"
      , help "The maximum database size defined in nr. of bytes NR_BYTES. NR_BYTES must be a multiple of the OS page size."
      ])

parseSelectDB :: Parser SelectDB
parseSelectDB = asum [
    SelectImmutableDB . snd <$> ((,) <$> onlyImmutableDB <*> analyseFrom)
  , pure SelectChainDB
  ]
  where
    onlyImmutableDB = flag' () (mconcat [
        long "only-immutable-db"
      , help "Validate only the Immutable DB (e.g. do not do ledger validation)"
      ])

    analyseFrom :: Parser (Maybe DiskSnapshot)
    analyseFrom = optional $ ((flip DiskSnapshot $ Just "db-analyser") . read) <$> strOption
      (  long "analyse-from"
      <> metavar "SLOT_NUMBER"
      <> help "Start analysis from ledger state stored at specific slot number" )


parseValidationPolicy :: Parser (Maybe ValidateBlocks)
parseValidationPolicy = parseMaybe $ asum [
      flag' ValidateAllBlocks $ mconcat [
          long "validate-all-blocks"
        , help "Validate all blocks of the Volatile and Immutable DB"
        ]
    , flag' MinimumBlockValidation $ mconcat [
          long "minimum-block-validation"
        , help "Validate a minimum part of the Volatile and Immutable DB"
        ]
    ]

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    , flag' ShowEBBs $ mconcat [
          long "show-ebbs"
        , help "Show all EBBs and their predecessors"
        ]
    , storeLedgerParser
    , flag' CountBlocks $ mconcat [
          long "count-blocks"
        , help "Count number of blocks processed"
        ]
    , checkNoThunksParser
    , flag' TraceLedgerProcessing $ mconcat [
          long "trace-ledger"
        , help $ "Maintain ledger state and trace ledger phases in the GHC event"
                <> " log. The db-analyser tool performs era-specific analysis"
                <> " of the ledger state and inserts markers for 'significant'"
                <> " events, such as for example epoch transitions."
        ]
    , fmap ReproMempoolAndForge $ option auto $ mconcat [
          long "repro-mempool-and-forge"
        , help $ "Maintain ledger state and mempool trafficking the"
              <> " transactions of each block. The integer is how many"
              <> "blocks to put in the mempool at once."
        , metavar "INT"
        ]
    , pure OnlyValidation
    ]

storeLedgerParser :: Parser AnalysisName
storeLedgerParser = (StoreLedgerStateAt . SlotNo) <$> option auto
  (  long "store-ledger"
  <> metavar "SLOT_NUMBER"
  <> help "Store ledger state at specific slot number" )

checkNoThunksParser :: Parser AnalysisName
checkNoThunksParser = CheckNoThunksEvery <$> option auto
  (  long "checkThunks"
  <> metavar "BLOCK_COUNT"
  <> help "Check the ledger state for thunks every n blocks" )

parseLimit :: Parser (Maybe Int)
parseLimit = asum [
    read <$> strOption (mconcat [
        long "num-blocks-to-process"
      , help "Maximum number of blocks we want to process"
      , metavar "INT"
      ])
  , pure Nothing
  ]

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "byron"
      (info (parseByronType   <**> helper) (progDesc "Analyse a Byron-only DB"))
  , command "shelley"
      (info (parseShelleyType <**> helper) (progDesc "Analyse a Shelley-only DB"))
  , command "cardano"
      (info (parseCardanoType <**> helper) (progDesc "Analyse a Cardano DB"))
  ]

parseByronType :: Parser BlockType
parseByronType = ByronBlock <$> argsParser Proxy

parseShelleyType :: Parser BlockType
parseShelleyType = ShelleyBlock <$> argsParser Proxy

parseCardanoType :: Parser BlockType
parseCardanoType = CardanoBlock <$> argsParser Proxy

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to analyse a Chain DB"
        ])

{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

analyse ::
     forall blk .
     ( Node.RunNode blk
     , Show (Header blk)
     , HasAnalysis blk
     , HasProtocolInfo blk
     , LedgerSupportsMempool.HasTxs blk
     )
  => CmdLine
  -> Args blk
  -> IO ()
analyse CmdLine {..} args =
    withRegistry $ \registry -> do

      chainDBTracer  <- mkTracer verbose
      analysisTracer <- mkTracer True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo  = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          k          = configSecurityParam cfg
          diskPolicy = defaultDiskPolicy k DefaultSnapshotInterval
          args' =
            Node.mkChainDbArgs
              registry InFuture.dontCheck cfg genesisLedger chunkInfo $
            ChainDB.defaultArgs (Node.stdMkChainDbHasFS dbDir) diskPolicy InMemoryBackingStore
          chainDbArgs = args' {
              ChainDB.cdbImmutableDbValidation = immValidationPolicy
            , ChainDB.cdbVolatileDbValidation  = volValidationPolicy
            , ChainDB.cdbTracer                = chainDBTracer
            }
          (immutableDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
          ledgerDbFS = ChainDB.cdbHasFSLgrDB chainDbArgs

      case selectDB of
        SelectImmutableDB initializeFrom -> do
          let initLedger = case initializeFrom of
                                Nothing       -> Right genesisLedger
                                Just snapshot -> Left snapshot

          let bs = case bsSelector of
                MEM          -> InMemoryBackingStore
                LMDB mapsize ->
                  maybe
                    (LMDBBackingStore defaultLMDBLimits)
                    (\n -> LMDBBackingStore (defaultLMDBLimits { lmdbMapSize = n }))
                    mapsize

          ImmutableDB.withDB (ImmutableDB.openDB immutableDbArgs runWithTempRegistry) $ \immutableDB -> do
            runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger
              , db = Left immutableDB
              , ledgerDbFS = ledgerDbFS
              , limit = limit
              , tracer = analysisTracer
              , backing = bs
              }
            tipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
            putStrLn $ "ImmutableDB tip: " ++ show tipPoint
        SelectChainDB ->
          ChainDB.withDB chainDbArgs $ \chainDB -> do
            runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger = Right genesisLedger
              , db = Right chainDB
              , ledgerDbFS = ledgerDbFS
              , limit = limit
              , tracer = analysisTracer
              , backing = undefined
              }
            tipPoint <- atomically $ ChainDB.getTipPoint chainDB
            putStrLn $ "ChainDB tip: " ++ show tipPoint
  where
    mkTracer False = return nullTracer
    mkTracer True  = do
      startTime <- getMonotonicTime
      return $ Tracer $ \ev -> do
        traceTime <- getMonotonicTime
        let diff = diffTime traceTime startTime
        hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
        hFlush stderr

    immValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ImmutableDB.ValidateAllChunks
      (_, Just MinimumBlockValidation) -> ImmutableDB.ValidateMostRecentChunk
      (OnlyValidation, _ )             -> ImmutableDB.ValidateAllChunks
      _                                -> ImmutableDB.ValidateMostRecentChunk

    volValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> VolatileDB.ValidateAll
      (_, Just MinimumBlockValidation) -> VolatileDB.NoValidation
      (OnlyValidation, _ )             -> VolatileDB.ValidateAll
      _                                -> VolatileDB.NoValidation

    -- Preferably, these settings should match the default configuration for
    -- @cardano-node@. There, we pick @'lmdbMapSize'@ and @'lmdbMaxDatabases'@
    -- such that they are sufficient for the medium term, i.e., until a more
    -- performant backing store is developed and integrated.
    defaultLMDBLimits = LMDBLimits {
      lmdbMapSize      = 16_000_000_000
    , lmdbMaxDatabases = 10
    , lmdbMaxReaders   = 16
    }
