{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node kernel which does chain selection and block production.
--
module Test.Ouroboros.Network.Diffusion.Node.NodeKernel
  ( -- * Common types
    NtNAddr
  , NtNAddr_ (..)
  , encodeNtNAddr
  , decodeNtNAddr
  , NtNVersion
  , NtNVersionData (..)
  , NtCAddr
  , NtCVersion
  , NtCVersionData
    -- * Node kernel
  , BlockGeneratorArgs (..)
  , relayBlockGenerationArgs
  , randomBlockGenerationArgs
  , NodeKernel (..)
  , newNodeKernel
  , registerClientChains
  , unregisterClientChains
  , withNodeKernelThread
  , NodeKernelError (..)
  ) where

import           GHC.Generics (Generic)

import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad (replicateM, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Char8 as BSC
import           Data.Hashable (Hashable)
import           Data.IP (IP (..), fromIPv4w, fromIPv6w, toIPv4, toIPv4w,
                     toIPv6, toIPv6w)
import qualified Data.IP as IP
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Numeric.Natural (Natural)

import           System.Random (StdGen, randomR)

import           Data.Monoid.Synchronisation

import           Network.Socket (PortNumber)

import           Ouroboros.Network.AnchoredFragment (Anchor (..))
import           Ouroboros.Network.Block (HasFullHeader, SlotNo)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Snocket (TestAddress (..))

import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Mock.ConcreteBlock (Block)
import qualified Ouroboros.Network.Mock.ConcreteBlock as ConcreteBlock
import           Ouroboros.Network.Mock.ProducerState

import           Simulation.Network.Snocket (AddressType (..),
                     GlobalAddressScheme (..))

import           Test.Ouroboros.Network.Orphans ()

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Ouroboros.Network.Mock.Chain (Chain (..))
import           Ouroboros.Network.NodeToNode ()
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSharing (PeerSharingRegistry (..),
                     newPeerSharingRegistry)
import qualified Test.Ouroboros.Network.Diffusion.Node.ChainDB as ChainDB
import           Test.Ouroboros.Network.Diffusion.Node.ChainDB (ChainDB (..))
import           Test.QuickCheck (Arbitrary (..), choose, chooseInt, frequency,
                     oneof)


-- | Node-to-node address type.
--
data NtNAddr_
  = EphemeralIPv4Addr Natural
  | EphemeralIPv6Addr Natural
  | IPAddr IP.IP PortNumber
  deriving (Eq, Ord, Generic)

instance Arbitrary NtNAddr_ where
  arbitrary = do
    -- TODO: Move this IP generator to ouroboros-network-testing
    a <- oneof [ IPv6 . toIPv6 <$> replicateM 8 (choose (0,0xffff))
               , IPv4 . toIPv4 <$> replicateM 4 (choose (0,255))
               ]
    frequency
      [ (1 , EphemeralIPv4Addr <$> (fromInteger <$> arbitrary))
      , (1 , EphemeralIPv6Addr <$> (fromInteger <$> arbitrary))
      , (3 , IPAddr a          <$> (read . show <$> chooseInt (0, 9999)))
      ]

instance Show NtNAddr_ where
    show (EphemeralIPv4Addr n) = "EphemeralIPv4Addr " ++ show n
    show (EphemeralIPv6Addr n) = "EphemeralIPv6Addr " ++ show n
    show (IPAddr ip port)      = "IPAddr (read \"" ++ show ip ++ "\") " ++ show port

instance GlobalAddressScheme NtNAddr_ where
    getAddressType (TestAddress addr) =
      case addr of
        EphemeralIPv4Addr _   -> IPv4Address
        EphemeralIPv6Addr _   -> IPv6Address
        IPAddr (IP.IPv4 {}) _ -> IPv4Address
        IPAddr (IP.IPv6 {}) _ -> IPv6Address
    ephemeralAddress IPv4Address = TestAddress . EphemeralIPv4Addr
    ephemeralAddress IPv6Address = TestAddress . EphemeralIPv6Addr

instance Hashable NtNAddr_

type NtNAddr        = TestAddress NtNAddr_
type NtNVersion     = UnversionedProtocol
data NtNVersionData = NtNVersionData
  { ntnDiffusionMode :: DiffusionMode
  , ntnPeerSharing   :: PeerSharing
  }
  deriving Show
type NtCAddr        = TestAddress Int
type NtCVersion     = UnversionedProtocol
type NtCVersionData = UnversionedProtocolData

encodeNtNAddr :: NtNAddr -> CBOR.Encoding
encodeNtNAddr (TestAddress (EphemeralIPv4Addr nat)) = CBOR.encodeListLen 2
                                                   <> CBOR.encodeWord 0
                                                   <> CBOR.encodeWord (fromIntegral nat)
encodeNtNAddr (TestAddress (EphemeralIPv6Addr nat)) = CBOR.encodeListLen 2
                                                   <> CBOR.encodeWord 1
                                                   <> CBOR.encodeWord (fromIntegral nat)
encodeNtNAddr (TestAddress (IPAddr ip pn)) = CBOR.encodeListLen 3
                                          <> CBOR.encodeWord 2
                                          <> encodeIP ip
                                          <> encodePortNumber pn

decodeNtNAddr :: CBOR.Decoder s NtNAddr
decodeNtNAddr = do
  _ <- CBOR.decodeListLen
  tok <- CBOR.decodeWord
  case tok of
    0 -> (TestAddress . EphemeralIPv4Addr . fromIntegral) <$> CBOR.decodeWord
    1 -> (TestAddress . EphemeralIPv6Addr . fromIntegral) <$> CBOR.decodeWord
    2 -> TestAddress <$> (IPAddr <$> decodeIP <*> decodePortNumber)
    _ -> fail ("decodeNtNAddr: unknown tok:" ++ show tok)

encodeIP :: IP -> CBOR.Encoding
encodeIP (IPv4 ip) = CBOR.encodeListLen 2
                  <> CBOR.encodeWord 0
                  <> CBOR.encodeWord32 (fromIPv4w ip)
encodeIP (IPv6 ip) = case fromIPv6w ip of
  (w1, w2, w3, w4) -> CBOR.encodeListLen 5
                   <> CBOR.encodeWord 1
                   <> CBOR.encodeWord32 w1
                   <> CBOR.encodeWord32 w2
                   <> CBOR.encodeWord32 w3
                   <> CBOR.encodeWord32 w4

decodeIP :: CBOR.Decoder s IP
decodeIP = do
  _ <- CBOR.decodeListLen
  tok <- CBOR.decodeWord
  case tok of
    0 -> (IPv4 . toIPv4w) <$> CBOR.decodeWord32
    1 -> do
      w1 <- CBOR.decodeWord32
      w2 <- CBOR.decodeWord32
      w3 <- CBOR.decodeWord32
      w4 <- CBOR.decodeWord32
      return (IPv6 (toIPv6w (w1, w2, w3, w4)))

    _ -> fail ("decodeIP: unknown tok:" ++ show tok)

encodePortNumber :: PortNumber -> CBOR.Encoding
encodePortNumber = CBOR.encodeWord16 . fromIntegral

decodePortNumber :: CBOR.Decoder s PortNumber
decodePortNumber = fromIntegral <$> CBOR.decodeWord16

data BlockGeneratorArgs block s = BlockGeneratorArgs
  { bgaSlotDuration   :: DiffTime
    -- ^ slot duration

  , bgaBlockGenerator :: s -> Anchor block -> SlotNo -> (Maybe block, s)
    -- ^ block generator

  , bgaSeed           :: s
  }

-- | Do not generate blocks.
--
relayBlockGenerationArgs :: DiffTime -> seed -> BlockGeneratorArgs block seed
relayBlockGenerationArgs bgaSlotDuration bgaSeed =
  BlockGeneratorArgs
    { bgaSlotDuration
    , bgaBlockGenerator = \seed _ _ -> (Nothing, seed)
    , bgaSeed
    }


-- | Generate a block according to given probability.
--
randomBlockGenerationArgs :: DiffTime
                          -> StdGen
                          -> Int -- between 0 and 100
                          -> BlockGeneratorArgs Block StdGen
randomBlockGenerationArgs bgaSlotDuration bgaSeed quota =
  BlockGeneratorArgs
    { bgaSlotDuration
    , bgaBlockGenerator = \seed anchor slot ->
                            let block = ConcreteBlock.fixupBlock anchor
                                      . ConcreteBlock.mkPartialBlock slot
                                      -- TODO:
                                      --  * use ByteString, not String;
                                      --  * cycle through some bodies
                                      --
                                      $ ConcreteBlock.BlockBody (BSC.pack "")
                            in case randomR (0, 100) seed of
                                (r, seed') | r <= quota ->
                                             (Just block, seed')
                                           | otherwise  ->
                                             (Nothing, seed')
    , bgaSeed
    }

data NodeKernel header block m = NodeKernel {
      -- | upstream chains
      nkClientChains
        :: StrictTVar m (Map NtNAddr (StrictTVar m (Chain header))),

      -- | chain producer state
      nkChainProducerState
        :: StrictTVar m (ChainProducerState block),

      nkFetchClientRegistry :: FetchClientRegistry NtNAddr header block m,

      nkPeerSharingRegistry :: PeerSharingRegistry NtNAddr m,

      nkChainDB :: ChainDB block m
    }

newNodeKernel :: MonadSTM m => m (NodeKernel header block m)
newNodeKernel = NodeKernel
            <$> newTVarIO Map.empty
            <*> newTVarIO (ChainProducerState Chain.Genesis Map.empty 0)
            <*> newFetchClientRegistry
            <*> newPeerSharingRegistry
            <*> ChainDB.newChainDB

-- | Register a new upstream chain-sync client.
--
registerClientChains :: MonadSTM m
                     => NodeKernel header block m
                     -> NtNAddr
                     -> m (StrictTVar m (Chain header))
registerClientChains NodeKernel { nkClientChains } peerAddr = atomically $ do
    chainVar <- newTVar Genesis
    modifyTVar nkClientChains (Map.insert peerAddr chainVar)
    return chainVar


-- | Unregister an upstream chain-sync client.
--
unregisterClientChains :: MonadSTM m
                       => NodeKernel header block m
                       -> NtNAddr
                       -> m ()
unregisterClientChains NodeKernel { nkClientChains } peerAddr = atomically $
    modifyTVar nkClientChains (Map.delete peerAddr)

withSlotTime :: forall m a.
                ( MonadAsync         m
                , MonadDelay         m
                , MonadMonotonicTime m
                )
             => DiffTime
             -> ((SlotNo -> STM m SlotNo) -> m a)
             -- ^ continuation which receives a callback allowing to block until
             -- the given slot passes.  The stm action returns the slot current
             -- slot number.
             -> m a
withSlotTime slotDuration k = do
    let start = Block.SlotNo 0
    slotVar <- newTVarIO start
    let waitForSlot :: SlotNo -> STM m SlotNo
        waitForSlot slot = do
          current <- readTVar slotVar
          check (current >= slot)
          return current
    withAsync (loop slotVar (succ start)) $ \_async -> k waitForSlot
  where
    loop :: StrictTVar m SlotNo
         -> SlotNo
         -> m Void
    loop slotVar = go
      where
        go :: SlotNo -> m Void
        go next = do
          t <- getMonotonicTime
          let delay = abs
                    $ Time (slotDuration * fromIntegral (Block.unSlotNo next))
                      `diffTime` t
          threadDelay delay
          atomically $ writeTVar slotVar next
          go (succ next)

-- | Node kernel erros.
--
data NodeKernelError = UnexpectedSlot !SlotNo !SlotNo
  deriving (Typeable, Show)

instance Exception NodeKernelError where


-- | Run chain selection \/ block production thread.
--
withNodeKernelThread
  :: forall block header m seed a.
     ( MonadAsync         m
     , MonadMonotonicTime m
     , MonadTimer         m
     , MonadThrow         m
     , MonadThrow    (STM m)
     , HasFullHeader block
     )
  => BlockGeneratorArgs block seed
  -> (NodeKernel header block m -> Async m Void -> m a)
  -- ^ The continuation which has a handle to the chain selection \/ block
  -- production thread.  The thread might throw an exception.
  -> m a
withNodeKernelThread BlockGeneratorArgs { bgaSlotDuration, bgaBlockGenerator, bgaSeed }
                     k = do
    kernel <- newNodeKernel
    withSlotTime bgaSlotDuration $ \waitForSlot ->
      withAsync (blockProducerThread kernel waitForSlot) (k kernel)
  where
    blockProducerThread :: NodeKernel header block m
                        -> (SlotNo -> STM m SlotNo)
                        -> m Void
    blockProducerThread NodeKernel { nkChainProducerState, nkChainDB }
                        waitForSlot
                      = loop (Block.SlotNo 1) bgaSeed
      where
        loop :: SlotNo -> seed -> m Void
        loop nextSlot seed = do
          -- update 'ChainProducerState' whatever happens first:
          -- - generate a new block for the next slot
          -- - a longer candidate chain is available
          (nextSlot', seed') <- atomically $ runFirstToFinish $

               --
               -- block production
               --
               FirstToFinish
                 ( do currentSlot <- waitForSlot nextSlot
                      -- we are not supposed to block, apart from the above
                      -- blocking call to 'waitForSlot'.
                      when (currentSlot /= nextSlot)
                         $ throwIO (UnexpectedSlot currentSlot nextSlot)
                      cps@ChainProducerState { chainState } <-
                        readTVar nkChainProducerState
                      let anchor :: Anchor block
                          anchor = Chain.headAnchor chainState
                      -- generate a new block, which fits on top of the 'anchor'
                      case bgaBlockGenerator seed anchor currentSlot of
                        (Just block, seed')
                          |    Block.blockPoint block
                            >= Chain.headPoint chainState
                          -> do
                             -- Forged a block add it to our ChainDB this will
                             -- make the new block available for computing
                             -- longestChain
                             ChainDB.addBlock block nkChainDB

                             -- Get possibly new longest chain
                             longestChain <-
                               LazySTM.readTVar (cdbLongestChainVar nkChainDB)

                             -- Switch to it and update our current state so we
                             -- can serve other nodes through block fetch.
                             let cps' = switchFork longestChain cps
                             writeTVar nkChainProducerState
                                       cps' { chainState = longestChain }
                          >> return (succ nextSlot, seed')
                        (_, seed')
                          -> return (succ nextSlot, seed')
                 )

               --
               -- chain selection
               --
            <> FirstToFinish
                 ( do
                      -- Get our current chain
                      cps@ChainProducerState { chainState } <-
                        readTVar nkChainProducerState
                      -- Get what ChainDB sees as the longest chain
                      longestChain <-
                        LazySTM.readTVar (cdbLongestChainVar nkChainDB)

                      -- Only update the chain if it's different than our current
                      -- one, else retry
                      check $ Chain.headPoint chainState
                           /= Chain.headPoint longestChain

                      -- If it's different, switch to it and update our current
                      -- state so we can serve other nodes through block fetch.
                      let cps' = switchFork longestChain cps
                      writeTVar nkChainProducerState
                                cps' { chainState = longestChain }
                      writeTVar nkChainProducerState cps'
                      -- do not update 'nextSlot'; This stm branch might run
                      -- multiple times within the current slot.
                      return (nextSlot, seed)
                 )
          loop nextSlot' seed'
