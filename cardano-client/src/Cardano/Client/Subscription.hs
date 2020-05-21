just break it
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Client.Subscription (
  subscribe
  ) where

import           Data.Proxy
import           Data.Void (Void)
import qualified Data.ByteString.Lazy as BSL
import           Ouroboros.Consensus.Config (TopLevelConfig, configBlock)
import           Ouroboros.Consensus.Network.NodeToClient (clientCodecs, ClientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (
                    nodeToClientProtocolVersion , supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.Run (RunNode, nodeNetworkMagic)
import           Ouroboros.Network.Mux (AppType (..), OuroborosApplication)
import           Ouroboros.Network.NodeToClient (ClientSubscriptionParams (..),
                    ConnectionId, LocalAddress,
                    NodeToClientProtocols (..),
                    NetworkClientSubcriptionTracers,
                    NodeToClientVersionData (..),
                    ncSubscriptionWorker,
                    newNetworkMutableState,
                    versionedNodeToClientProtocols)

import           Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToClientVersion)
import           Ouroboros.Network.Protocol.Handshake.Version (DictVersion, Versions, foldMapVersions)
import qualified Ouroboros.Network.Snocket as Snocket

import qualified Ouroboros.Network.NodeToClient (NodeToClientVersion)
import           Control.Monad.Class.MonadST (MonadST)
import           Prelude
import           Ouroboros.Consensus.Block.Abstract (getCodecConfig)
  
subscribe ::
  ( RunNode blk , MonadST m )
  => Snocket.LocalSnocket
  -> TopLevelConfig blk
  -> NetworkClientSubcriptionTracers
  -> ClientSubscriptionParams ()
  -> (NodeToClientVersion blk
      -> ClientCodecs blk m
      -> NodeToClientProtocols 'InitiatorApp BSL.ByteString IO x y)
  -> IO Void
subscribe
  sn
  topLevelConfig
  tracers
  subscriptionParams
  protocols
  = do
    networkState <- newNetworkMutableState
    ncSubscriptionWorker
        sn
        tracers
        networkState
        subscriptionParams
        (versionedProtocols (Proxy :: Proxy blk) topLevelConfig protocols)

versionedProtocols ::
  (RunNode blk, MonadST m)
  => Proxy blk
  -> TopLevelConfig blk
  -> (NodeToClientVersion blk
      -> ClientCodecs blk m
      -> NodeToClientProtocols appType bytes IO a b)
  -> Versions
       Ouroboros.Network.NodeToClient.NodeToClientVersion
       DictVersion
       (ConnectionId LocalAddress
          -> OuroborosApplication appType bytes IO a b)
versionedProtocols blkProxy topLevelConfig p
  = foldMapVersions applyVersion $ supportedNodeToClientVersions blkProxy
  where
    applyVersion v =
      versionedNodeToClientProtocols
        (nodeToClientProtocolVersion blkProxy v)
        (NodeToClientVersionData { networkMagic = nodeNetworkMagic topLevelConfig })
        (p v $ clientCodecs ( getCodecConfig $ configBlock topLevelConfig) v)
