# Revision history for ouroboros-network

## next

### Breaking

* Introduced big ledger peers to the outbound governor.  The breaking changes include:
  - Added new targets to `PeerSelectionTargets` data type

  - Added `requestBigLedgerPeers` to `PeerSelectionActions`.

  - `establishPeerConnection` and `ativatePeerConnection` receive an
    `IsLedgerPeer` argument (which is then passed to mini-protocols via
    `ExtendedInitiatorContext`.

  - The `PeerSelectionState` contains new fields to support big ledger peers.

  - Modified `PickPolicy` type, it is now parametrised by monad `m` rather than
    by an `stm` monad.  With this change the type alias can be used in
   `pickPeers` type signature.

  - `TraceLedgerPeers` renamed some constructors:
      - `PickedPeer  -> PickedLedgerPeer`
      - `PickedPeers -> PickedLedgerPeers`;
    added new ones:
      - `PickedBigLedgerPeer`
      - `PickedBigLedgerPeers`;
    and `FetchingNewLedgerState` constructor has a new field: number of big
    ledger peers.

* Propagated changes from `ouroboros-network-framework` related to the
  introduction of initiator and responder contexts to `RunMiniProtocol` data
  type. These changes include breaking changes to the following APIs:

  - `Ouroboros.Network.Diffusion` is using: `OuroborosBundleWithExpandedCtx`
    for node-to-node applications, `OuroborosApplicationWithMinimalCtx` for
    node-to-client responders.
  - `Ouroboros.Network.NodeToNode` exports `MinimalInitiatorContext` and
    `ExpandedInitiatorContext` data types.
  - `Ouroboros.Network.NodeToClient` exports `MinimalInitiatorContext` and
    `ResponderContext` data types.
  - `Ouroboros.Network.NodeToNode.NodeToNodeProtocols`,
    `Ouroboros.Network.NodeToNode.nodeToNodeProtocols`,
    `Ouroboros.Network.NodeToNode.versionedNodeToClientProtocols`,
    `Ouroboros.Network.NodeToNode.withServer`  were modified.
  - `Ouroboros.Network.NodeToClient.NodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.nodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.versionedNodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.withServer`,
    `Ouroboros.Network.NodeToClient.ipSubscriptionWorker`,
    `Ouroboros.Network.NodeToClient.dnsSubscriptionWorker` were modified.

## 0.1.0.0 -- 2018-09-20

* Initial experiments and prototyping
