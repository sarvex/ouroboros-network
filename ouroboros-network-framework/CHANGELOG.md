# Revision history for ouroboros-network-framework

## next version

### Breaking changes

* `MuxProtocolBundle` type alias was removed, since it was just reduced to
  a list of 'MiniProtocol's.

* Added `ExpandedInitiatorContext`, `MinimalInitiatorContext` and
  `ResponderInitiatorContext` types in a new module:
  `Ouroboros.Network.Context`.  The module also re-exports `ConnectionId`,
  `IsBigLedgerPeer` and `ControlMessageSTM` thus an unqualified import might
  cause some warnings.

* `RunMiniProtocol` now contains callbacks which receive a context.  The type
  is polymorphic over initiator and responder contexts.  We also provide type
  aliases for `RunMiniProtocolWithExpandedCtx` and
  `RunMiniProtocolWithMinimalCtx` which instatiate initiator and responider
  contexts.

* Added `OuroborosBundleWithExpandedCtx` and `OuroborosBundleWithMinimalCtx`
  type aliases.

* Added `OuroborosApplicationWithMinimalCtx` and
  `OuroborosApplicationWithExpandedCtx` type aliases.

* Added `contramMapInitiatorCtx` which contramaps the initiator context of an
  `OuroborosApplication`.

* Added `fromOuroborosBundle` which creates `OuroborosApplication` from
  `OuroborosBundle` by forgetting the hot / warm / established distinction
  between all mini-protocols.

* Removed `MuxBundle` and `mkMuxApplicationBundle` which is no longer needed.

* Due to the above changes the following APIs changed their type signatures:

  - `Ouroboros.Network.Socket.connectToNode`
  - `Ouroboros.Network.Socket.connectToNode'`
  - `Ouroboros.Network.Socket.connectToNodeSocket`
  - `Ouroboros.Network.Socket.SomeResponderApplication`
  - `Ouroboros.Network.Socket.withServerNode`
  - inbound governor API

* `MuxPeer` changed it's kind and it was renamed to `MiniProtocolCb`, the old
  type is still provided but deprecated.  The `MuxPeerRaw` constructor was
  renamed to `MiniProtocolCb` (the old one is still provided but deprecated).
  `MuxPeer` and `MuxPeerPipelined` constructors also changed its type and are
  now deprecated.  Use `mkMiniProtocolCbFromPeer` and
  `mkMiniProtocolCbFromPeerPipelined` instead.

  Also note that even the deprecated constructors have changed their types.

* `runMuxPeer` change its type but also is now deprecated in favour of `runMiniProtocolCb`.  The latter
  receives two arguments: the context and `Network.Mux.Channel.Channel` rather
  than `Ouroboros.Network.Channel.Channel` (no need to use
  `Ouroboros.Network.Channel.fromChannel`).  `runMuxPeer` accepts the context (added argument) and
  `Ouroboros.Network.Channel.Channel`.

### Non-breaking changes

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
