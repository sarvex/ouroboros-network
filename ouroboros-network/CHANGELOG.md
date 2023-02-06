# Revision history for ouroboros-network

## next

### Breaking

* `TraceLedgerPeers` renamed some constructors:
    - `PickedPeer  -> PickedLedgerPeer`
    - `PickedPeers -> PickedLedgerPeers`;
  added new ones:
    - `PickedBigLedgerPeer`
    - `PickedBigLedgerPeers`;
  and `FetchingNewLedgerState` constructor has a new field: number of big
  ledger peers.

## 0.1.0.0 -- 2018-09-20

* Initial experiments and prototyping
