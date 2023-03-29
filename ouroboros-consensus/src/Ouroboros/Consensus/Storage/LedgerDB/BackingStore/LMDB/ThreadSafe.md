# BS (BackingStore)

## bsClose
* BS should not be closed or closing
* BS closing should be atomic, so all BS and BSVH operations should fail
* All open BSVH should be closed in the process

## bsCopy
* BS should not be closed or closing
* can be used concurrently with other transactions.

## bsWrite
* BS should not be closed or closing
* bsWrite will block other bsWrites

## bsValueHandle
* BS should not be closed or closing
* Generating BSVH id should be atomic
* Adding new BSVH to open BSVH should be unique

# BSVH (BackingStoreValueHandle)

* Should not be shared among threads, so no concurrent access to the same BSVH
  allowed.

## bsvhClose
* BS should not be closed or closing
* BSVH should not be closed or closing
* BSVH closing should be atomic, so all other BSVH operations on the same BSVH
  should fail

## bsvhRead / bsvhRangeRead
* BS should not be closed or closed
* BSVH should not be closed or closing

# Conclusions

* Closing BS should not proceed during an unfinished BS or BSVH operation
* All BS and BSVH operations should fail when BS is closing or closed
* BSVH creation should be unique

* Closing BSVH should not proceed during an unfinished same-id BSVH operation
* All same-id BSVH operations should fail when BSVH is closing or closed
* Concurrent access to one BSVH is not allowed