#What it is
A Common Lisp implementation of a Bloom Filter anmd a Linear Probabilistic Counter. 
Above that some simple 32 bit hash functions and a test suite to play around with 
parameters and to allow for basic profiling.

Bloom filters represebt a set and allow to determine if an item is present in it. 
If an item cannot be found it is definitely not there, false positives are possible though.
Their probability is tunable. 

A linear probabilistic counter aims to approximately count unique items in a set. 
Like in the case of Bloom filters, items are added by means of their hash value, 
the counter estimates the cardinality of the set.
#How to use it
The bloom filter:

      (in-package :bloom)  
      (let* ((number-of-bits (* 16 1024 8))
             (number-of-hash-keys 8)
             (bloom (make-bloom-filter number-of-bits number-of-hash-keys))
        (add bloom (babel:string-to-octets "bloom"))
        (contains bloom (babel:string-to-octets "bloom")))
        
The LPC:

     (in-package :probcount)
     (let* ((size-in-kb 1)
            (hash-fn 'fnv32)
            (lpc (make-linear-prob-counter size-in-kb hash-fn)))
        (offer lpc (babel:string-to-octets "lpc"))
        (princ (cardinality lpc)))
    

## test
Run with 

     (asdf:operate 'asdf:test-op :probcount)

#Todos / Shortcomings
* 32 bit hashes might not be sufficient to implement big counters or bigger bloom filters.
* Profiling tests are incomplete.
* Most of the hash functions do not go well with short keys.
* The Bloom filter could support deletion.
#License
(c) David Mathei.  See LICENSE file for permissions
