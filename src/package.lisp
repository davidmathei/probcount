;;; (c) David Mathei.  See LICENSE file for permissions
(in-package :cl-user)

(defpackage :probcount.hashes
  (:nicknames "PCH")
  (:use :cl)
  (:export 
    :murmur32  
    :djb2 
    :sdbm 
    :rot 
    :sax 
    :fnv32 
    :oat
    :superfast
    :python-string
    :jenkins))

(defpackage :probcount
  (:nicknames "PCNT")
  (:use :cl 
        :probcount.hashes)
  (:export 
     :make-linear-prob-counter
     :offer
     :cardinality
     :utilization
     :reset
     :saturated
     :modexp
     :is-prime-fermat
     :find-next-prime-below))

(defpackage :bloom
  (:use :cl 
        :probcount.hashes)
  (:export 
     :make-bloom-filter
     :add
     :contains
     :saturation
     :make-double-hash-fn
     :make-double-hash-oat-fnv))
