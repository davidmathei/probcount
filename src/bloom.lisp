;;; (c) David Mathei.  See LICENSE file for permissions
(in-package :bloom)
;;;============= Bloom filter ========================================================
;;; see http://blogs.oracle.com/jrose/entry/bloom_filters_in_a_nutshell
;;;     for entropy-based reasoning concerning bloom filters.
;;;     he arrives at: bitsize ~ #items * #hashes * ln(2) 
;;; http://www.dis.uniroma1.it/~leon/didattica/seminari/bloom.pdf
;;;     and (broder/mitzenmacher) recommend though to choose 
;;;     bitsize = c * #items
;;;     with c being a small natural number.
;;;
;;; TODO OR'ing filters.
;;; TODO implement set union/intersection
;;; TODO extend for use with deletions.
;;;      deletions with count array 4 bits per counter mosltly anoug (broder/mitzenmacher)
;;; TODO we create now a vector of bitmap indices. it is more efficient if these indices
;;;       get writtebn to the bitmap immediately, resp, read. In the case of #'contains
;;;       we might even abridge the generation of more hashes if the first 0 is found.
;;;       fix that. 

(defclass bloom-filter ()
  ((bitmap :accessor bitmap 
	   :initarg :bitmap)
   (hash-fun :accessor hash-fun 
	     :initarg :hash-fun)))


(defun make-bloom-filter (size keys)
  (make-instance 'bloom-filter 
		 :bitmap (make-array size 
				     :element-type 'bit 
				     :initial-element 0)
		 ;; hardcoded for now. FIXME
		 :hash-fun (make-double-hash-oat-fnv keys size)))


(defmethod add ((bloom bloom-filter) octets)
  (loop for i across (funcall (hash-fun bloom) octets) 
     do (setf (aref (bitmap bloom) i) 
	      1)))

(defmethod contains ((bloom bloom-filter) octets)
  "Test, whether an item is known. 
  False positives are possible, false negatives not."
  (loop for i across (funcall (hash-fun bloom) octets) 
       until (zerop (aref (bitmap bloom) i))
       finally (return (not (zerop (aref (bitmap bloom) i))))))

(defmethod saturation ((bloom bloom-filter))
  "How full is this filter?"
  (* 1.0 (/ (loop for b across (bitmap bloom) 
	       if (= 1 b) count b)
	    (array-dimension (bitmap bloom) 0))))
;;;=============== HASH FUNCTIONS ==========================================
;;; instead of multiple hash functions multiple linear combinations of 
;;; two hash functions can be used ("double hash").
;;; the combination used here is
;;;     Gi(x) = H1(x) + i*H2(x) + f(i)
;;; for efficiency reasons you might want to hardcode your own double hash
;;; instead of having the indirection of multiple funcalls
        
		  
(defun make-double-hash-fn(num-keys modulus fn-1 fn-2)
"return double hash function using the two specified hash functions"
  (lambda (octets)
    (let ((result (make-array num-keys :element-type '(unsigned-byte 32))))
      (dotimes (i num-keys)
	(setf (aref result i) 
	      (mod (ldb (byte 32 0) (+ (funcall fn-1 octets) 
			   (* i (funcall fn-2 octets)) 
			   (* i i))) 
		   modulus)))
      result)))


(defun make-double-hash-oat-fnv (num-keys modulus)
"double hash function using one-at-a-time and fnv hash"
 (lambda (octets) 
   (let ((result (make-array num-keys :element-type '(unsigned-byte 32))) 
	 (h1 (oat octets)) 
	 (h2 (fnv32 octets))) 
    (dotimes (i num-keys)
      (setf (aref result i)  
	    (mod (ldb (byte 32 0) 
		      (+ h1 
			 (* i h2 ) 
			 (* i i))) 
		 modulus))) 
    result)))
