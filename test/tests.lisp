;;; (c) David Mathei.  See LICENSE file for permissions
(in-package :probcount-test)
;; run with (asdf:operate 'asdf:test-op :probcount)
;;========== helper functions ========================
(defun generate-octet-vector (maxlen) 
  (apply #'vector
	 (loop repeat (1+ (random maxlen)) 
	    collecting (random 256))))

(defun random-long-octets (count maxlen)
  (loop repeat count collecting (generate-octet-vector maxlen)))

(defparameter *octetlist* nil)
(defparameter *num-tokens* 0)



;;========== test hashes =============================
;; TODO avalanching tests.
;; TODO tests with uniform domains (URL's)
;; TODO tests with long keys
;; TODO split test into more small tests

(defun hash-into-buckets (num-buckets hash-fn octets-list) 
"mind you that the length of the list is relevant, the list should also be unique"
  (let ((buckets (make-array num-buckets 
			     :element-type 'fixnum 
			     :initial-element 0)))
    (dolist (octets octets-list)
      (let ((offs (mod (funcall hash-fn octets) 
		       num-buckets)))
	(incf (aref buckets offs))))
    buckets))

(defun max-collision (bins)
  (loop for b across bins 
     maximizing b))

(defun empty-bins (bins) 
  (loop for b across bins 
     if (zerop b) 
     counting b))


(defun chi-squared (bins bin-ratio num-bins) 
 "chi squared test over the variance. reported in stddevs
  http://www.kfki.hu/~kadlec/sw/netfilter/ct2/"
  (let ((chi 0.0))
    (loop for b across bins
       do 
	 (let ((diff (- b bin-ratio)))
	   (setf chi (+ chi (* diff diff)))))
    ;; pulled out of the summming
    (setf chi (/ chi bin-ratio))
    ;; convert to stddev
    (/ (- chi num-bins) (sqrt num-bins))))


(defun test-one-hash (hashfun num-bins num-items)
  (let ((bin-ratio (/ num-items num-bins))
	(bins (hash-into-buckets num-bins hashfun *octetlist*)))
    (let ((longest-chain (max-collision bins))
	   (empty-bins (empty-bins bins))
	   (chisqd (chi-squared bins bin-ratio num-bins))) 
      (format 't "Evaluating ~a with ~a itens in ~a bins:~%" 
	      hashfun 
	      num-items 
	      num-bins)
      (format 't "empty: ~a longest: ~a chi^2: ~a~%~%" 
	      empty-bins 
	      longest-chain 
	      chisqd))))

(defun test-hashes () 
"All hashes are evaluated with respect to their value distribution. 
 The test is done with ascii tokens of common word length.
 This must give just an idea for comparison of performance.
 Thorough tests must be mnore directed to the domain at hand: IP adresses or URI's.
 "
  (let ((num-bins 1023)
	(hashfuns '(murmur32 ; performs bad on short octet-vectors
		    djb2 
		    sdbm 
		    rot 
		    sax 
		    fnv32 
		    oat 
		    python-string 
		    superfast 
		    jenkins)))
    (dolist (h hashfuns)
      (test-one-hash h num-bins *num-tokens*))
    ; there is no real failure in this test 
    't))
;;========== test bloom ==============================
(defun test-bloom () 
  (let ((evens (make-bloom-filter (* 16 1024 8) 8))
	(odds (make-bloom-filter (* 16 1024 8) 8)))
    (dolist (o *octetlist*)
      (let ((even? (zerop (mod (length o) 2))))
	(if even? 
	    (add evens o)
	    (add odds o))))
    (let ((ok 0)
	  (fpos 0)
	  (fneg 0))
      (dolist (o *octetlist*)
	(let ((even? (zerop (mod (length o) 2)))
	      (in-even? (contains evens o))
	      (in-odd? (contains odds o)))
	  (when (and even? in-even?) (incf ok))
	  (when (and even? (not in-even?)) (incf fneg))
	  (when (and even? in-odd?) (incf fpos))	  
	  (when (and (not even?) in-even?) (incf fpos))
	  (when (and (not even?) in-odd?) (incf ok))
	  (when (and (not even?) (not in-odd?)) (incf fneg))))
      (format 't "Bloom says: saturation even: ~a odd: ~a~%" 
	      (bloom:saturation evens) 
	      (bloom:saturation odds))
      (format 't "~a ok ~a false positives and ~a false negatives~%~%"  
	      ok 
	      fpos 
	      fneg)
      ;; this can never be positive if implemented correctly.
      (zerop fneg))))

;;========== test lpc ================================
;; TODO: more tests with different kind of non-unique items (IP-adresses, API keys etc)
(defun test-lpc () 
  (let ((counter (make-linear-prob-counter)))
    (dolist (token *octetlist*)
      (offer counter token))
    (format 't "lpc after feeding ~a unique items: card.: ~a util.: ~a~%" 
	    (length *octetlist*) 
	    (cardinality counter) 
	    (utilization counter))
    (reset counter)
    (format 't "lpc after reset: card.: ~a util.: ~a~%" 
	    (cardinality counter) 
	    (utilization counter)) 
    't))





;;=====================================================
(defun run-tests () 
  (let* ((*octetlist* (random-long-octets 10000 200))
	 (*num-tokens* (length *octetlist*))) 
    (or  
     (reduce (lambda (acc f) 
	       (and (funcall f) 
		    acc)) 
	     (list #'test-hashes 
		   #'test-bloom 
		   #'test-lpc) 
	     :initial-value 't)
     (format 't "there were errors~%"))))
