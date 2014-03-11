;;; (c) David Mathei.  See LICENSE file for permissions
(in-package :probcount)
(make-random-state)      
;;; ==================================================================
;;; Data structure to count approximatly unique items.
;;; See http://en.wikipedia.org/wiki/Approximate_counting_algorithm
;;; or http://highlyscalable.wordpress.com/2012/05/01/probabilistic-structures-web-analytics-data-mining/
;;; or http://highscalability.com/blog/2012/4/5/big-data-counting-how-to-count-a-billion-distinct-objects-us.html
;;; The hashes in this package Our hashes give us maximally 2^32-1 offsets, 
;;; with a utilization of 0.5 we might count 3.0e9 items.
;;; That seems sufficient to count disticnt API keys ... 
;;; ==================================================================




(defclass linear-prob-counter () 
  ((kbsize 
    :initarg :kbsize
    :initform 1
    :accessor kbsize)
   (num-bits :accessor num-bits) 
   (num-free-bits :accessor num-free-bits) 
   (bitmap :accessor bitmap)
   (hash-fun 
    :initarg :hash-fun 
    :accessor hash-fun)))

(defun modexp (base exp m)
  "modular exponentiation. we dont check here if b < m"
  (do
   ((e exp 
       (ash e -1))
    (result 1)
    (b (mod base m) 
       (mod (* b b) m)))
   ((zerop e) result)
    (unless (zerop (logand 1 e))
      (setf result (mod (* result b) m)))))

(defun is-prime-fermat (p &optional (trials 4))
  "primality test based on fermat's little theorem"
  (let ((p-1 (1- p)))
    (do* ((i trials (1- i))
	 (a (1+ (random p-1)) 
	    (1+ (random p-1)))
	 (x (modexp a p-1 p)
	    (modexp a p-1 p)))
	((or (zerop i) ;; we actually test trials+1 iff all x's were 1 until now. no harm.
	     (not (= 1 x)))
	 (= 1 x)))))

(defun find-next-prime-below (num)
  (do ((n (- (logior num 1) 2) ; make odd
	  (- n 2))) 
      ((is-prime-fermat n) n)))


(defmethod initialize-instance :after ((counter linear-prob-counter) &key)
  (let ((bit-size (find-next-prime-below
		   (ash (kbsize counter) 
			(+ 3 10))))) 
    (setf (num-bits counter) bit-size)
    (reset counter)))

(defmethod cardinality ((counter linear-prob-counter))
  "the estimated size of the set"
  (round (* (num-bits counter) 
	    (log (/ (num-bits counter) 
		    (1+ (num-free-bits counter)))))))

(defmethod utilization ((counter linear-prob-counter))
  "The filling-degree of the counter"
  (* 1.0 
     (/ (- (num-bits counter) 
	   (num-free-bits counter)) 
	(num-bits counter))))

(defmethod saturated ((counter linear-prob-counter))
  "check if the counter is 'full'"
  (zerop (num-free-bits counter)))

(defmethod offer ((counter linear-prob-counter) hashable-item)
  "'Count' one item. Currently process only ctet-vectors."
  (when (not (saturated counter))
    (let ((offs (mod (funcall (hash-fun counter) 
			      hashable-item) 
		     (num-bits counter))))
      (when (zerop (aref (bitmap counter) offs))
	(decf (num-free-bits counter))
	(setf (aref (bitmap counter) offs) 1)))))

(defmethod reset ((counter linear-prob-counter))
  "Make a fresh counter. This method is also called during initialization"
  (setf (num-free-bits counter) (num-bits counter)
	(bitmap counter) (make-array (ash (kbsize counter) 13) 
				       :element-type 'bit 
				       :initial-element 0)))


	  
(defun make-linear-prob-counter (&key (size-in-kb 1) (hash-fun #'fnv32))
  (make-instance 'linear-prob-counter :kbsize size-in-kb :hash-fun hash-fun))
