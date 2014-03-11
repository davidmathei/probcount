;;; (c) David Mathei.  See LICENSE file for permissions
(in-package :probcount.hashes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some 32 bit hashing functions.
;;; All functions take a vector of octets as returned for instance by babel:string-to-octets.
;;; Some still use them internally after converting to a list.
;;; An abundance of u32 calls is used to keep also intermediary results in a 32 bit register.
;;; FIXME : sbcl's optimizer has problems with that:  #'ash expects 'fixnum, not 'integer. 
;;; FIXME : use u32 more sparingly
;;; CHECK : sbcl prefers logand xFFFFFFFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(nil)
(declaim (inline rot32 double-hash bytes-to-word32)
	 (optimize (speed 3) 
		   (safety 0) 
		   (space 0) 
		   (debug 0) 
		   (compilation-speed 0)))

(defmacro u32 (&body body)
  "Force x into an uit32 word, implicitely applying mod 32. 
   logand optimization in sbcl is limited ('* is excluded)"
  `(ldb (byte 32 0) ,@body))

(defun rot32 (x k)
  "rotate x left by 32 bits with a 32 bit modulus"
  (declare (type (unsigned-byte 32) x)
	   (type (unsigned-byte 6) k))
  (u32 (logior (ash x k) (ash x (- k 32))))) 

(defun bytes-to-word32 (d c b a)
  "collects consecutve bytes d,c,b,a into an unit32 word. Big endian."
  (declare (type (unsigned-byte 8) a b c d))
  (u32 (logior (ash a 24)
	       (ash b 16)
	       (ash c 8)
	       d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Most of these functions look like a clear cut case for #'reduce, but on a vector 
;;; this performs up to 20 times slower and conses until daybreak.
;;; TODO Some of these are candidates for folding incomimg octets into uint32. 
;;;      Try & measure in tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun djb2 (octets)
  "Hash function for strings. The octets are used for hashing, no 32bit words."
  (declare (type (vector (unsigned-byte 8)) octets))
  (let ((hash 5381))
    (dotimes (i (length octets))
      (setf hash (u32 (logxor (aref octets i) (1+ (ash hash 5))))))
    hash))

(defun sdbm (octets)
"Variant of BerkeleyDB hash algo."
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((hash 0))
    (dotimes (i (length octets))
      (setf hash (u32 (- (+ (aref octets i) (ash hash 6) (ash hash 16)) hash))))
    hash))
       
(defun rot (octets)
"Simple rotation."
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((hash 0))
    (dotimes (i (length octets))
      (setf hash (u32 (logxor (ash hash 4) (ash hash -26) (aref octets i)))))
    hash))


(defun sax (octets)
"ShiftAddXor ... "
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((hash 0))
    (dotimes (i (length octets))
      (setf hash (u32 (logxor hash (+ (ash hash 5) (ash hash -2) (aref octets i))))))
    hash))

(defun fnv32 (octets)
"Fnv hash"
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((hash  2166136261)
	(mlt 16777619))
    (dotimes (i (length octets))
      (setf hash (logxor (u32 (* hash mlt)) (aref octets i))))
    hash))


(defun oat (octets)
"one at a time, jenkins"
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((hash 0))
    (dotimes (i (length octets))
      (setf hash (u32 (+ hash (aref octets i)))
	    hash (u32 (+ hash (ash hash 10)))
	    hash (u32 (logxor hash (ash hash -6)))))
    (setf hash (u32 (+ hash (ash hash 3)))
	  hash (logxor hash (ash hash -11))
	  hash (u32 (+ hash (ash hash 15))))))

;;FIXME: should operaton on vector, not list
(defun python-string (octets)
"string hash used by python-the-language"
  (let* ((octet-list (coerce octets 'list))
	 (value (u32 (ash (first octet-list) 7))))
    (loop for c in octet-list
	 do (setf value (u32 (logxor (* 1000003 value) c))))
    (u32 (logxor value (length octet-list)))))

(defun murmur32 (octets)
"murmur2 for 32 bit"
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((num-bytes (length octets))) 
    (multiple-value-bind (num-blocks32 len-tail) (truncate num-bytes 4)
      (let ((h 0) 
	    (tailpos (* 4 num-blocks32)))
	;; process all blocks of 32 bits
	(dotimes (i num-blocks32)
	  (let* ((bi (* 4 i))
		(blockval (bytes-to-word32 (aref octets bi)
					   (aref octets (+ bi 1))
					   (aref octets (+ bi 2))
					   (aref octets (+ bi 3)))))
	    (setf blockval (u32 (* blockval #xcc9e2d51))
		  blockval (u32 (logior (ash blockval 15) (ash blockval (- 15 32))))
		  blockval (u32 (* blockval #x1b873593))
		  h (logxor h blockval)
		  h (u32 (logior (ash h 13) (ash h (- 13 32))))
		  h (u32 (+ (* h 5) #xe6546b64)))))
       ;; process the tail
	(let ((blockval 0))
	  
	  (when (= 3 len-tail) 
	    (setf blockval (u32 
				(logxor blockval 
					(ash (aref octets 
						   (+ 2 tailpos)) 
					     16)))))
	  (when (= 2 len-tail) 
	    (setf blockval (u32 (logxor blockval 
						    (ash (aref octets 
							       (+ 1 tailpos)) 
							 8)))))
	  
	  (when (= 1 len-tail) 
	    (setf blockval (u32 (logxor blockval (aref octets tailpos)))
		  blockval (u32 (* blockval #xcc9e2d51))
		  ;; typo ?
		  blockval (u32 (logior (ash blockval 13) (ash blockval (- 15 32))))
		  blockval (u32 (* blockval #x1b873593))
		  h (logxor h blockval))))
	;; postprocess
	(setf h (logxor h num-bytes)
	      h (logxor h (ash h (- 16)))
	      h (u32 (* h #x85ebca6b))
	      h (logxor h (ash h (- 13)))
	      h (u32 (* h #xc2b2ae35))
	      h (logxor h (ash h (- 16))))))))




;; FIXME Rewrite to vector usage + index loop
(defun superfast(octets)
 "originally (paul hsieh) designed for a stream of 8 or 16 bit bytes."
  (let ((hash 0)
	(tmp 0))
    (declare (type (unsigned-byte 32) hash tmp))
    (loop for cons on (coerce octets 'list) by #'cddr
	 do (let ((x (first cons))
		  (y (if (second cons) 
		       (second cons)
		       0)))
	      (declare (type (unsigned-byte 32) x y))
	      (if (not (zerop y))
		  (setf hash (u32 (+ hash x))
			tmp  (u32 (logxor (ash y 11) hash))
			hash (u32 (logxor (ash hash 16) tmp))
			hash (u32 (+ hash (ash hash -11))))
		  (setf hash (u32 (+ hash y))
			hash (u32 (logxor hash (ash hash -11)))
			hash (u32 (+ hash (ash hash -17)))))))
    (setf hash (u32 (logxor hash (ash hash 3)))
	  hash (u32 (+ hash (ash hash -5)))
	  hash (u32 (logxor hash (ash hash 2)))
	  hash (u32 (+ hash (ash hash -15)))
	  hash (u32 (logxor hash (ash hash 10)))
	  hash (u32 (logior hash (ash (lognot hash) 31))))
    (the (unsigned-byte 32) hash)))
	      

;;FIXME Rewrite to vector usage + index loop
(defun jenkins(octets &key (initial-val 0)) 
  (let ((len (length octets))
	 (a #16r9e3779b9)
	 (b #16r9e3779b9)
	 (c initial-val))
    (declare (type (unsigned-byte 32) a b c))
    (labels ((wirble (lst l)
	       (if (>= l 12)
		   (progn 
		     (destructuring-bind (k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 . rest) lst
		       (setf a (u32 (+ a k0 (ash k1 8) (ash k2 16) (ash k3 24)))
			     b (u32 (+ b k4 (ash k5 8) (ash k6 16) (ash k7 24)))
			     c (u32 (+ c k8 (ash k9 8) (ash k10 16) (ash k11 24))))
		       (mix)
		       (wirble rest (- l 12))))
		   (values lst l)))
	     (mix ()
	       (setf a (u32 (- a b)) a (u32 (- a c)) a (u32 (logxor a (ash c -13)))
		     b (u32 (- b c)) b (u32 (- b a)) b (u32 (logxor b (ash a 8)))
		     c (u32 (- c a)) c (u32 (- c b)) c (u32 (logxor c (ash b -13)))
		     a (u32 (- a b)) a (u32 (- a c)) a (u32 (logxor a (ash c -12)))
		     b (u32 (- b c)) b (u32 (- b a)) b (u32 (logxor b (ash a 16)))
		     c (u32 (- c a)) c (u32 (- c b)) c (u32 (logxor c (ash b -5)))		   
		     a (u32 (- a b)) a (u32 (- a c)) a (u32 (logxor a (ash c -3)))
		     b (u32 (- b c)) b (u32 (- b a)) b (u32 (logxor b (ash a 10)))
		     c (u32 (- c a)) c (u32 (- c b)) c (u32 (logxor c (ash b -15)))))		       
	     (process-rest (lst l)
	       (when (and (plusp l) (not (null lst)))
		 (let ((head (car lst))) 
		   (let ((head8 (ash head 8))
			 (head16 (ash head 16))
			 (head24 (ash head 24))) 
		     (case l
		       (11 (setf c (u32 (+ c head24))))
		       (10 (setf c (u32 (+ c head16))))
		       (9  (setf c (u32 (+ c head8))))
		       (8  (setf b (u32 (+ b head24))))
		       (7  (setf b (u32 (+ b head16))))
		       (6  (setf b (u32 (+ b head8))))
		      (5  (setf b (u32 (+ b head))))
		      (4  (setf a (u32 (+ a head24))))
		      (3  (setf a (u32 (+ a head16))))
		      (2  (setf a (u32 (+ a head8))))
		      (1  (setf a (u32 (+ a head)))))))
		 (process-rest (cdr lst) (1- l)))))
      (multiple-value-bind (lst l) 
	  (wirble  (coerce octets 'list) len)
	(incf c len)
	(process-rest (reverse lst) l))
      (mix)
      c)))

