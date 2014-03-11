;;; (c) David Mathei.  See LICENSE file for permissions
(defpackage :probcount-system
  (:use :cl :asdf))

(in-package :probcount-system)

(defsystem :probcount
  :name "probcount"
  :description "bloom filter, probabilistic counting and hashes"
  :long-description ""
  :version "0.1"
  :author "David Mathei <david.mathei@gmail.com>"
  :licence "MIT"
  :components ((:module "src"
		:components 	
		((:file "package")
		 (:file "hash" :depends-on ("package"))
		 (:file "bloom" :depends-on ("hash"))
		 (:file "count" :depends-on ("hash")))))) 


(defmethod perform ((o asdf:test-op)
                    (s (eql (asdf:find-system :probcount))))
            (asdf:load-system :probcount-test)
            (funcall (find-symbol (string :run-tests) :probcount-test )))

                    
