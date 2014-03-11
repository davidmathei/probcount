;;; (c) David Mathei.  See LICENSE file for permissions
(defpackage :probcount-test-system
  (:use :cl :asdf))

(in-package :probcount-test-system)

(defsystem :probcount-test
  :name "probcount-test"
  :description "test package for probcount"
  :version "0.1"
  :author "David Mathei <david.mathei@gmail.com>"
  :licence "MIT"
  :depends-on (:babel :split-sequence :probcount)
  :components ((:module "test"
		:components ((:file "package")
			     (:file "tests"))))) 


                    
