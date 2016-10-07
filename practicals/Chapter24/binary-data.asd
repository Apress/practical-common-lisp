(defpackage :com.gigamonkeys.binary-data-system (:use :asdf :cl))
(in-package :com.gigamonkeys.binary-data-system)

(defsystem binary-data
  :name "binary-data"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Parser for binary data files. "
  :long-description ""
  :components
  ((:file "packages")
   (:file "binary-data" :depends-on ("packages")))
  :depends-on (:macro-utilities))

        
