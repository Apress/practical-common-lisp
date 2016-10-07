(defpackage :com.gigamonkeys.macro-utilities-system (:use :asdf :cl))
(in-package :com.gigamonkeys.macro-utilities-system)

(defsystem macro-utilities
  :name "macro-utilities"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Utilities for writing macros"
  :long-description ""
  :components
  ((:file "packages")
   (:file "macro-utilities" :depends-on ("packages")))
  :depends-on ())

        
