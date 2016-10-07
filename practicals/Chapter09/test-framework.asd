(defpackage :com.gigamonkeys.test-system (:use :asdf :cl))
(in-package :com.gigamonkeys.test-system)

(defsystem test-framework
  :name "test-framework"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Simple unit test framework for Common Lisp"
  :long-description ""
  :components
  ((:file "packages")
   (:file "test" :depends-on ("packages")))
  :depends-on (:macro-utilities))
