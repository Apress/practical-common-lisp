(defpackage :com.gigamonkeys.spam-system (:use :asdf :cl))
(in-package :com.gigamonkeys.spam-system)

(defsystem spam
  :name "spam"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Spam filter"
  :long-description ""
  :components
  ((:file "packages")
   (:file "spam" :depends-on ("packages")))
  :depends-on (:cl-ppcre :pathnames))

        
