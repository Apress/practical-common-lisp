(defpackage :com.gigamonkeys.simple-database-system (:use :asdf :cl))
(in-package :com.gigamonkeys.simple-database-system)

(defsystem simple-database
  :name "simple-database"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Simple s-expression database."
  :long-description ""
  :components
  ((:file "packages")
   (:file "simple-database" :depends-on ("packages"))))

        
