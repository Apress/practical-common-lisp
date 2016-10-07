(defpackage :com.gigamonkeys.mp3-database-system (:use :asdf :cl))
(in-package :com.gigamonkeys.mp3-database-system)

(defsystem mp3-database
  :name "mp3-database"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "In-memory MP3 Database."
  :long-description ""
  :components
  ((:file "packages")
   (:file "database" :depends-on ("packages"))
   (:file "mp3-database" :depends-on ("packages" "database")))
  :depends-on (:pathnames :macro-utilities :id3v2))

        
