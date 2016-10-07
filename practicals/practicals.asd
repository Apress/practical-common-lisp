(defpackage :com.gigamonkeys.practicals-system (:use :asdf :cl))
(in-package :com.gigamonkeys.practicals-system)

(require :aserve)

(defsystem practicals
    :name "practicals"
    :author "Peter Seibel <peter@gigamonkeys.com>"
    :version "1.0"
    :maintainer "Peter Seibel <peter@gigamonkeys.com>"
    :licence "BSD"
    :description "All code from Practical Common Lisp."
    :depends-on 
    (:binary-data
     :html
     :id3v2
     :macro-utilities
     :mp3-browser
     :mp3-database
     :pathnames
     :shoutcast
     :simple-database
     :spam
     :test-framework
     :url-function))

        
