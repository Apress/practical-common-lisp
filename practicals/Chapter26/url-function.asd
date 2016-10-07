(defpackage :com.gigamonkeys.url-function-system (:use :asdf :cl))
(in-package :com.gigamonkeys.url-function-system)

(require :aserve)

(defsystem url-function
  :name "url-function"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "0.1"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "define-url-function macro for AllegroServe"
  :long-description ""
  :components
  ((:file "packages")
   (:file "html-infrastructure" :depends-on ("packages")))
  :depends-on (:html :macro-utilities))

        
