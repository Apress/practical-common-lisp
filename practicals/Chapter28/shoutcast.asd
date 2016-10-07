(defpackage :com.gigamonkeys.shoutcast-system (:use :asdf :cl))
(in-package :com.gigamonkeys.shoutcast-system)

(require :aserve)

(defsystem shoutcast
  :name "shoutcast"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "0.1"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Shoutcast server."
  :long-description "Shoutcast server that runs in AllegroServe"
  :components
  ((:file "packages")
   (:file "song-source" :depends-on ("packages"))
   (:file "shoutcast" :depends-on ("packages")))
  :depends-on (:html :pathnames :macro-utilities :id3v2 :mp3-database :url-function))

        
