(defpackage :com.gigamonkeys.mp3-browser-system (:use :asdf :cl))
(in-package :com.gigamonkeys.mp3-browser-system)

(require :aserve)

(defclass css-file (static-file) ())
(defmethod source-file-type ((c css-file) (s module)) "css")

(defsystem mp3-browser
  :name "mp3-browser"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "AllegroServe-based user interface for Shoutcast server."
  :long-description ""
  :components
  ((:file "packages")
   (:file "playlist" :depends-on ("packages"))
   (:file "mp3-browser" :depends-on ("packages" "playlist"))
   (:css-file "mp3-browser"))
  :depends-on (:id3v2 :mp3-database :shoutcast :url-function :html))

        
