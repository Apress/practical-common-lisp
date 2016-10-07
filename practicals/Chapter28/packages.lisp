(in-package :cl-user)

(defpackage :com.gigamonkeys.shoutcast
  (:use :common-lisp 
        :net.aserve 
        :com.gigamonkeys.id3v2)
  (:export :song
           :file
           :title
           :id3-size
           :find-song-source
           :current-song
           :still-current-p
           :maybe-move-to-next-song
           :*song-source-type*))

