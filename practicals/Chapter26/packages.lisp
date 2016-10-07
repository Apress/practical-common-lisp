(in-package :cl-user)

(defpackage :com.gigamonkeys.url-function
  (:use :common-lisp 
        :net.aserve 
        :com.gigamonkeys.html
        :com.gigamonkeys.macro-utilities)
  (:export :define-url-function
           :string->type))

