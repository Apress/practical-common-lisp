(in-package :cl-user)

(defpackage :com.gigamonkeys.html
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :with-html-output
           :with-html-to-file
           :in-html-style
           :define-html-macro
           :define-css-macro
           :css
           :html
           :emit-css
           :emit-html
           :&attributes))
