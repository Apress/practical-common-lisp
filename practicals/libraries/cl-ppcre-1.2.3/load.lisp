;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/load.lisp,v 1.12 2005/02/02 18:34:30 edi Exp $

;;; Copyright (c) 2002-2004, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(let ((cl-ppcre-base-directory
        (make-pathname :name nil :type nil :version nil
                       :defaults (parse-namestring *load-truename*)))
      must-compile)
  (with-compilation-unit ()
    (dolist (file '("packages"
                    "specials"
                    "util"
                    "errors"
                    #-:use-acl-regexp2-engine "lexer"
                    #-:use-acl-regexp2-engine "parser"
                    #-:use-acl-regexp2-engine "regex-class"
                    #-:use-acl-regexp2-engine "convert"
                    #-:use-acl-regexp2-engine "optimize"
                    #-:use-acl-regexp2-engine "closures"
                    #-:use-acl-regexp2-engine "repetition-closures"
                    #-:use-acl-regexp2-engine "scanner"
                    "api"
                    "ppcre-tests"))
      (let ((pathname (make-pathname :name file :type "lisp" :version nil
                                     :defaults cl-ppcre-base-directory)))
        ;; don't use COMPILE-FILE in Corman Lisp, it's broken - LOAD
        ;; will yield compiled functions anyway
        #-:cormanlisp
        (let ((compiled-pathname (compile-file-pathname pathname)))
          (unless (and (not must-compile)
                       (probe-file compiled-pathname)
                       (< (file-write-date pathname)
                          (file-write-date compiled-pathname)))
            (setq must-compile t)
            (compile-file pathname))
          (setq pathname compiled-pathname))
        (load pathname)))))



