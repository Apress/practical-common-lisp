;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/packages.lisp,v 1.17 2004/09/30 09:58:42 edi Exp $

;;; Copyright (c) 2002-2004, Dr. Edmund Weitz. All rights reserved.

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

#-:cormanlisp
(defpackage #:cl-ppcre
  (:nicknames #:ppcre)
  #+genera (:shadowing-import-from #:common-lisp #:lambda #:simple-string #:string)
  (:use #-genera #:cl #+genera #:future-common-lisp)
  (:export #:create-scanner
           #:parse-tree-synonym
           #:define-parse-tree-synonym
           #:scan
           #:scan-to-strings
           #:do-scans
           #:do-matches
           #:do-matches-as-strings
           #:all-matches
           #:all-matches-as-strings
           #:split
           #:regex-replace
           #:regex-replace-all
           #:regex-apropos
           #:regex-apropos-list
           #:quote-meta-chars
           #:*regex-char-code-limit*
           #:*use-bmh-matchers*
           #:*allow-quoting*
           #:ppcre-error
           #:ppcre-invocation-error
           #:ppcre-syntax-error
           #:ppcre-syntax-error-string
           #:ppcre-syntax-error-pos
           #:register-groups-bind
           #:do-register-groups))

#+:cormanlisp
(defpackage "CL-PPCRE"
  (:nicknames "PPCRE")
  (:use "CL")
  (:export "CREATE-SCANNER"
           "PARSE-TREE-SYNONYM"
           "DEFINE-PARSE-TREE-SYNONYM"
           "SCAN"
           "SCAN-TO-STRINGS"
           "DO-SCANS"
           "DO-MATCHES"
           "DO-MATCHES-AS-STRINGS"
           "ALL-MATCHES"
           "ALL-MATCHES-AS-STRINGS"
           "SPLIT"
           "REGEX-REPLACE"
           "REGEX-REPLACE-ALL"
           "REGEX-APROPOS"
           "REGEX-APROPOS-LIST"
           "QUOTE-META-CHARS"
           "*REGEX-CHAR-CODE-LIMIT*"
           "*USE-BMH-MATCHERS*"
           "*ALLOW-QUOTING*"
           "PPCRE-ERROR"
           "PPCRE-INVOCATION-ERROR"
           "PPCRE-SYNTAX-ERROR"
           "PPCRE-SYNTAX-ERROR-STRING"
           "PPCRE-SYNTAX-ERROR-POS"
           "REGISTER-GROUPS-BIND"
           "DO-REGISTER-GROUPS"))

#-:cormanlisp
(defpackage #:cl-ppcre-test
  #+genera (:shadowing-import-from #:common-lisp #:lambda)
  (:use #-genera #:cl #+genera #:future-common-lisp #:cl-ppcre)
  (:export #:test))

#+:cormanlisp
(defpackage "CL-PPCRE-TEST"
  (:use "CL" "CL-PPCRE")
  (:export "TEST"))
