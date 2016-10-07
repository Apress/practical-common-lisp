(in-package #:com.gigamonkeys.html)

;;; CSS support

;; For stylesheets
(define-html-special-operator css-style (processor &rest body)
  (dolist (sexp body)
    (if (eql (first sexp) :import)
      (emit-css-import processor sexp)
      (process-css processor sexp))))

(defun emit-css-import (processor sexp)
  (let ((url (second sexp)))
    (freshline processor)
    (raw-string processor "@import ")
    (cond
      ((consp url)
       (raw-string processor "url(")
       (raw-string processor (second url))
       (raw-string processor ")"))
      (t (raw-string processor (format nil "\"~a\"" url))))
    (raw-string processor ";")))

(defun process-css (processor sexp)
  (destructuring-bind (selector &rest attributes) sexp
    (freshline processor)
    (emit-css-selector processor selector)
    (freshline processor)
    (raw-string processor "{")
    (indent processor)
    (freshline processor)
    (loop for (k v) on attributes by #'cddr do
         (process-css-key-or-value processor k)
         (raw-string processor ": ")
         (process-css-key-or-value processor v)
         (raw-string processor ";")
         (freshline processor))
    (unindent processor)
    (freshline processor)
    (raw-string processor "}")
    (freshline processor)))

(defun emit-css-selector (processor selector)
  (cond
    ((atom selector)
     (raw-string processor (string selector)))
    ((and (consp selector) (member (first selector) '(or and adjacent)))
     (loop with separator = (case (first selector) (or ", ") (and " ") (adjacent " + "))
        for (x . rest) on (rest selector)
        do (emit-css-selector processor x)
        when rest do (raw-string processor separator)))
    (t
     (multiple-value-bind (tag class pseudo-class id) (parse-selector selector)
       (when tag
         (embed-value processor (string tag)))
       (when class
         (embed-value processor (format nil ".~a" class)))
       (when pseudo-class
         (embed-value processor (format nil ":~a" pseudo-class)))
       (when id
         (embed-value processor (format nil "#~a" id)))))))

(defun parse-selector (selector)
  (if (member (first selector) '(:class :pseudo-class :id))
    (destructuring-bind (&key class pseudo-class id) selector
      (values nil class pseudo-class id))
    (destructuring-bind (tag &key class pseudo-class id) selector
      (values tag class pseudo-class id))))

(defun process-css-key-or-value (processor form)
  (if (keywordp form)
    (embed-value processor (string-downcase form))
    (process processor form)))

