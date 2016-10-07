;;; This file contains the demonstration code used in Chapter 26. This
;;; file is not loaded as part of the url-function system.

(require :aserve)

(defpackage :com.gigamonkeys.web
  (:use :cl :net.aserve :com.gigamonkeys.html :com.gigamonkeys.url-function))

(in-package :com.gigamonkeys.web)

(start :port 2001)

(publish :path "/random-number" :function 'random-number)

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (format 
       (request-reply-stream request)
       "<html>~@
            <head><title>Random</title></head>~@
            <body>~@
            <p>Random number: ~d</p>~@
            </body>~@
            </html>~@
           "
       (random 1000)))))

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:html
            (:head (:title "Random"))
            (:body
              (:p "Random number: " (:print (random 1000))))))))))

(publish :path "/show-query-params" :function 'show-query-params)

(defun show-query-params (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:standard-page
           (:title "Query Parameters")
           (if (request-query request)
             (html 
               (:table :border 1
                       (loop for (k . v) in (request-query request)
                          do (html (:tr (:td k) (:td v))))))
             (html (:p "No query parameters.")))))))))

(publish :path "/simple-form" :function 'simple-form)

(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:html
            (:head (:title "Simple Form"))
            (:body
              (:form :method "POST" :action "/show-query-params"
                     (:table
                       (:tr (:td "Foo")
                            (:td (:input :name "foo" :size 20)))
                       (:tr (:td "Password")
                            (:td (:input :name "password" :type "password" :size 20))))
                     (:p (:input :name "submit" :type "submit" :value "Okay")
                         (:input ::type "reset" :value "Reset"))))))))))

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (let* ((limit-string (or (request-query-value "limit" request) ""))
               (limit (or (parse-integer limit-string :junk-allowed t) 1000)))
          (html
            (:html
              (:head (:title "Random"))
              (:body
                (:p "Random number: " (:print (random limit)))))))))))

(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html
          (:standard-page
           (:title "Cookies")
           (if (null (get-cookie-values request))
             (html (:p "No cookies."))
             (html 
               (:table
                 (loop for (key . value) in (get-cookie-values request)
                    do (html (:tr (:td key) (:td value)))))))))))))

(publish :path "/show-cookies" :function 'show-cookies)



(defun set-cookie (request entity)
  (with-http-response (request entity :content-type "text/html")
    (set-cookie-header request :name "MyCookie" :value "A cookie value")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
        (html 
          (:standard-page
           (:title "Set Cookie")
           (:p "Cookie set.")
           (:p (:a :href "/show-cookies" "Look at cookie jar."))))))))

(publish :path "/set-cookie" :function 'set-cookie)

(defmethod string->type ((type (eql 'integer)) value)
  (parse-integer (or value "") :junk-allowed t))

(define-url-function random-number (request (limit integer 1000))
  (:html
    (:head (:title "Random"))
    (:body
      (:p "Random number: " (:print (random limit))))))
