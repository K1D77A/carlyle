;;;; carlyle.lisp

(in-package #:carlyle)

(defclass carlyle-app (ningle:app)
  ())

(defmethod ningle:not-found ((app carlyle-app))
  (call-next-method)
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list :content-type "application/json")))
  (compose-condition (make-condition 'not-found) nil))

(defmacro safe-destructure-keys (keys list &body body)
  "Creates a let binding for each of the keys listed in KEYS in LIST using getf, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (list)
    `(let ,(mapcar (lambda (key)
                     `(,key (or (getf ,list ,(intern (string-upcase key) :keyword))
                                (error 'malformed-json))))
            keys)
       (locally ,@body))))




