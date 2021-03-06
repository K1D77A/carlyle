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

(defmacro safe-destructure-keys (keys hash &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (hash)
    `(let ,(mapcar (lambda (key)
                     `(,key (or (gethash ,(string key) ,hash)
                                (error 'malformed-json))))
            keys)
       (locally ,@body))))

(defmacro safe-destructure-params (keys params &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (params)
    `(let ,(mapcar (lambda (key)
                     `(,key (or (cdr (assoc ,(string key) ,params :test #'string=))
                                (error 'malformed-params))))
            keys)
       (locally ,@body))))

(defun http-body:parse (content-type content-length raw-body)
  (declare (ignore content-type content-length raw-body))
  nil)
