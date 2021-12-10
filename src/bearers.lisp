(in-package #:carlyle)

(defgeneric find-bearer-token (token args)
  (:documentation "Specialize me to find and validate a bearer token. Args are provided by
the key 'bearer-verifier-args in the defapi-call"))
