(in-package #:carlyle)

(defgeneric find-bearer-token (token)
  (:documentation "Specialize me to find and validate a bearer token."))
