(in-package #:caryle)

#||
This file contains the code for handling a variety of conditions signalled that require 
responding to the caller.

compose errors that are like so 
error: { 
code: <same as the status code>
category: <category of error, say authentication error etc, invalid input etc.>
info: { 
description: <a description of the error>
<arbitrary keys and values>
}
recover: { 
<arbitrary keys and values on actions to be taken>
}
}

||#

(defun %compose-quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators a hash"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (setf (gethash a hashtable) b)))
          alist)
    hashtable))

(defgeneric componse-condition (condition request &rest args)
  (:documentation "Composes a condition into a JSON object that is sent back to the user."))

(defmethod compose-condition :around (condition request &rest args)
  (let* ((res (cl-json:encode-json-to-string (call-next-method))))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*)
                  (list :crc (crc32 (babel:string-to-octets res)))))
    res))

(defmethod compose-condition (condition request &rest args)
  (prog1 
      (%compose-quick-hash
       `(("error" . ,(apply #'compose-condition/error condition request args))
         ("info" . ,(apply #'compose-condition/info condition request args))
         ("recover" . ,(apply #'compose-condition/recover condition request args))))
    (setf (lack.response:response-status ningle:*response*) (http-status-code condition))))

(defgeneric compose-condition/error (condition request &rest args)
  (:documentation "Compose the error aspects of the condition."))

(defmethod compose-condition/error :around (condition request &rest args)
  (handler-case
      (call-next-method)
    (condition ()
      (apply #'compose-condition/error :total-failure request args))))

(defmethod compose-condition/error (condition request &rest args)
  "Fall back method"
  (with-accessors ((http-status-code http-status-code)
                   (category category))
      condition
    (setf (lack.response:response-status ningle:*response*) (http-status-code condition))
    (%compose-quick-hash
     `(("code" . ,http-status-code)
       ("category" . ,category)))))

(defmethod compose-condition/error ((condition (eql :total-failure))
                                    request &rest args)
  (%compose-quick-hash
   `(("code" . 500)
     ("category" . "INTERNAL-SERVER-ERROR"))))

(defgeneric compose-condition/info (condition request &rest args)
  (:documentation "Compose the info aspects of the condition using the APPEND method.")
  (:method-combination append :most-specific-last))

(defmethod compose-condition/info :around (condition request &rest args)
  (declare (ignore args))
  (%compose-quick-hash (call-next-method)))

(defmethod compose-condition/info append (condition request &rest args)
  (declare (ignore args))
  `(("description" . ,(description condition))))

(defgeneric compose-condition/recover (condition request &rest args)
  (:documentation "Compose the recover aspects of the condition using the APPEND method.")
  (:method-combination append :most-specific-first))

(defmethod compose-condition/recover :around (condition request &rest args)
  (declare (ignore args))
  (%compose-quick-hash (call-next-method)))

(defmethod compose-condition/recover append (condition request &rest args)
  (declare (ignore args))
  nil)


