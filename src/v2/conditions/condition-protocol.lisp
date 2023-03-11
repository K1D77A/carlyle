(in-package #:carlyle/v2)

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
  (let ((hashtable (apply #'make-hash-table (nconc rest (list :test #'equal)))))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (setf (gethash a hashtable) b)))
          alist)
    hashtable))

(defgeneric compose-condition (way condition api name method version request response)
  (:documentation "Composes a condition into a JSON object that is sent back to the user."))

(defgeneric compose-condition/error (condition api name method version  request response)
  (:documentation "Compose the error aspects of the condition.")
  (:method-combination append :most-specific-last))

(defmethod compose-condition/error :around 
    (condition api name method version request response)
  (with-accessors ((http-status-code http-status-code))
      condition
    (setf (lack.response:response-status ningle:*response*)
          (http-status-code condition))
    (%compose-quick-hash
     (handler-case 
         (call-next-method)
       (condition ()
         (compose-condition/error (make-instance 'total-failure)
                                  api name method version 
                                  request response))))))

(defmethod compose-condition/error append
    (condition api name method version request response)
  (with-accessors ((http-status-code http-status-code)
                   (category category))
      condition
    `(("code" . ,http-status-code)
      ("category" . ,category))))


(defgeneric compose-condition/info 
    (condition api name method version request response)
  (:documentation "Compose the info aspects of the condition using the APPEND method.")
  (:method-combination append :most-specific-first))

(defmethod compose-condition/info :around
    (condition api name method version request response)
  (%compose-quick-hash
   (handler-case 
       (call-next-method)
     (condition ()
       (compose-condition/info (make-instance 'total-failure)
                               api name method version 
                               request response)))))

(defmethod compose-condition/info append 
    (condition api name method version request response)
  `(("description" . ,(description condition))))

(defmethod compose-condition/info append 
    ((condition missing-path-arg) api name method version request response)
  `(("missing-arg" . ,(expected condition))))

(defmethod compose-condition/info append 
    ((condition unknown-argument) api name method version request response)
  `(("argument" . ,(argument condition))))

(defmethod compose-condition/info append 
    ((condition not-implemented) api name method version request response)
  `(("feature" . ,(feature condition))))


(defgeneric compose-condition/recover
    (condition api name method version request response)
  (:documentation "Compose the recover aspects of the condition using the APPEND method.")
  (:method-combination append :most-specific-first))

(defmethod compose-condition/recover :around
    (condition api name method version request response)
  (%compose-quick-hash
   (handler-case
       (call-next-method)
     (condition ()
       (compose-condition/recover (make-instance 'total-failure)
                                  api name method version 
                                  request response)))))

(defmethod compose-condition/recover append
    ((condition api-condition) api name method version request response)
  nil)

(defmethod compose-condition/recover append
    ((c total-failure) api name method version request response)
  `(("high" . "Report error to an administrator")))



(defmethod compose-condition :around ((way (eql :json)) condition api name
                                      method version request response)
  "By default wrap around the call and afterwards convert it to json using jojo:to-json 
and then crc it and append that as a header."
   (let* ((res (jojo:to-json  (call-next-method))))
     (setf (lack.response:response-headers ningle:*response*)
           (append (lack.response:response-headers ningle:*response*)
                   (list :crc (crc32 (babel:string-to-octets res)))))
     res))

(defmethod compose-condition ((way (eql :json))
                              condition api name method version request response)
  (%compose-quick-hash
   `(("error" . ,(compose-condition/error
                  condition api name method version request response))
     ("info" . ,(compose-condition/info
                 condition api name method version request response))
     ("recover" . ,(compose-condition/recover
                    condition api name method version request response)))
   :test #'equal :size 3))

(defmethod compose-condition ((way (eql :list))
                              condition api name method version request response)
  `(("error" . ,(alexandria:hash-table-alist
                 (compose-condition/error
                  condition api name  method version request response)))
    ("info" . ,(alexandria:hash-table-alist 
                (compose-condition/info
                 condition api name method version request response)))
    ("recover" . ,(alexandria:hash-table-alist
                   (compose-condition/recover
                    condition api name method version request response)))))
