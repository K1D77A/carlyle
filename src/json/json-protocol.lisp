(in-package #:caryle)

#||
This file contains the code for a protocol that generates the response JSON.
||#

(defgeneric obj->json (stream object ignore)
  (:documentation "Converts an OBJECT to json, ignores the slots listed in IGNORE."))

(defmethod obj->json (stream object ignore)
  (jojo:to-json object))

(defgeneric post-process-body (result way params)
  (:documentation "After body is finished use this to process the final evaluation into 
JSON. WAY is the means of doing this."))

(defmethod post-process-body :around (result way params)
  (let* ((res (call-next-method)))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*)
                  (list :crc (crc32 (babel:string-to-octets res)))))
    res))

(defmethod post-process-body (result (way (eql :jojo)) params)
  "Single object uses jojo to generate the JSON."
  (jojo:to-json result))

(defmethod post-process-body (result (way (eql :jojo-list)) params)
  "Converts a list RESULT into json using JOJO."
  (jojo:to-json result))



