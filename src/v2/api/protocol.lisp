(in-package #:carlyle/v2)

(defclass* api-object ()
  ((params
    :accessor params
    :initform nil
    :type (or null list))
   (headers
    :accessor headers
    :initarg :headers
    :type hash-table)
   (request
    :accessor request
    :type lack.request:request)
   (response
    :accessor response
    :type lack.response:response)
   (body
    nil 
    :accessor body)
   (anything 
    :accessor anything
    :initform ()
    :documentation "key val plist for random values")
   (raw-body
    :accessor raw-body
    :type (or null (array (unsigned-byte 8))))
   (path
    :accessor path
    :type (or string pathname)))
  (:export-accessor-names-p t)
  (:export-class-name-p t))

(defgeneric %append-headers (api name method version)
  (:documentation "The fallback method for appending the correct headers to a request."))

(defmethod %append-headers (api name method version)
  (with-accessors ((response response))
      api 
    (setf (lack.response:response-headers response)
          (append (lack.response:response-headers response)
                  (list :content-type "application/json")))))

(defgeneric %authentication (api name method version)
  (:documentation
   "The fallback method for validating the authentication provided by the request.
Calls both %find-bearer-token and %validate-bearer-token"))

(defmethod %authentication (api name method version)
  "By default extract the authorization header, extract the token then call 
#'find-bearer-token with the method version api and the extracted token, 
then call #'%validate-bearer-token with method version api and the token."
  (with-accessors ((request request))
      api 
    (let* ((bearer (req-header "authorization" request))
           (token (second (str:split #\Space bearer)))
           (token? (%find-bearer-token api name method version token)))
      (%validate-bearer-token api name method version token?))))

(defgeneric %find-bearer-token (api name method version token)
  (:documentation "Specialize me to find and validate a bearer token."))

(defmethod %find-bearer-token (api name method version (token null))
  "If TOKEN is null then signal 'no-bearer-token"
  (%signal-no-bearer api name method version token))

(defgeneric %signal-no-bearer (api name method version token)
  (:method (api name method version token)
    (error 'no-bearer-token)))

(defgeneric %validate-bearer-token (api name method version obj)
  (:documentation "This generic is used to make sure that the token received is valid."))

(defmethod %validate-bearer-token (api name method version obj)
  "By default signal 'bad-bearer"
  (%signal-bad-bearer api name method version token))

(defgeneric %signal-bad-bearer (api name method version token)
  (:method (api name method version token)
    (error 'bad-bearer)))



(defgeneric %content-parser (api name method version)
  (:documentation "The default generic for parsing the body of requests."))

(defmethod %content-parser (api name method version)
  "By default attempt to get the raw-body and then parse it as a hash-table and set the 
(body api) to that hash-table. Calls %content-validation with the raw body."
  (with-accessors ((body body)
                   (raw-body raw-body)
                   (request request))
      api
    ;;need a way to constrain the body size.
    (let ((raw (%request-raw-body request)))
      (when raw
        (%content-validation api name method version raw)
        (setf raw-body raw
              body (shasht:read-json (babel:octets-to-string raw)))))))

(defgeneric %content-validation (api name method version raw)
  (:documentation "The default generic for making sure that the body received is valid.
This uses CRC.")
  (:method (api name method version raw)
    t))

(defgeneric %condition-handler (condition api name method version)
  (:documentation "The fallback generic for handling conditions."))

(defmethod %condition-handler (condition api name method version)
  "The default method for handling conditions calls api-condition."
  (with-accessors ((request request)
                   (response response))
      api
    (process-condition condition api name method version request response)))

(defgeneric %record-condition (condition api name method version)
  (:documentation "The default means of recording a condition. Doesn't do anything."))

(defmethod %record-condition (condition api name method version)
  nil)


(defgeneric %signal-missing-path-arg (api name method version &rest args)
  (:method (api name method version &rest args)
    (apply #'error 'missing-path-arg args)))

(defgeneric %parse-params (api name params-list method version)
  (:method (api name params-list method version)
    (with-accessors ((params params))
        api
      (dolist (arg params-list)
        (let ((extracted  (cdr (assoc arg params :test #'string-equal))))
          (unless extracted
            (%signal-missing-path-arg api name method version :expected arg))
          (let ((parsed (quri.decode:url-decode extracted)))
            (%verify-parameter api name method version arg parsed)
            (setf (slot-value api arg) parsed)))))))

(defgeneric %signal-unknown-argument (api name method version &rest args)
  (:method (api name method version &rest args)
    (apply #'error 'unknown-argument args)))

(defgeneric %verify-parameter (api name method version key val)
  (:method :around (api name method version key val)
    (or (call-next-method)
        (%signal-unknown-argument api name method version
                                  :argument (string key)
                                  :value val)))
  (:method (api name method version key (val null))
    nil))

(defgeneric %around-execution (function api name method version)
  (:documentation "Specialize :around methods for this so you can perform actions around
the execution of the request.")
  (:method (function api name method version)
    (funcall function)))

(defgeneric %post-process-body (api name method version result)
  (:documentation "After body is finished use this to api the final evaluation into 
JSON. WAY is the means of doing this.")
  (:method :around (api name method version result)
    (let* ((res (call-next-method)))
      (setf (lack.response:response-headers ningle:*response*)
            (append (lack.response:response-headers ningle:*response*)
                    (list :crc (crc32 (babel:string-to-octets res)))))
      res))
  (:method (api name method version result)
    result))


(defmethod %request-validation (api name method version)
  t)

(defun to-hash-table (octets)  
  (jojo:parse (babel:octets-to-string octets) :as :hash-table))

(defun check-valid-crc (crc raw-body)
  "Compute the CRC32 for the RAW-BODY (bytes) and signal 'bad-crc if it does not match 
the CRC provided."
  (or (string= (format nil "~D" (crc32 raw-body)) crc)
      (error 'bad-crc)))


(defun validate-crc (request raw-body)
  (let ((crc-header (req-header "crc" request)))
    (unless crc-header
      (error 'missing-crc))
    (check-valid-crc crc-header raw-body)))

(defun validate-bearer (request)
  (or (req-header "authorization" request)
      (error 'no-bearer-token)))

(defgeneric process-condition (condition api name method version request response))

(defmethod process-condition :around (condition api name
                                      method version request response)
  (setf (lack.response:response-status response) 500)
  (call-next-method))

(defmethod process-condition (condition api name method version  request response)
  (compose-condition :json (make-condition 'api-condition
                                           :description (type-of condition))
                     api name 
                     method version request response))

(defmethod process-condition ((condition api-condition)
                              api name method version
                              request response)
  (setf (lack.response:response-status response) (http-status-code condition))
  (compose-condition :json condition api name method version request response))

(defun verify-parameters (params)
  (mapc (lambda (alist)
          (let ((key (car alist))
                (val (cdr alist)))
            (when (keywordp key)
              (unless (verify-param key val)
                (error 'unknown-argument)))))
        params))

(defun extract-args-from-url (url)
  (let ((split (str:split #\/ url :omit-nulls t)))
    (loop :for str :in split
          :when (char= (aref str 0) #\:)
            :collect  (intern (string-upcase (subseq str 1))))))

(defun req-header (header req)
  (gethash header (lack.request:request-headers req)))

(defun %request-raw-body (request)
  (let ((len (lack.request:request-content-length request)))
    (when (and (numberp len)
               (plusp len))
      (let ((raw (lack.request:request-raw-body request)))
        (when raw 
          (let ((seq (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence seq raw)
            seq))))))
