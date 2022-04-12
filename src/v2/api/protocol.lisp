(in-package #:carlyle/v2)

(defclass* api-object ()
  ((params
    :accessor params
    :initform nil
    :type (or null list))
   (request
    :accessor request
    :type lack.request:request)
   (response
    :accessor response
    :type lack.response:response)
   (body
    nil 
    :accessor body)
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
  (error 'no-bearer-token))

(defgeneric %validate-bearer-token (api name method version obj)
  (:documentation "This generic is used to make sure that the token received is valid."))

(defmethod %validate-bearer-token (api name method version obj)
  "By default signal 'bad-bearer"
  (error 'bad-bearer))



(defgeneric %content-parser (api name method version)
  (:documentation "The default generic for parsing the body of requests."))

(defmethod %content-parser (api name method version)
  "By default attempt to get the raw-body and then parse it as a hash-table and set the 
(body api) to that hash-table. Calls %content-validation with the raw body."
  (with-accessors ((body body)
                   (raw-body raw-body)
                   (request request))
      api
    (let ((raw (%request-raw-body request)))
      (when raw
        (%content-validation api name method version)
        (setf raw-body raw
              body (jojo:parse (babel:octets-to-string raw) :as :hash-table))))))

(defgeneric %content-validation (api name method version)
  (:documentation "The default generic for making sure that the body received is valid.
This uses CRC."))

(defmethod %content-validation (api name method version)
  "The default method CRC's the raw body to make sure that it is valid."
  (with-accessors ((raw-body raw-body)
                   (request request))
      api
    (when (and (slot-boundp api 'raw-body)
               raw-body)
      (validate-crc request raw-body))))

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


(defgeneric %parse-params (api name params-list method version))

(defmethod %parse-params (api name params-list method version)
  (with-accessors ((params params))
      api
    (print params)
    (dolist (arg params-list)
      (print arg)
      (let ((extracted  (cdr (assoc arg params :test #'string-equal))))
        (unless extracted
          (error 'missing-path-arg :expected arg))
        (let ((parsed (quri.decode:url-decode extracted)))
          (%verify-parameter api name method version arg parsed)
          (setf (slot-value api arg) parsed))))))

(defgeneric %verify-parameter (api name method version key val))

(defmethod %verify-parameter :around (api name method version key val)
  (or (call-next-method)
      (error 'unknown-argument :argument (string key))))

(defmethod %verify-parameter (api name method version key (val null))
  nil)

(defmethod %verify-parameter (api name method version key val)
  nil)



(defgeneric %body (api name method version))

(defmethod %body :around (api name method version)
  (jojo:to-json (call-next-method)))

(defmethod %body (api name method version)
  "")

(defgeneric %post-process-body (api name method version result)
  (:documentation "After body is finished use this to api the final evaluation into 
JSON. WAY is the means of doing this."))

(defmethod %post-process-body :around (api name method version result)
  (let* ((res (call-next-method)))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*)
                  (list :crc (crc32 (babel:string-to-octets res)))))
    res))

(defmethod %post-process-body (api name method version result)
  (jojo:to-json result))

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
  (compose-condition :json (make-condition 'api-condition :description (type-of condition))
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
    (when len
      (let ((raw (lack.request:request-raw-body request)))
        (when raw 
          (let ((seq (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence seq raw)
            seq))))))

