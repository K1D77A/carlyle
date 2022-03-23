(in-package #:carlyle/v2)

(defclass process-object ()
  ((params
    :accessor params
    :initarg :params
    :initform nil
    :type (or null list))
   (request
    :accessor request
    :initarg :request
    :type lack.request:request)
   (response
    :accessor response
    :initarg :response
    :type lack.response:response)
   (body
    :accessor body
    :initarg :body
    :initform nil)
   (raw-body
    :accessor raw-body
    :initarg :raw-body
    :initform nil
    :type (or null (array (unsigned-byte 8))))
   (path
    :accessor path
    :initarg :path
    :type (or string pathname))))

(defgeneric %append-headers (method version process)
  (:documentation "The fallback method for appending the correct headers to a request."))

(defmethod %append-headers (method version process)
  (with-accessors ((response response))
      process 
    (setf (lack.response:response-headers response)
          (append (lack.response:response-headers response)
                  (list :content-type "application/json")))))


(defgeneric %authentication (method version process)
  (:documentation
   "The fallback method for validating the authentication provided by the request.
Calls both %find-bearer-token and %validate-bearer-token"))

(defmethod %authentication (method version process)
  "By default extract the authorization header, extract the token then call 
#'find-bearer-token with the method version process and the extracted token, 
then call #'%validate-bearer-token with method version process and the token."
  (with-accessors ((request request))
      process 
    (let* ((bearer (req-header "authorization" request))
           (token (second (str:split #\Space bearer)))
           (token? (%find-bearer-token method version process token)))
      (%validate-bearer-token method version process token?))))

(defgeneric %find-bearer-token (method version process token)
  (:documentation "Specialize me to find and validate a bearer token."))

(defmethod %find-bearer-token (method version process (token null))
  "If TOKEN is null then signal 'no-bearer-token"
  (error 'no-bearer-token))

(defgeneric %validate-bearer-token (method version process obj)
  (:documentation "This generic is used to make sure that the token received is valid."))

(defmethod %validate-bearer-token (method version process obj)
  "By default signal 'bad-bearer"
  (error 'bad-bearer))



(defgeneric %content-parser (method version process)
  (:documentation "The default generic for parsing the body of requests."))

(defmethod %content-parser (method version process)
  "By default attempt to get the raw-body and then parse it as a hash-table and set the 
(body process) to that hash-table. Calls %content-validation with the raw body."
  (with-accessors ((body body)
                   (raw-body raw-body)
                   (request request))
      process
    (let ((raw (%request-raw-body request)))
      (when raw
        (%content-validation method version process)
        (setf raw-body raw
              body (jojo:parse (babel:octets-to-string raw) :as :hash-table))))))

(defgeneric %content-validation (method version process)
  (:documentation "The default generic for making sure that the body received is valid.
This uses CRC."))

(defmethod %content-validation (method version process)
  "The default method CRC's the raw body to make sure that it is valid."
  (with-accessors ((raw-body raw-body)
                   (request request))
      process
    (when raw-body
      (validate-crc request raw-body))))



(defgeneric %condition-handler (condition method version process)
  (:documentation "The fallback generic for handling conditions."))

(defmethod %condition-handler (condition method version process)
  "The default method for handling conditions calls process-condition."
  (with-accessors ((request request)
                   (response response))
      process
    (process-condition condition method version process request response)))

(defgeneric %record-condition (condition method version process)
  (:documentation "The default means of recording a condition. Doesn't do anything."))

(defmethod %record-condition (condition method version process)
  nil)


(defgeneric %parse-params (params-list method version process))

(defmethod %parse-params (params-list method version process)
  (with-accessors ((params params))
      process
    (dolist (arg params-list)
      (setf (slot-value process arg)
            (quri.decode:url-decode (cdr (assoc arg params :test #'string-equal)))))))

(defgeneric %body (method version process))

(defmethod %body :around (method version process)
  (jojo:to-json (call-next-method)))

(defmethod %body (method version process)
  "")

(defgeneric %post-process-body (method version process result)
  (:documentation "After body is finished use this to process the final evaluation into 
JSON. WAY is the means of doing this."))

(defmethod %post-process-body :around (method version process result)
  (let* ((res (call-next-method)))
    (setf (lack.response:response-headers ningle:*response*)
          (append (lack.response:response-headers ningle:*response*)
                  (list :crc (crc32 (babel:string-to-octets res)))))
    res))

(defmethod %request-validation (method version process)
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

(defgeneric process-condition (condition method version process request response))

(defmethod process-condition :around (condition method version process  request response)
  (setf (lack.response:response-status response) 500)
  (call-next-method))

(defmethod process-condition (condition method version process  request response)
  (compose-condition (make-condition 'api-condition :description (type-of condition))
                     method version process request response))

(defmethod process-condition ((condition api-condition) method version process
                              request response)
  (setf (lack.response:response-status response) (http-status-code condition))
  (compose-condition condition method version process request response))



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

