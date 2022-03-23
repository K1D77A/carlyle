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

(defmacro defapi (app name (version method path)
                  (&key (requires-auth t)
                     (contains-body t))
                  &body body)
  "documentation"
  (let ((params-args (extract-args-from-url path)))
    (flet ((make-name (method)
             (intern (string-upcase (format nil "~A%~A" name method)))))
      `(progn (defclass ,(make-name "process")
                  (process-object)
                ,(mapcar (lambda (arg)
                           (list arg
                                 :initform nil
                                 :reader arg))
                  (extract-args-from-url path)))
              (defmethod
                  ,(make-name "body")
                  ((method (eql ,method)) version process)
                ,(if body
                     `(locally ,@body)
                     `(%body ,method ,version process)))
              (defmethod
                  ,(make-name "authentication")
                  ((method (eql ,method)) version process)
                ,(if requires-auth 
                     `(%authentication ,method ,version process)
                     t))
              (defmethod
                  ,(make-name "request-validation")
                  ((method (eql ,method))
                   version process)
                ,(if contains-body 
                     `(%request-validation ,method ,version process)
                     t))
              (defmethod
                  ,(make-name "content-parser")
                  ((method (eql ,method)) version process)
                ,(if contains-body 
                     `(%content-parser ,method ,version process)
                     t))
              (defmethod
                  ,(make-name "content-validation")
                  ((method (eql ,method)) version process)
                ,(if contains-body 
                     `(%content-validation ,method ,version process)
                     t))
              (defmethod
                  ,(make-name "condition-handler")
                  (condition (method (eql ,method)) version process)
                (%condition-handler condition ,method ,version process))
              (defmethod
                  ,(make-name "parse-params")
                  ((method (eql ,method)) version process)
                ,(if params-args 
                     `(%parse-params ',params-args ,version ,method process)
                     t))
              (defmethod
                  ,(make-name "post-process-body")
                  ((method (eql ,method)) version process result)
                ,(if contains-body
                     `(%post-process-body ,method ,version process result)
                     t))
              (defmethod
                  ,(make-name "append-headers")
                  ((method (eql ,method)) version process)
                (%append-headers ,method ,version process))
              (setf (ningle:route ,app ,(format nil "/~A~A"
                                                (string-downcase version)
                                                path)
                                  :method ,method)
                    (lambda (params)
                      (let ((process (make-instance ',(make-name "process")
                                                    :params params
                                                    :request ningle:*request*
                                                    :response ningle:*response*
                                                    :path ,path)))
                        (handler-case
                            (progn 
                              (funcall ',(make-name "append-headers")
                                       ,method ,version process)
                              (funcall ',(make-name "authentication")
                                       ,method ,version process)
                              (funcall ',(make-name "parse-params")
                                       ,method ,version process)
                              (funcall ',(make-name "content-parser")
                                       ,method ,version process)
                              (funcall ',(make-name "content-validation")
                                       ,method ,version process)
                              (funcall ',(make-name "post-process-body")
                                       ,method ,version process 
                                       (funcall ',(make-name "body")
                                                ,method ,version process)))
                          (condition (c)
                            (funcall ',(make-name "condition-handler")
                                     c ,method ,version
                                     process))))))))))


(defparameter *app* (make-instance 'ningle:<app>))

(defapi *app* cake-dog (:v1 :post "/:cake/:dog")
  (:requires-auth t :contains-body t)
  (format nil "~A/~A/~A" (cake process) (dog process)
          (body process)))

(defgeneric %append-headers (method version process))

(defmethod %append-headers (method version process)
  (with-accessors ((response response))
      process 
    (setf (lack.response:response-headers response)
          (append (lack.response:response-headers response)
                  (list :content-type "application/json")))))

(defgeneric %authentication (method version process))

(defmethod %authentication (method version process)
  (with-accessors ((request request))
      process 
    (let* ((bearer (req-header "authorization" request))
           (token (second (str:split #\Space bearer)))
           (token? (%find-bearer-token method version process token)))
      (%validate-bearer-token method version process token?))))


(defgeneric %find-bearer-token (method version process token)
  (:documentation "Specialize me to find and validate a bearer token."))

(defgeneric %validate-bearer-token (method version process obj))


(defgeneric %content-parser (method version process))

(defmethod %content-parser (method version process)
  (with-accessors ((body body)
                   (raw-body raw-body)
                   (request request))
      process
    (let ((raw (%request-raw-body request)))
      (when raw 
        (setf raw-body raw
              body (jojo:parse (babel:octets-to-string raw) :as :hash-table))))))

(defgeneric %content-validation (method version process))

(defmethod %content-validation (method version process)
  (with-accessors ((raw-body raw-body)
                   (request request))
      process
    (when raw-body
      (validate-crc request raw-body))))

(defgeneric %condition-handler (condition method version process))

(defmethod %condition-handler (condition method version process)
  (with-accessors ((request request)
                   (response response))
      process
    (process-condition condition request response)))

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

(defgeneric process-condition (condition request response))

(defmethod process-condition :around (condition request response)
  (setf (lack.response:response-status response) 500)
  (call-next-method))

(defmethod process-condition (condition request response)
  (compose-condition (make-condition 'api-condition :description (type-of condition))
                     request))

(defmethod process-condition ((condition api-condition) request response)
  (setf (lack.response:response-status response) (http-status-code condition))
  (compose-condition condition request))

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

