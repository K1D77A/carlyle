(in-package #:carlyle)

(defclass request ()
  ())

(defclass with-body (request)
  ())

(defclass without-body (request)
  ())

(defclass delete-request (without-body)
  ())

(defclass get-request (without-body)
  ())

(defclass put-request (with-body)
  ())

(defclass post-request (with-body)
  ())

(defclass patch-request (with-body)
  ())

(defun %method->object (method)
  (case method
    (:GET 'get-request)
    (:DELETE 'delete-request)
    (:PUT 'put-request)
    (:POST 'post-request)
    (:PATCH 'patch-request)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro args-to-let (args params way &body body)
    `(let ,(mapcar (lambda (arg)
                     `(,arg
                       (quri.decode:url-decode
                        (cdr (assoc ,(intern (string-upcase arg) :keyword) ,params)))))
            args)
       (declare (ignorable ,@args))
       (locally (post-process-body (locally ,@body) ,way ,params))))

  (defun extract-args-from-url (url)
    (let ((split (str:split #\/ url :omit-nulls t)))
      (loop :for str :in split
            :when (char= (aref str 0) #\:)
              :collect (intern (string-upcase (subseq str 1))))))

  (defmacro defapi (app (method url requires-auth-p)
                    (params json post-process-way) &body body)
    (let ((args (extract-args-from-url url)))
      (alexandria:with-gensyms (obj)
        `(setf (ningle:route ,app ,url :method ,method)
               (lambda (,params)
                 (handler-case
                     (let ((,obj (make-instance (%method->object ,method))))
                       (set-response-headers ningle:*response*)
                       (verify-parameters ,params)
                       ,(if json 
                            `(let ((,json
                                     (verify-api-request ,obj ningle:*request*
                                                         ,requires-auth-p)))
                               (declare (ignorable ,json))
                               ,(if args
                                    `(args-to-let ,args ,params ,post-process-way ,@body)
                                    `(post-process-body (locally ,@body) ,post-process-way
                                                        ,params)))
                            `(progn
                               (verify-api-request ,obj ningle:*request* ,requires-auth-p)
                               ,(if args
                                    `(args-to-let ,args ,params ,post-process-way ,@body)
                                    `(post-process-body (locally ,@body)
                                                        ,post-process-way ,params)))))
                   (condition (c)
                     (process-condition c ningle:*request* ningle:*response*))))))))
  )

(defmacro defapi%no-json (app (method url requires-auth-p)
                          (params post-process-way) &body body)
  `(defapi ,app (,method ,url ,requires-auth-p) (,params nil ,post-process-way)
     ,@body))

(defmacro def-auth-api%get (app (url params post-process-way) &body body)
  `(defapi%no-json ,app (:GET ,url t) (,params ,post-process-way)
     ,@body))

(defmacro def-no-auth-api%get (app (url params post-process-way) &body body)
  `(defapi%no-json ,app (:GET ,url nil) (,params ,post-process-way)
     ,@body))

(defmacro def-auth-api%post (app (url params json post-process-way) &body body)
  `(defapi ,app (:POST ,url t) (,params ,json ,post-process-way)
     ,@body))

(defmacro def-no-auth-api%post (app (url params json post-process-way) &body body)
  `(defapi ,app (:POST ,url nil) (,params ,json ,post-process-way)
     ,@body))

(defmacro def-auth-api%delete (app (url params post-process-way) &body body)
  `(defapi%no-json ,app (:DELETE ,url t) (,params ,post-process-way)
     ,@body))

(defgeneric verify-api-request (request-obj ningle-request requires-auth))

(defmethod verify-api-request ((obj with-body) req (req-auth t))
  (validate-bearer req)
  (check-authorization req)
  (let ((raw-body (%request-raw-body req)))
    (validate-crc req raw-body)
    raw-body))

(defmethod verify-api-request ((obj with-body) req (req-auth null))
  (let ((raw-body (%request-raw-body req)))
    (validate-crc req raw-body)
    raw-body))

(defmethod verify-api-request ((obj without-body) req (req-auth t))
  (validate-bearer req)
  (check-authorization req))

(defmethod verify-api-request ((obj without-body) req (req-auth null))
  t)

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

(defun check-authorization (request)
  "For the default authorized-requests, we extract the bearer token from the headers, 
the existence of this header was validated before hand. If the token is not found then 
'bad-bearer is signalled. If the token has expired then 'bad-bearer is signalled. "
  (let* ((bearer (req-header "authorization" request))
         (token (second (str:split #\Space bearer)))
         (found? (find-bearer-token token)))
    (validate-bearer-token found?)))

(defun set-response-headers (response)
  (setf (lack.response:response-headers response)
        (append (lack.response:response-headers response)
                (list :content-type "application/json"))))

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
            :collect (intern (string-upcase (subseq str 1))))))

(defun req-header (header req)
  (gethash header (lack.request:request-headers req)))

(defun %request-raw-body (request)
  (let* ((len (lack.request:request-content-length request))
         (raw (lack.request:request-raw-body request))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence seq raw)
    seq))

