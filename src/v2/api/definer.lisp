(in-package #:carlyle/v2)

#||
This file contains the code to define user facing API's.
||#

(defmacro defapi-group (&rest superclasses)
  "Generate the generics with fallback methods for methods defined for a defapi call."
  (flet ((make-name (method)
           (intern (string-upcase (format nil "~{~A~^-~}%~A" superclasses method)))))
    `(progn
       (defgeneric ,(make-name "body")
           (api name method version)
         (:method (api name method version)
           (%body api name method version)))
       (defgeneric ,(make-name "authentication")
           (api name method version)
         (:method (api name method version)
           (%authentication api name method version)))
       (defgeneric
           ,(make-name "request-validation")
           (api name method version)
         (:method (api name method version)
           (%request-validation api name method version)))
       (defgeneric
           ,(make-name "content-parser")
           (api name method version)
         (:method (api name method version)
           (%content-parser api name method version)))
       (defgeneric
           ,(make-name "content-validation")
           (api name method version raw)
         (:method (api name method version raw)
           (%content-validation api name method version raw)))
       (defgeneric
           ,(make-name "condition-handler")
           (condition api name method version)
         (:method (condition api name method version)
           (%condition-handler condition api name method version)))
       (defgeneric
           ,(make-name "condition-recorder")
           (condition api name method version)
         (:method (condition api name method version)
           (%record-condition condition api name method version)))
       (defgeneric
           ,(make-name "parse-params")
           (api name method version)
         (:method (api name method version)
           (%parse-params api name nil method version)))
       (defgeneric
           ,(make-name "post-process-body")
           (api name method version result)
         (:method (api name method version result)
           (%post-process-body api name method version result)))
       (defgeneric
           ,(make-name "append-headers")
           (api name method version)
         (:method (api name method version)
           (%append-headers api name method version))))))

(defmacro defapi (app name superclasses (version method path)
                  (&key (requires-auth t)
                     (contains-body t)
                     (api-var 'api))
                  &body body)
  "documentation"
  (unless (keywordp name)
    (error "Name must be a keyword"))
  (let ((params-args (extract-args-from-url path)))
    (flet ((make-name (method)
             (intern (string-upcase (format nil "~{~A~^-~}%~A" superclasses method)))))
      (let ((class-name
              (intern (string-upcase (format nil "~A%~A" name
                                             (make-name "api"))))))
        (alexandria:with-gensyms (fun)
          `(progn (defclass ,class-name
                      ,(append superclasses (list 'api-object))
                    ,(mapcar (lambda (arg)
                               (list arg
                                     :initform nil
                                     :reader arg))
                      (extract-args-from-url path)))
                  (defmethod
                      ,(make-name "around-execution")
                      (function (,api-var ,class-name)
                       (name (eql ,name)) (method (eql ,method))
                       version)
                    (%around-execution function ,api-var ,name ,method ,version))
                  (defmethod
                      ,(make-name "authentication")
                      ((,api-var ,class-name) (name (eql ,name)) (method (eql ,method))
                       version)
                    ,(if requires-auth 
                         `(%authentication ,api-var ,name ,method ,version)
                         t))
                  (defmethod
                      ,(make-name "request-validation")
                      ((,api-var ,class-name) (name (eql ,name)) (method (eql ,method))
                       version)
                    ,(if contains-body 
                         `(%request-validation ,api-var ,name ,method ,version)
                         t))
                  (defmethod
                      ,(make-name "content-parser")
                      ((,api-var ,class-name) (name (eql ,name)) (method (eql ,method))
                       version)
                    ,(if contains-body 
                         `(%content-parser ,api-var ,name ,method ,version)
                         t))            
                  (defmethod
                      ,(make-name "condition-handler")
                      (condition (,api-var ,class-name) (name (eql ,name))
                       (method (eql ,method)) version)
                    (%condition-handler condition ,api-var ,name ,method ,version))
                  (defmethod
                      ,(make-name "condition-recorder")
                      (condition (,api-var ,class-name) (name (eql ,name))
                       (method (eql ,method)) version)
                    (%record-condition condition ,api-var ,name
                                       ,method ,version))
                  (defmethod
                      ,(make-name "parse-params")
                      ((,api-var ,class-name) (name (eql ,name))
                       (method (eql ,method)) version)
                    ,(if params-args 
                         `(%parse-params ,api-var ,name
                                         ',params-args ,method ,method)
                         t))
                  (defmethod
                      ,(make-name "post-process-body")
                      ((,api-var ,class-name) (name (eql ,name)) (method (eql ,method))
                       version result)
                    (%post-process-body ,api-var ,name ,method ,version result))
                  (defmethod
                      ,(make-name "append-headers")
                      ((,api-var ,class-name) (name (eql ,name)) (method (eql ,method))
                       version)
                    (%append-headers ,api-var ,name ,method ,version))
                  (setf (ningle:route ,app ,(format nil "/~A~A"
                                                    (string-downcase version)
                                                    path)
                                      :method ,method)
                        (lambda (params)
                          (let ((,api-var (make-instance
                                           ',class-name
                                           :params params
                                           :request ningle:*request*
                                           :response ningle:*response*
                                           :path ,path
                                           :headers
                                           (lack.request:request-headers
                                            ningle:*request*))))
                            (let ((,fun
                                    (lambda ()
                                      (handler-case
                                          (progn 
                                            (funcall ',(make-name "append-headers")
                                                     ,api-var ,name ,method ,version)
                                            (funcall ',(make-name "authentication")
                                                     ,api-var ,name ,method ,version)
                                            (funcall ',(make-name "parse-params")
                                                     ,api-var ,name  ,method ,version)
                                            (funcall ',(make-name "content-parser")
                                                     ,api-var ,name ,method ,version )
                                            (funcall ',(make-name "post-process-body")
                                                     ,api-var ,name  ,method ,version 
                                                     (locally ,@body)))
                                        (serious-condition (c)
                                          (handler-case 
                                              (funcall ',(make-name "condition-recorder")
                                                       c ,api-var ,name ,method ,version)
                                            (condition (con)
                                              (setf c con)))
                                          (funcall ',(make-name "condition-handler")
                                                   c ,api-var ,name ,method ,version))))))
                              (funcall ',(make-name "around-execution") ,fun
                                       ,api-var ,name ,method ,version)))))))))))

