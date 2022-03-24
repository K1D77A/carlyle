(in-package #:carlyle/v2)

#||
This file contains the code to define user facing API's.
||#

(defmacro defapi (app name superclasses (version method path)
                  (&key (requires-auth t)
                     (contains-body t))
                  &body body)
  "documentation"
  (unless (keywordp name)
    (error "Name must be a keyword"))
  (let ((params-args (extract-args-from-url path)))
    (flet ((make-name (method)
             (intern (string-upcase (format nil "~{~A~^-~}%~A" superclasses method)))))
      `(progn (defclass ,(make-name "api")
                  ,(append superclasses (list 'api-object))
                ,(mapcar (lambda (arg)
                           (list arg
                                 :initform nil
                                 :reader arg))
                  (extract-args-from-url path)))
              (defmethod
                  ,(make-name "body")
                  (api name (method (eql ,method)) version)
                ,(if body
                     `(locally ,@body)
                     `(%body api ,name ,method ,version)))
              (defmethod
                  ,(make-name "authentication")
                  (api name (method (eql ,method)) version)
                ,(if requires-auth 
                     `(%authentication api ,name ,method ,version)
                     t))
              (defmethod
                  ,(make-name "request-validation")
                  (api name (method (eql ,method))
                   version)
                ,(if contains-body 
                     `(%request-validation api ,name ,method ,version)
                     t))
              (defmethod
                  ,(make-name "content-parser")
                  (api name (method (eql ,method)) version)
                ,(if contains-body 
                     `(%content-parser api ,name ,method ,version)
                     t))
              (defmethod
                  ,(make-name "content-validation")
                  (api name (method (eql ,method)) version)
                ,(if contains-body 
                     `(%content-validation api ,name ,method ,version)
                     t))
              (defmethod
                  ,(make-name "condition-handler")
                  (condition api name (method (eql ,method)) version)
                (%condition-handler condition api ,name ,method ,version))
              (defmethod
                  ,(make-name "condition-recorder")
                  (condition api name (method (eql ,method)) version)
                (%record-condition condition api ,name
                                   ,method ,version))
              (defmethod
                  ,(make-name "parse-params")
                  (api name (method (eql ,method)) version)
                ,(if params-args 
                     `(%parse-params api ,name
                                     ',params-args ,version ,method)
                     t))
              (defmethod
                  ,(make-name "post-process-body")
                  (api name (method (eql ,method)) version result)
                ,(if contains-body
                     `(%post-process-body api ,name ,method ,version result)
                     t))
              (defmethod
                  ,(make-name "append-headers")
                  (api name (method (eql ,method)) version)
                (%append-headers api ,name ,method ,version))
              (setf (ningle:route ,app ,(format nil "/~A~A"
                                                (string-downcase version)
                                                path)
                                  :method ,method)
                    (lambda (params)
                      (let ((api (make-instance ',(make-name "api")
                                                :params params
                                                :request ningle:*request*
                                                :response ningle:*response*
                                                :path ,path)))
                        (handler-case
                            (progn 
                              (funcall ',(make-name "append-headers")
                                       api ,name ,method ,version)
                              (funcall ',(make-name "authentication")
                                       api ,name ,method ,version)
                              (funcall ',(make-name "parse-params")
                                       api ,name  ,method ,version)
                              (funcall ',(make-name "content-parser")
                                       api ,name ,method ,version )
                              (funcall ',(make-name "content-validation")
                                       api ,name ,method ,version)
                              (funcall ',(make-name "post-process-body")
                                       api ,name  ,method ,version 
                                       (funcall ',(make-name "body")
                                                api ,name ,method ,version)))
                          (condition (c)
                            (handler-case 
                                (funcall ',(make-name "condition-recorder")
                                         c api ,name ,method ,version)
                              (condition (con)
                                (setf c con)))
                            (funcall ',(make-name "condition-handler")
                                     c api ,name ,method ,version))))))))))

