(in-package #:carlyle/v2)

#||
This file contains the code to define user facing API's.
||#

(defmacro defapi (app name superclasses (version method path)
                  (&key (requires-auth t)
                     (contains-body t))
                  &body body)
  "documentation"
  (let ((params-args (extract-args-from-url path)))
    (flet ((make-name (method)
             (intern (string-upcase (format nil "~A%~A" name method)))))
      `(progn (defclass ,(make-name "process")
                  ,(append superclasses (list 'process-object))
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
                  ,(make-name "condition-recorder")
                  (condition (method (eql ,method)) version process)
                (%record-condition condition ,method ,version process))
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
                            (handler-case 
                                (funcall ',(make-name "condition-recorder")
                                         c ,method ,version process)
                              (condition (con)
                                (setf c con)))
                            (funcall ',(make-name "condition-handler")
                                     c ,method ,version
                                     process))))))))))

