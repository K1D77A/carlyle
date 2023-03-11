;;;; package.lisp

(defpackage #:carlyle/v2
  (:use #:cl)
  (:import-from :hu.dwim.defclass-star
                #:defclass*)
  (:export ;;conditions.lisp
   #:carlyle-condition
   
   #:api-condition
   #:http-status-code
   #:category
   #:description

   #:rate-limited

   #:missing-path-arg
   #:expected

   #:not-found

   #:bad-request

   #:not-implemented
   #:feature

   #:total-failure

   #:malformed-json

   #:malformed-params

   #:unknown-argument
   #:argument

   #:missing-crc

   #:bad-crc

   #:expired-bearer-token

   #:anything

   #:no-bearer-token

   #:forbidden

   #:bad-bearer

   ;;condition protocol
   #:compose-condition

   #:compose-condition/error

   #:compose-condition/info

   #:compose-condition/recover

   ;;api protocol
   #:%append-headers

   #:%authentication

   #:%find-bearer-token

   #:%validate-bearer-token

   #:%content-parser

   #:%content-validation

   #:%condition-handler

   #:%record-condition

   #:%parse-params

   #:%body

   #:%post-process-body

   #:%request-validation

   #:%verify-parameter

   ;;definer
   #:defapi
   #:defapi-group
   #:api

   ;;encryption.lisp
   #:crc32

   ;;carlyle.lisp
   #:carlyle-app

   #:safe-destructure-keys

   #:safe-destructure-params))
   

   
