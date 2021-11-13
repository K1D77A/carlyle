;;;; package.lisp

(defpackage #:carlyle
  (:use #:cl)
  (:export ;;conditions.lisp
   #:caryle-condition
   
   #:api-condition
   #:http-status-code
   #:category
   #:description

   #:rate-limited

   #:not-found

   #:bad-request

   #:malformed-json

   #:unknown-argument

   #:missing-crc

   #:bad-crc

   #:no-bearer-token

   #:forbidden

   #:bad-bearer

   ;;condition protocol
   #:compose-condition

   #:compose-condition/error

   #:compose-condition/info

   #:compose-condition/recover

   ;;json
   #:post-process-json

   ;;bearer
   #:find-bearer-token

   #:validate-bearer-token
   ;;api-protocol
   #:request

   #:with-body

   #:without-body

   #:delete-request

   #:get-request

   #:put-request

   #:post-request

   #:patch-request

   #:defapi
   #:defapi%no-json
   #:def-auth-api%get
   #:def-no-auth-api%get
   #:def-auth-api%post
   #:def-no-auth-api%post
   #:def-auth-api%delete

   #:verify-api-request

   #:process-condition

   #:verify-param

   ;;encryption.lisp
   #:crc32

   ;;carlyle.lisp
   #:carlyle-app

   #:safe-destructure-keys))
   

   
