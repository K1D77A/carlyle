(in-package #:carlyle)

(define-condition carlyle-condition (simple-error)
  ()
  (:documentation "Top level condition for all bsfe related conditions."))

(define-condition api-condition (carlyle-condition)
  ((http-status-code 
    :reader http-status-code
    :initarg :http-status-code
    :initform 500)
   (category
    :reader category
    :initarg :category
    :initform "INTERNAL-SERVER-ERROR")
   (description
    :reader description
    :initarg :description
    :initform "Internal server failure.")))

(define-condition rate-limited (api-condition)
  ((http-status-code
    :initform 429)
   (category
       :initform "LIMITED-EXCEEDED")
   (description
    :initform "You have been rate limited.")))

(define-condition not-found (api-condition)
  ((http-status-code
    :initform 404)
   (category
    :initform "NOT-FOUND")
   (description
    :initform "Entity not found")))

(define-condition bad-request (api-condition)
  ((http-status-code
    :initform 400)
   (category
    :initform "BAD-REQUEST")))

(define-condition malformed-json (bad-request)
  ((description
    :initform "The JSON sent does not match the JSON required..")))

(define-condition malformed-params (bad-request)
  ((description
    :initform "The PARAMS sent do not match the applicable params..")))

(define-condition unknown-argument (bad-request)
  ((description
    :initform "Unable to determine the types of arguments provided.")))

(define-condition missing-crc (bad-request)
  ((description
    :initform "CRC header is missing. Make sure its uppercase.")))

(define-condition no-bearer-token (bad-request)
  ((description
    :initform "Bearer token is missing for a request that requires authorization.")))

(define-condition bad-crc (bad-request)
  ((description
    :initform "The crc32 computed by the server does not match the CRC sent by you.")))

(define-condition expired-bearer-token (bad-request)
  ((description
    :initform "Bearer token has expired")))

(define-condition forbidden (api-condition)
  ((http-status-code
    :initform 403)
   (description
    :initform "Unauthorized to perform action")
   (category
       :initform "FORBIDDEN")))

(define-condition bad-bearer (forbidden)
  ((description
    :initform "Invalid token")))


