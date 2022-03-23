(in-package #:carlyle/v2)

(defmethod compose-condition/recover append
    ((condition bad-request)  method version process request response)
  `(("low" . "Resend request.")))

(defmethod compose-condition/recover append
    ((condition bad-crc)  method version process request response)
  `(("high" . "Fix CRC calculator")))

(defmethod compose-condition/recover append
    ((condition missing-crc)  method version process request response)
  `(("high" . "Add a CRC Header which is computed from the body")))

(defmethod compose-condition/recover append
    ((condition no-bearer-token) method version process request response)
  `(("high" . "Append authorization header")))

(defmethod compose-condition/recover append
    ((condition unknown-argument) method version process request response)
  `(("high" . "Fix structure of request.")))

(defmethod compose-condition/recover append
    ((condition forbidden) method version process request response)
  `(("low" . "Reauthorize")))

(defmethod compose-condition/recover append
    ((condition no-bearer-token) method version process request response)
  `(("high" . "Request a new token")))

(defmethod compose-condition/recover append
    ((condition bad-bearer) method version process request response)
  `(("high" . "Request a new token")))

(defmethod compose-condition/recover append
    ((condition not-found) method version process request response)
  `(("low" . "Send no more requests to this URI")))
