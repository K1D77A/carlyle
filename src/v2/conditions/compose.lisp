(in-package #:carlyle/v2)

(defmethod compose-condition/recover append
    ((condition bad-request) api name method version request response)
  `(("low" . "Resend request.")))

(defmethod compose-condition/recover append
    ((condition bad-crc) api name method version request response)
  `(("high" . "Fix CRC calculator")))

(defmethod compose-condition/recover append
    ((condition missing-crc)  api name method version request response)
  `(("high" . "Add a CRC Header which is computed from the body")))

(defmethod compose-condition/recover append
    ((condition no-bearer-token) api name method version request response)
  `(("high" . "Append authorization header")))

(defmethod compose-condition/recover append
    ((condition unknown-argument) api name method version request response)
  `(("high" . "Fix structure of request.")))

(defmethod compose-condition/recover append
    ((condition forbidden) api name method version request response)
  `(("low" . "Reauthorize")))

(defmethod compose-condition/recover append
    ((condition no-bearer-token) api name method version request response)
  `(("high" . "Request a new token")))

(defmethod compose-condition/recover append
    ((condition bad-bearer) api name method version request response)
  `(("high" . "Request a new token")))

(defmethod compose-condition/recover append
    ((condition not-found) api name method version request response)
  `(("low" . "Send no more requests to this URI")))
