(in-package #:carlyle)

(defmethod compose-condition/recover append ((condition bad-request) request &rest args)
  (declare (ignore args))
  `(("low" . "Resend request.")))

(defmethod compose-condition/recover append ((condition bad-crc) request &rest args)
  (declare (ignore args))
  `(("high" . "Fix CRC calculator")))

(defmethod compose-condition/recover append ((condition missing-crc) request &rest args)
  (declare (ignore args))
  `(("high" . "Add a CRC Header which is computed from the body")))

(defmethod compose-condition/recover append ((condition no-bearer-token) request &rest args)
  (declare (ignore args))
  `(("high" . "Append authorization header")))

(defmethod compose-condition/recover append ((condition unknown-argument) request &rest args)
  (declare (ignore args))
  `(("high" . "Fix structure of request.")))

(defmethod compose-condition/recover append ((condition forbidden) request &rest args)
  (declare (ignore args))
  `(("low" . "Reauthorize")))

(defmethod compose-condition/recover append ((condition bad-bearer) request &rest args)
  (declare (ignore args))
  `(("high" . "Request a new token")))

(defmethod compose-condition/recover append ((condition not-found) request &rest args)
  (declare (ignore args))
  `(("low" . "Send no more requests to this URI")))
