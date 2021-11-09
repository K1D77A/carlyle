(in-package #:carlyle)

(defun crc32 (content)
  (ironclad:octets-to-integer (ironclad:digest-sequence :crc32 content)))
