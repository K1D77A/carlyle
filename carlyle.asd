;;;; carlyle.asd

(asdf:defsystem #:carlyle
  :description "A framework for constructing a REST based backend."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:str
               #:clack
               #:ningle
               #:cl-json
               #:jonathan
               #:closer-mop
               #:ironclad
               #:local-time)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "encryption")
               (:module "conditions"
                :serial t
                :components ((:file "conditions")
                             (:file "condition-protocol")
                             (:file "compose")))
               (:module "json"
                :serial t
                :components ((:file "json-protocol")))
               (:module "api"
                :serial t
                :components ((:file "api-protocol")))
               (:file "carlyle")))
