;;;; carlyle.asd

(asdf:defsystem #:carlyle
  :description "A framework for constructing a REST based backend."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.3"
  :depends-on (#:str
               #:clack
               #:alexandria
               #:ningle
               #:cl-json
               #:jonathan
               #:shasht
               #:closer-mop
               #:hu.dwim.defclass-star
               #:ironclad
               #:local-time)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "encryption")
               (:file "bearers")
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
               (:file "carlyle")
               (:module "v2"
                :serial t
                :components ((:file "package")
                             (:file "encryption")
                             (:file "bearers")
                             (:module "conditions"
                              :serial t
                              :components ((:file "conditions")
                                           (:file "condition-protocol")
                                           (:file "compose")))
                             (:module "api"
                              :serial t
                              :components ((:file "protocol")
                                           (:file "definer")))
                             (:file "carlyle")))))

