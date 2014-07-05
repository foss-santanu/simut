;;;; simut.asd

(asdf:defsystem #:simut
  :serial t
  :description "My (sim)ple (u)nit (t)esting framework"
  :author "Santanu Chakrabarti <santanu.chakrabarti@gmail.com>"
  :version "0.1"
  :license "GPL"
  :components ((:file "package")
               (:file "simut")
               (:module "src"
                :components ((:file "unit-test-structs")))
               (:module "test"
                :components ((:file "test-unit-test-structs")))
               )
  )



