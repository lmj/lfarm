(defsystem :lfarm-gss
  :description "GSS-API support for lfarm"
  :license "BSD"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :depends-on (:lfarm-common
               :cl-gss
               :trivial-gray-streams)
  :serial t
  :components ((:module "lfarm-gss"
                        :serial t
                        :components ((:file "packages")
                                     (:file "wrapper-stream")
                                     (:file "lfarm-gss")))))
