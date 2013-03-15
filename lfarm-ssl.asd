(defsystem :lfarm-ssl
  :description "SSL support for lfarm"
  :license "BSD"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :depends-on (:lfarm-common
               :cl+ssl)
  :serial t
  :components ((:module "lfarm-ssl"
                        :serial t
                        :components ((:file "packages")
                                     (:file "lfarm-ssl")))))
