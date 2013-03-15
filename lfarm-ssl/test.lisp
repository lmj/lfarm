;;;  Despite its name, this file does not contain any test cases. It's merely a
;;;  program used to set up the nescessary instances when working on the ssl code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #p"/Users/elias/prog/lfarm/" asdf:*central-registry*)
  (ql:quickload "lfarm-server")
  (ql:quickload "lfarm-client")
  (ql:quickload "lfarm-ssl"))

(setq lfarm-common:*auth* (make-instance 'lfarm-ssl:ssl-auth
                                         :server-cert-data (make-instance 'lfarm-ssl:cert-data
                                                                          :path "/Users/elias/z/server.crt"
                                                                          :key "/User/elias/z/server.key"
                                                                          :password "foofoo"
                                                                          )
                                         :client-cert-data (make-instance 'lfarm-ssl:cert-data
                                                                          ;:path #p"/Users/elias/z/server.crt"
                                                                          ;:key #p"/User/elias/z/server.key"
                                                                          ;:password "foofoo"
                                                                          )))

(lfarm-server:start-server "localhost" 7777 :background t)
(setq lfarm-client.kernel:*kernel* (lfarm:make-kernel '(("localhost" 7777))))
