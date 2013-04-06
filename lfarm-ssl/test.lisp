;;;  Despite its name, this file does not contain any test cases. It's merely a
;;;  program used to set up the nescessary instances when working on the ssl code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #p"/Users/elias/prog/lfarm/" asdf:*central-registry*)
  (ql:quickload "lfarm-server")
  (ql:quickload "lfarm-client")
  (ql:quickload "lfarm-ssl"))

(let ((lfarm-common:*auth* (make-instance 'lfarm-ssl:ssl-auth-server
                                          :path "/Users/elias/z/server_cert.pem"
                                          :key "/Users/elias/z/server.pem"
                                          ;:password "foofoo"
                                          )))
  (lfarm-server:start-server "localhost" 7777 :background t))

(let ((lfarm-common:*auth* (make-instance 'lfarm-ssl:ssl-auth-client
                                          :path "/Users/elias/z/server_cert.pem"
                                          :key "/Users/elias/z/client.pem"
                                          ;:password "foofoo"
                                          )))
  (setq lfarm-client.kernel:*kernel* (lfarm:make-kernel '(("localhost" 7777)))))
