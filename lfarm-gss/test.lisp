(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "lfarm-server")
  (ql:quickload "lfarm-client")
  (ql:quickload "lfarm-gss"))

(setq lfarm-common:*log-level* :info)
(setq lfarm-common:*auth* (make-instance 'lfarm-gss:gss-auth))

(lfarm-server:start-server (machine-instance) 4500 :background t)
(setf lfarm:*kernel* (lfarm:make-kernel `((,(machine-instance) 4500))))
