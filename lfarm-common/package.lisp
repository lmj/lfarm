
(defpackage #:lfarm-common
  (:documentation
   "(private) Common components for lfarm.")
  (:use #:cl)
  ;; util
  (:export #:with-gensyms
           #:when-let
           #:when-let*
           #:repeat
           #:defwith
           #:call-body
           #:with-tag
           #:dosequence
           #:unwind-protect/safe
           #:unwind-protect/safe-bind
           #:named-lambda
           #:import-now)
  ;; log
  (:export #:info
           #:bad
           #:with-errors-logged
           #:*log-level*
           #:*log-stream*)
  ;; socket
  (:export #:socket-connect/retry
           #:with-timeout
           #:timeout-expired-p
           #:socket-listen*
           #:socket-accept*
           #:socket-connect*
           #:*connect-retry-interval*)
  ;; address
  (:export #:ensure-addresses
           #:with-each-address
           #:with-each-address/handle-error)
  ;; errors
  (:export #:make-task-error-data
           #:task-error-data
           #:task-error-data-report
           #:task-error-data-desc
           #:corrupted-stream-error)
  ;; serialize
  (:export #:*element-type*
           #:serialize
           #:deserialize
           #:deserialize-buffer
           #:read-serialized-buffer))
