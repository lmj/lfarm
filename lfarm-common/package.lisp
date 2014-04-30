;;; Copyright (c) 2013, James M. Lawrence. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;;
;;;     * Neither the name of the project nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage #:lfarm-common.data-transport
  (:use #:cl)
  (:export #:auth-error
           #:initialize-client-stream
           #:initialize-server-stream
           #:send-buffer
           #:receive-buffer
           #:stream-close))

(defpackage #:lfarm-common
  (:documentation
   "(private) Common components for lfarm.")
  (:use #:cl
        #:lfarm-common.data-transport)
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
           #:lambda-list-parameters
           #:with-timeout
           #:timeout-expired-p
           #:alias-macro
           #:alias-function
           #:unsplice)
  ;; threads
  (:export #:make-thread
           #:make-lock
           #:with-lock-held
           #:current-thread
           #:destroy-thread
           #:with-lock-predicate/wait)
  ;; log
  (:export #:info
           #:bad
           #:with-errors-logged
           #:*log-level*
           #:*log-stream*)
  ;; socket
  (:export #:*auth*
           #:+corrupt-stream-flag+
           #:connection-refused-error
           #:socket-listen
           #:socket-accept
           #:socket-connect
           #:socket-connect/retry
           #:socket-stream
           #:socket-close
           #:wait-for-input
           #:with-connected-socket
           #:with-connected-stream
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
  ;; object transport
  (:export #:*element-type*
           #:send-object
           #:receive-object
           #:serialize-to-buffer
           #:deserialize-buffer
           #:receive-serialized-buffer)
  ;; imports
  (:import-from #:alexandria
                #:with-gensyms
                #:when-let
                #:when-let*
                #:named-lambda)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:make-lock
                #:with-lock-held
                #:current-thread
                #:destroy-thread)
  (:import-from #:usocket
                #:timeout-error
                #:unknown-error
                #:connection-aborted-error
                #:connection-refused-error))
