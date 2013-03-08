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

(defpackage #:lfarm-admin
  (:documentation
   "Administrative duties on a server (ping it or end it).")
  (:use #:cl
        #:lfarm-common)
  (:export #:ping
           #:end-server))

(in-package #:lfarm-admin)

(defun end-server (host port)
  "End the server at host:port.

This only stops new connections from being made. Connections in
progress are unaffected."
  (info "end-server" host port)
  (with-connected-stream (stream (socket-connect host port))
    (send-object :end-server stream)))

(defun write-ping (stream)
  (send-object :ping stream))

(defun read-pong (stream)
  (case (receive-object stream)
    (:pong)
    (otherwise (error 'corrupted-stream-error :stream stream))))

(defun send-ping (host port socket stream)
  (info "sending ping" host port)
  (write-ping stream)
  (info "ping sent" host port socket))

(defun receive-pong (socket timeout)
  (when (wait-for-input socket :timeout timeout)
    (info "detected pong" socket)
    (read-pong (socket-stream socket))
    (info "received pong" socket)
    t))

(defun ping (host port &key (timeout 3))
  "Send a ping to the lfarm server at host:port.

Keep trying to make contact for `timeout' seconds, or if `timeout' is
nil then try forever. Default is 3 seconds.

Returns true if successful and nil otherwise."
  (info "attempting ping" host port)
  (with-tag :retry
    (with-timeout (timeout)
      (multiple-value-bind (socket stream)
          (handler-case (socket-connect/retry host port :timeout timeout)
            (connection-refused-error () (return-from ping nil)))
        (send-ping host port socket stream)
        (cond ((receive-pong socket timeout)
               (info "got pong" socket timeout)
               t)
              ((timeout-expired-p)
               (info "ping timed out" socket timeout)
               nil)
              (t
               ;; Attempt to work around CCL bugs (not always successful).
               (info "weird socket state; retrying ping" socket timeout)
               (go :retry)))))))
