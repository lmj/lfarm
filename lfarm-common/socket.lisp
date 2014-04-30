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

(in-package #:lfarm-common)

(defvar *auth* nil)

(defvar *connect-retry-interval* 0.25)

(defconstant +corrupt-stream-flag+ 'corrupt-stream-flag)

(defclass socket ()
  ((usocket :reader usocket :initarg :usocket)))

(defclass streaming-socket (socket)
  ((stream :reader socket-stream :initarg :stream)))

(defun make-socket (usocket)
  (make-instance 'socket :usocket usocket))

(defun make-streaming-socket (init-fn usocket &rest args)
  (unwind-protect/safe
   :main (let ((stream (apply init-fn *auth* (usocket:socket-stream usocket)
                              args)))
           (make-instance 'streaming-socket :usocket usocket :stream stream))
   :abort (usocket:socket-close usocket)))

(defun make-streaming-client-socket (usocket server-name)
  (make-streaming-socket #'initialize-client-stream usocket server-name))

(defun make-streaming-server-socket (usocket)
  (make-streaming-socket #'initialize-server-stream usocket))

(defun socket-listen (host port)
  (make-socket (usocket:socket-listen host port
                                      :reuse-address t
                                      :element-type *element-type*)))

(defun socket-accept (socket)
  (loop (when-let ((usocket (handler-case (usocket:socket-accept
                                           (usocket socket)
                                           :element-type *element-type*)
                              (connection-aborted-error ()))))
          (return (make-streaming-server-socket usocket)))))

(defun socket-connect (host port)
  (let ((usocket (usocket:socket-connect host port
                                         :element-type *element-type*)))
    (make-streaming-client-socket usocket host)))

(defwith with-connected-socket ((:vars socket-var) socket-value)
  (usocket:with-connected-socket (usocket (usocket socket-value))
    (call-body socket-value)))

(defwith with-connected-stream ((:vars stream-var) socket-value)
  (usocket:with-connected-socket (usocket (usocket socket-value))
    (call-body (socket-stream socket-value))))

(defun socket-close (socket)
  ;; data transport callback
  (stream-close *auth* (socket-stream socket))
  (usocket:socket-close (usocket socket)))

(defun wait-for-input (socket &key timeout)
  (usocket:wait-for-input (usocket socket) :timeout timeout :ready-only t))

(defun %socket-connect/retry (host port timeout)
  (info "socket-connect/retry" host port timeout)
  (with-timeout (timeout)
    (with-tag :retry
      (handler-case (socket-connect host port)
        ((or connection-refused-error timeout-error unknown-error) ()
          (when (timeout-expired-p)
            (info "socket-connect/retry timeout" host port)
            (error 'connection-refused-error))
         (info "socket-connect/retry again" host port timeout)
         (sleep *connect-retry-interval*)
         (go :retry))))))

(defun socket-connect/retry (host port &key timeout)
  (let ((socket (%socket-connect/retry host port timeout)))
    (values socket (socket-stream socket))))
