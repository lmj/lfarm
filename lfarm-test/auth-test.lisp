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

(in-package #:lfarm-test)

(defclass password-auth ()
  ((password :reader password :initarg :password)))

(define-condition password-auth-error (lfarm-common.data-transport:auth-error)
  ()
  (:report "bad password"))

(defmethod lfarm-common.data-transport:initialize-client-stream
    ((auth password-auth) stream server-name)
  (declare (ignore server-name))
  (info "initializing client stream for auth password")
  (send-object (password auth) stream)
  (info "client sent password")
  (handler-case (ecase (receive-object stream)
                  (:ok (info "client notified that password accepted")))
    (end-of-file ()
      (info "client detected that server rejected password")
      (error 'password-auth-error)))
  stream)

(defmethod lfarm-common.data-transport:initialize-server-stream
    ((auth password-auth) stream)
  (info "initializing server stream for auth password")
  (cond ((equal (password auth) (receive-object stream))
         (info "server accepted password")
         (send-object :ok stream))
        (t
         (error 'password-auth-error)))
  stream)

(defmethod lfarm-common.data-transport:send-buffer
    ((auth password-auth) buffer stream)
  (call-next-method auth
                    (case (deserialize-buffer buffer)
                      (7 (serialize-to-buffer 8))
                      (11 (error "11 is not allowed"))
                      (otherwise buffer))
                    stream))

(base-test auth-test
  (let ((host *local-host*)
        (port (next-port))
        (*auth* (make-instance 'password-auth :password "hello")))
    (with-server (host port)
      (with-kernel (*kernel* `((,host ,port)))
        (let ((channel (make-channel)))
          (submit-task channel #'+ 3 4)
          ;; 3 + 4 = 8, hooray!
          (is (= 8 (receive-result channel)))
          (submit-task channel #'+ 5 6)
          (signals task-execution-error
            (receive-result channel))))
      (let ((pass (make-instance 'password-auth :password "world")))
        (let ((*auth* pass))
          (signals kernel-creation-error
            (make-kernel `((,host ,port)))))
        (signals kernel-creation-error
          (make-kernel `((,host ,port)) :auth pass))))))
