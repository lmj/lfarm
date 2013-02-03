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

(import-now usocket:socket-stream
            usocket:connection-refused-error
            usocket:timeout-error
            usocket:unknown-error)

(defvar *connect-retry-interval* 0.25)

(macrolet ((define-variant (name orig)
             `(defun ,name (&rest args)
                (multiple-value-call #',orig (values-list args)
                                     :element-type *element-type*))))
  (define-variant socket-listen* usocket:socket-listen)
  (define-variant socket-accept* usocket:socket-accept)
  (define-variant socket-connect* usocket:socket-connect))

(defun get-time ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun expiredp (start timeout)
  (>= (- (get-time) start)
      timeout))

(defmacro with-timeout ((timeout) &body body)
  (with-gensyms (timeout-value start)
    `(let* ((,timeout-value ,timeout)
            (,start (and ,timeout-value (get-time))))
       (flet ((timeout-expired-p ()
                (and ,timeout-value
                     (expiredp ,start ,timeout-value))))
         ,@body))))

(defun %socket-connect/retry (host port timeout)
  (info "socket-connect/retry" host port timeout)
  (with-timeout (timeout)
    (with-tag :retry
      (handler-case (socket-connect* host port)
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
