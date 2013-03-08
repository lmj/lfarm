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

(in-package #:lfarm-common.data-transport)

(define-condition auth-error (error)
  ()
  (:documentation
   "Raise this error or a subclass thereof when auth fails."))

(defgeneric initialize-server-stream (auth stream)
  (:documentation
   "Initialize a server-side stream. Return a new stream or the same
   stream."))

(defgeneric initialize-client-stream (auth stream server-name)
  (:documentation
   "Initialize a client-side stream connected to server named
   `server-name'. Return a new stream or the same stream."))

(defgeneric send-buffer (auth buffer stream)
  (:documentation
   "Send a (unsigned-byte 8) vector over a stream."))

(defgeneric receive-buffer (auth stream)
  (:documentation
   "Receive a (unsigned-byte 8) vector from a stream."))

(defmethod initialize-server-stream ((auth t) stream)
  (declare (ignore auth))
  stream)

(defmethod initialize-client-stream ((auth t) stream server-name)
  (declare (ignore auth server-name))
  stream)
