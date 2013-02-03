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

;;; An object is serialized into a buffer before being written to the
;;; stream in order to avoid corrupting the stream when a
;;; serialization error occurs.
;;;
;;; The raw object buffer is extracted from the stream before
;;; deserializing because it may have to be read more than once (for
;;; undefined package errors).

(in-package #:lfarm-common)

#-lfarm.with-text-serializer
(progn
  (defun serialize-to-buffer (object)
    (flexi-streams:with-output-to-sequence (out :element-type *element-type*)
      (backend-serialize object out)))

  (defun deserialize-buffer (buffer)
    (flexi-streams:with-input-from-sequence (in buffer)
      (backend-deserialize in))))

#+lfarm.with-text-serializer
(progn
  (defun serialize-to-buffer (object)
    (with-output-to-string (out nil :element-type *element-type*)
      (backend-serialize object out)))

  (defun deserialize-buffer (buffer)
    (backend-deserialize (make-string-input-stream buffer))))

(defun serialize (object stream)
  (let ((buffer (serialize-to-buffer object)))
    (backend-serialize (length buffer) stream)
    (write-sequence buffer stream))
  (force-output stream))

(defun read-serialized-buffer (stream)
  (let* ((expected-size (backend-deserialize stream))
         (buffer (make-array expected-size :element-type *element-type*))
         (actual-size (handler-case (read-sequence buffer stream)
                        (end-of-file ()
                          (error 'corrupted-stream-error :stream stream)))))
    (unless (eql expected-size actual-size)
      (error 'corrupted-stream-error :stream stream))
    buffer))

(defun deserialize (stream)
  (deserialize-buffer (read-serialized-buffer stream)))
