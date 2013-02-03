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

(import-now bordeaux-threads:make-lock
            bordeaux-threads:with-lock-held)

(defvar *log-level* :error
  "Set to :error to log only errors; set to :info for verbosity.")

(defvar *log-stream* *debug-io*
  "Stream for logging.")

(defvar *log-lock* (make-lock))

(defun timestamp ()
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun write-log (level package &rest args)
  (let ((timestamp (timestamp))
        (*print-readably* nil)
        (*print-pretty* nil)
        (*print-circle* t))
    (with-lock-held (*log-lock*)
      (format *log-stream* "~&~a ~a ~a ~{~a~^ ~}~%"
              timestamp level (string-downcase (package-name package)) args)
      (finish-output *log-stream*))))

(defmacro info (&rest args)
  `(case *log-level*
     (:info (write-log "info" ,*package* ,@args))))

(defmacro bad (&rest args)
  `(ccase *log-level*
     ((:info :error) (write-log "**ERROR**" ,*package* ,@args))))

(defmacro with-errors-logged (&body body)
  `(handler-bind ((error (lambda (err) (bad err))))
     ,@body))
