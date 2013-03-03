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

(in-package #:lfarm-client.promise)

(import-now bordeaux-threads:make-lock
            bordeaux-threads:with-lock-held)

;;;; util

(defmacro with-lock-predicate/wait (lock predicate &body body)
  ;; predicate intentionally evaluated twice
  `(when ,predicate
     (with-lock-held (,lock)
       (when ,predicate
         ,@body))))

;;;; promises

;;; Avoid `defmethod' since there are outstanding issues with
;;; concurrent method calls.

(defconstant +no-result+ 'no-result)

(defclass future ()
  ((result :initform +no-result+)
   (lock :initform (make-lock))
   (channel :initform (make-channel))))

(defwith with-unfulfilled (future)
  (with-slots (result lock) future
    (with-lock-predicate/wait lock (eq result +no-result+)
      (call-body))))

(defmacro future (&body body)
  (with-gensyms (future channel)
    `(let ((,future (make-instance 'future)))
       (with-slots ((,channel channel)) ,future
         (submit-task ,channel
                      (lambda ()
                        (multiple-value-list (progn ,@body)))))
       ,future)))

(defun force-future (future)
  (with-slots (result channel) future
    (with-unfulfilled (future)
      (setf result (receive-result channel)))
    (values-list result)))

(defun force (promise)
  (typecase promise
    (future (force-future promise))
    (otherwise (lparallel:force promise))))

(defmacro speculate (&body body)
  `(let ((lparallel:*task-priority* :low))
     (force ,@body)))

(defun fulfill-future (future fn)
  (declare (ignore future fn))
  ;; Doing this properly would involve creating a callback for when
  ;; the task is submitted. Don't seem worth it (yet).
  nil)

(defun %fulfill (promise fn)
  (typecase promise
    (future (fulfill-future promise fn))
    (otherwise (lparallel:fulfill promise (funcall fn)))))

(defmacro fulfill (promise &body body)
  `(%fulfill ,promise (lambda () ,@body)))

(defun maybe-update-status (future)
  (with-unfulfilled (future)
    (with-slots (result channel) future
      (multiple-value-bind (value foundp) (try-receive-result channel)
        (when foundp
          (setf result value))))))

(defun fulfilled-future-p (future)
  (maybe-update-status future)
  (with-slots (result) future
    (not (eq result +no-result+))))

(defun fulfilledp (promise)
  (typecase promise
    (future (fulfilled-future-p promise))
    (otherwise (lparallel:fulfilledp promise))))

(alias-function promise lparallel:promise)
(alias-macro delay lparallel:delay)

;;; Thwart attempts to serialize promises.
#-lfarm.with-text-serializer
(macrolet ((define-store-lock (type)
             `(cl-store:defstore-cl-store (obj ,type stream)
                (declare (ignore obj stream))
                (cl-store:store-error "Cannot store object of type ~s."
                                      ',type))))
  (define-store-lock #.(type-of (make-lock))))
