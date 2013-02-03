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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unsplice (form)
    (if form (list form) nil))

  (defun strip-aux (lambda-list)
    (subseq lambda-list 0 (position '&aux lambda-list)))

  (defun params (lambda-list)
    (multiple-value-bind (reqs opts rest keys)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (remove-if #'null (append reqs
                                (mapcar #'first opts)
                                (mapcar #'third opts)
                                (list rest)
                                (mapcar #'cadar keys)
                                (mapcar #'third keys))))))

(defmacro flet-alias ((name fn) &body body)
  `(flet ((,name (&rest args)
            (declare (dynamic-extent args))
            (apply ,fn args)))
     (declare (inline ,name))
     ,@body))

(defmacro define-with-fn (macro-name fn-name lambda-list declares body)
  (alexandria:with-gensyms (body-fn)
    `(defun ,fn-name (,body-fn ,@lambda-list)
       ,@declares
       (declare (type function ,body-fn))
       (block ,macro-name
         (flet-alias (call-body ,body-fn)
           ,@body)))))

(defmacro define-with-macro (macro-name fn-name lambda-list vars doc)
  (let ((macro-params (append vars (params lambda-list))))
    (alexandria:with-gensyms (whole body)
      `(defmacro ,macro-name (,@(when macro-params `(&whole ,whole))
                              ,@(unsplice `(,@vars ,@lambda-list))
                              &body ,body)
         ,@(unsplice doc)
         (declare (ignore ,@(params lambda-list)))
         `(,',fn-name (lambda (,,@vars) ,@,body)
                      ,@,(when macro-params
                           `(subseq (second ,whole) ,(length vars))))))))

(defmacro defwith (macro-name lambda-list &body body)
  "Define a function along with a macro that expands to a call of that
function. Inside `defwith' is an flet named `call-body'.

  (defwith with-foo (value)
    (let ((*foo* value))
      (call-body)))

is equivalent to

  (defun call-with-foo (body-fn value)
    (let ((*foo* value))
      (funcall body-fn)))

  (defmacro with-foo ((value) &body body)
    `(call-with-foo (lambda () ,@body) ,value))

Placing a `:vars' form at the head of the lambda list will generate a
macro that assigns to the given variables.

  (defwith with-add-result ((:vars result) x y)
    (call-body (+ x y)))

is equivalent to

  (defun call-with-add-result (body-fn x y)
    (funcall body-fn (+ x y)))

  (defmacro with-add-result ((result x y) &body body)
    `(call-with-add-result (lambda (,result) ,@body) ,x ,y))
"
  (let ((fn-name (alexandria:symbolicate '#:call- macro-name))
        (vars (case (ignore-errors (caar lambda-list))
                (:vars (prog1 (cdar lambda-list)
                         (pop lambda-list)))
                (otherwise nil))))
    (multiple-value-bind (body declares doc)
        (alexandria:parse-body body :documentation t)
      `(progn
         (define-with-fn
             ,macro-name ,fn-name ,lambda-list ,declares ,body)
         (define-with-macro
             ,macro-name ,fn-name ,(strip-aux lambda-list) ,vars ,doc)))))
