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

(in-package #:lfarm-client.cognate)

;;;; plet

(defun pairp (form)
  (and (consp form) (eql (length form) 2)))

(defun parse-bindings (bindings)
  (let* ((pairs     (remove-if-not #'pairp bindings))
         (non-pairs (remove-if     #'pairp bindings))
         (syms      (loop
                       :for (name nil) :in pairs
                       :collect (gensym (symbol-name name)))))
    (values pairs non-pairs syms)))

(defmacro plet (bindings &body body)
  "The syntax of `plet' matches that of `let'.

  plet ({var-no-init | (var [init-form])}*) form*

For each (var init-form) pair, a future is created which executes
`init-form'. Inside `body', `var' is a symbol macro which expands to a
`force' form for the corresponding future.

Each `var-no-init' is bound to nil and each `var' without `init-form'
is bound to nil (no future is created)."
  (multiple-value-bind (pairs non-pairs syms) (parse-bindings bindings)
    `(symbol-macrolet ,(loop
                          :for sym :in syms
                          :for (name nil) :in pairs
                          :collect `(,name (force ,sym)))
       (let (,@(loop
                  :for sym :in syms
                  :for (nil form) :in pairs
                  :collect `(,sym (future ,form)))
             ,@non-pairs)
         ,@body))))
