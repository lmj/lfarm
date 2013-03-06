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

;;;; Serialization of closures.

;;; Find free symbols inside the lambda and then use
;;; `variable-information' from CLTL2 to discover the lexical
;;; variables and symbol macros captured. `flet' functions are
;;; ignored.

(in-package #:lfarm-client.kernel)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-cltl2))

(defun variable-information (var env)
  (#+sbcl sb-cltl2:variable-information
   #+ccl ccl:variable-information
   #+allegro sys:variable-information
   #+lispworks hcl:variable-information
   var env))

(defun lexical-var-p (var env)
  (eq :lexical (variable-information var env)))

(defun symbol-macro-p (var env)
  (eq :symbol-macro (variable-information var env)))

#+(and sbcl (not lfarm.with-hu-walker))
(defun %find-free-symbols (lambda-list body)
  ;; SBCL code walker
  (let ((free-symbols nil))
    (flet ((visit (form context env)
             (declare (ignore context))
             (typecase form
               (symbol
                ;; If a lexical variable is recognized then it is
                ;; defined somewhere inside the lambda, which means it
                ;; is not captured. Similarly throw out
                ;; symbol-macrolets defined inside the lambda.
                ;;
                ;; lexical-var-p is sometimes wrong in conjunction
                ;; with sb-walker, so use sb-walker:var-lexical-p.
                (unless (or (sb-walker:var-lexical-p form env)
                            (symbol-macro-p form env))
                  (push form free-symbols))))
             form))
      (sb-walker:walk-form `(lambda ,lambda-list ,@body) nil #'visit)
      (remove-duplicates free-symbols))))

#+lfarm.with-hu-walker
(defun %find-free-symbols (lambda-list body)
  ;; hu.dwim code walker
  (flet ((walk (form fn)
           (hu.dwim.walker:rewrite-ast
            (hu.dwim.walker:walk-form form)
            (lambda (parent type form)
              (declare (ignore parent type))
              (typecase form
                (hu.dwim.walker:free-variable-reference-form
                 (funcall fn form)))
              form))))
    (let ((free-symbols nil))
      (handler-bind ((style-warning #'muffle-warning))
        (walk `(lambda ,lambda-list ,@body)
              (lambda (sym) (push sym free-symbols))))
      (remove-duplicates (mapcar #'hu.dwim.walker:name-of
                                 free-symbols)))))

#+(and (not sbcl) (not lfarm.with-hu-walker))
(defun %find-free-symbols (lambda-list body)
  ;; Fake code walker: grab all symbols which are not the head of a
  ;; form. The worst case outcome is overshoot: capturing more
  ;; variables than necessary. This will cause an error if a
  ;; needlessly captured variable is not serializable, but is
  ;; otherwise harmless apart from the increased bandwidth.
  (labels ((walk-symbols (form fn)
             (typecase form
               (symbol (funcall fn form))
               (cons (destructuring-bind (first &rest rest) form
                       (typecase first
                         (symbol) ; skip
                         (cons (walk-symbols first fn)))
                       (mapcar (lambda (form) (walk-symbols form fn))
                               rest))))))
    (let ((free-symbols nil))
      (walk-symbols `(progn ,@body)
                    (lambda (sym) (push sym free-symbols)))
      (set-difference (remove-duplicates free-symbols)
                      (lambda-list-parameters lambda-list)))))

(defun find-symbol-macros (syms env)
  (remove-if-not (lambda (sym) (symbol-macro-p sym env)) syms))

(defun sort-symbols (symbols)
  (sort symbols #'string< :key #'symbol-name))

(defun find-free-symbols (lambda-list body env
                          &optional (known-symbol-macros nil))
  ;; Expand symbol macros only once; any more could lead to an
  ;; unportable result.
  ;;
  ;; We must track `known-symbol-macros' when using the fake walker.
  (let* ((initial-syms (remove-duplicates
                        (%find-free-symbols lambda-list body)))
         (symbol-macros (set-difference (find-symbol-macros initial-syms env)
                                        known-symbol-macros))
         (more-syms (mapcan (lambda (sym-mac)
                              (find-free-symbols
                               lambda-list
                               (list (macroexpand-1 sym-mac env))
                               env
                               (append symbol-macros known-symbol-macros)))
                            symbol-macros)))
    ;; The only reason to sort is to make testing easier.
    (sort-symbols (remove-duplicates (append initial-syms more-syms)))))

(defun find-captures (lambda-list body env)
  (loop
     :for sym :in (find-free-symbols lambda-list body env)
     :if (lexical-var-p sym env)  :collect sym :into lexicals :else
     :if (symbol-macro-p sym env) :collect sym :into symbol-macros
     :finally (return (values lexicals symbol-macros))))

(defun make-closure-form (name lambda-list body env lexicals symbol-macros)
  (let ((lambda-type (if name 'named-lambda 'lambda)))
    ``(symbol-macrolet
          (,,@(loop
                 :for var :in symbol-macros
                 :collect ``(,',var ,',(macroexpand-1 var env))))
        (let (,,@(loop
                    :for var :in lexicals
                    :collect ``(,',var
                                (deserialize-buffer
                                 ,(serialize-to-buffer ,var)))))
          (,',lambda-type ,@',(unsplice name) ,',lambda-list
            ,@',body)))))

(defun maybe-make-closure-form (name lambda-list body env)
  (multiple-value-bind (lexicals symbol-macros)
      (find-captures lambda-list body env)
    (and (or lexicals symbol-macros)
         (make-closure-form name lambda-list body env lexicals symbol-macros))))

(defun closure-lambda-form (form)
  (let ((args (make-symbol (string 'args))))
    ``(lambda (&rest ,',args)
        (apply ,,form ,',args))))

(defun serialize-lambda (lambda-list body env &key name)
  "If no closure is detected, return a lambda form.

If a closure is detected, generate code for a closure in which the
values of the closed-over variables are assigned their current values.
When evaluated, the generated code produces a closure equivalent (for
our purposes) to a regular lambda with the given `lambda-list' and
`body'. The difference is that it holds a snapshot of the closed-over
variables rather than the original variables themselves."
  ;; Need to return a lambda form for `compile'.
  (let ((closure-form (maybe-make-closure-form name lambda-list body env)))
    (if closure-form
        (closure-lambda-form closure-form)
        (lambda-form name lambda-list body))))
