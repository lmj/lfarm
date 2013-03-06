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

(import-now lfarm-client.kernel::maybe-convert-task
            lfarm-client.kernel::maybe-convert-task-form)

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

;;;; subdivide

(defun find-num-parts (size parts-hint)
  (multiple-value-bind (quo rem) (floor size parts-hint)
    (values (if (zerop quo) rem parts-hint) quo rem)))

(defmacro with-parts (seq-size parts-hint &body body)
  (with-gensyms (quo rem index num-parts part-offset part-size)
    `(multiple-value-bind
           (,num-parts ,quo ,rem) (find-num-parts ,seq-size ,parts-hint)
       (let ((,index       0)
             (,part-offset 0)
             (,part-size   0))
         (flet ((next-part ()
                  (when (< ,index ,num-parts)
                    (unless (zerop ,index)
                      (incf ,part-offset ,part-size))
                    (setf ,part-size (if (< ,index ,rem) (1+ ,quo) ,quo))
                    (incf ,index)))
                (part-size   () ,part-size)
                (part-offset () ,part-offset)
                (num-parts   () ,num-parts))
           (declare (inline part-size part-offset num-parts)
                    (ignorable #'part-size #'part-offset #'num-parts))
           ,@body)))))

(defun zip/vector (seqs)
  (apply #'map 'vector (lambda (&rest args) args) seqs))

(defun find-min-length (seqs)
  (reduce #'min seqs :key #'length))

(defun get-parts-hint (parts-hint)
  (cond (parts-hint
         (check-type parts-hint (integer 1 #.most-positive-fixnum))
         parts-hint)
        (t
         (kernel-worker-count))))

(defmacro pop-plist (list)
  (check-type list symbol)
  `(loop
      :while (keywordp (car ,list))
      :collect (pop ,list)
      :collect (pop ,list)))

(defmacro pop-keyword-args (list &rest keys)
  (check-type list symbol)
  (with-gensyms (plist)
    `(when-let (,plist (pop-plist ,list))
       (destructuring-bind (&key ,@keys) ,plist
         (values ,@keys)))))

(defmacro pop-options (list)
  `(pop-keyword-args ,list size parts))

(defmacro with-parsed-options ((seqs size parts-hint) &body body)
  (check-type seqs symbol)
  (check-type size symbol)
  (check-type parts-hint symbol)
  `(multiple-value-bind (,size ,parts-hint) (pop-options ,seqs)
     (unless ,seqs
       (error "Input sequence(s) for parallelization not found."))
     (unless ,size
       (setf ,size (find-min-length ,seqs)))
     (setf ,parts-hint (get-parts-hint ,parts-hint))
     ,@body))

(defun subdivide-array (array size parts-hint)
  ;; Create copies, in contradistinction to lparallel. Otherwise we
  ;; send unnecessary data over the wire. A serialized displaced
  ;; vector includes its displaced-to vector.
  (with-parts size parts-hint
    (map-into (make-array (num-parts))
              (lambda ()
                (next-part)
                (replace (make-array (part-size)
                                     :element-type (array-element-type array))
                         array
                         :start2 (part-offset))))))

(defun subdivide-list (list size parts-hint)
  ;; Create copies, in contradistinction to lparallel. Otherwise we
  ;; send unnecessary data over the wire.
  (with-parts size parts-hint
    (loop
       :with p := list
       :while (next-part)
       :collect (loop
                   :repeat (part-size)
                   :collect (car p)
                   :do (setf p (cdr p))))))

(defun make-parts (result size parts-hint)
  (etypecase result
    (list (subdivide-list result size parts-hint))
    (vector (subdivide-array result size parts-hint))))

(defun make-input-parts (sequences size parts-hint)
  "Subdivide and interleave sequences for parallel mapping."
  (zip/vector (mapcar (lambda (seq) (make-parts seq size parts-hint))
                      sequences)))

;;;; task util

(defun receive-indexed (channel count)
  (loop
     :with result := (make-array count)
     :repeat count
     :do (destructuring-bind (index . data) (receive-result channel)
           (setf (aref result index) data))
     :finally (return result)))

(defun task->fn-form (task)
  (etypecase task
    (symbol `',task)
    (cons task)))

;;;; pmap

(defwith with-max-fill-pointer (seq)
  (if (and (vectorp seq)
           (array-has-fill-pointer-p seq))
      (let ((prev-fill-pointer (fill-pointer seq)))
        (unwind-protect/safe
         :prepare (setf (fill-pointer seq) (array-total-size seq))
         :main (call-body)
         :cleanup (setf (fill-pointer seq) prev-fill-pointer)))
      (call-body)))

(defun mapping-task (subresult-type task)
  `(lambda (subseqs part-index part-size)
     (cons part-index (apply #'map-into
                             (make-sequence ',subresult-type part-size)
                             ,(task->fn-form task)
                             subseqs))))

(defun subresult-type (result-seq)
  (let ((element-type (etypecase result-seq
                        (list t)
                        (vector (array-element-type result-seq)))))
    `(simple-array ,element-type (*))))

(defun pmap-into/submit (channel result-seq task sequences size parts-hint)
  (let* ((task (maybe-convert-task task))
         (mapping-task (mapping-task (subresult-type result-seq) task))
         (input-parts (make-input-parts sequences size parts-hint)))
    (with-parts size parts-hint
      (loop
         :for subseqs :across input-parts
         :for part-index :from 0
         :while (next-part)
         :do (submit-task channel mapping-task subseqs
                          part-index (part-size))))))

(defun pmap-into/receive (channel result-seq size parts-hint)
  (with-parts size parts-hint
    (let ((result-parts (receive-indexed channel (num-parts))))
      (with-max-fill-pointer (result-seq)
        (loop
           :for index :from 0
           :while (next-part)
           :do (replace result-seq (aref result-parts index)
                        :start1 (part-offset)
                        :end1 (+ (part-offset) (part-size))))))))

(defun pmap-into/parsed (result-seq task sequences size parts-hint)
  (let ((channel (make-channel)))
    (pmap-into/submit channel result-seq task sequences size parts-hint)
    (pmap-into/receive channel result-seq size parts-hint))
  result-seq)

(defun pmap/parsed (result-type function sequences size parts-hint)
  ;; do nothing for (pmap nil ...)
  (when result-type
    (pmap-into/parsed (make-sequence result-type size)
                      function
                      sequences
                      size
                      parts-hint)))

(defun pmap/unparsed (result-type function sequences)
  (with-parsed-options (sequences size parts-hint)
    (pmap/parsed result-type function sequences size parts-hint)))

(defun pmap/fn (result-type task first-sequence &rest more-sequences)
  (pmap/unparsed result-type task (cons first-sequence more-sequences)))

(defmacro pmap (result-type task first-sequence &rest more-sequences
                &environment env)
  "Parallel version of `map'. Keyword arguments `parts' and `size' are
also accepted.

The `parts' option divides each sequence into `parts' number of parts.
Default is (kernel-worker-count).

The `size' option limits the number of elements mapped to `size'. When
given, no `length' calls are made on the sequence(s) passed.

Warning: `size' must be less than or equal to the length of the
smallest sequence passed. It is unspecified what happens when that
condition is not met."
  `(pmap/fn ,result-type
            ,(maybe-convert-task-form task env)
            ,first-sequence
            ,@more-sequences))

(defun pmapcar/fn (task first-sequence &rest more-sequences)
  (apply #'pmap/fn 'list task (cons first-sequence more-sequences)))

(defmacro pmapcar (task first-sequence &rest more-sequences
                   &environment env)
  "Parallel version of `mapcar'. Keyword arguments `parts' and `size'
are also accepted (see `pmap').

Unlike `mapcar', `pmapcar' also accepts vectors."
  `(pmap/fn 'list
            ,(maybe-convert-task-form task env)
            ,first-sequence
            ,@more-sequences))

(defun pmap-into-thunk-form (task)
  (with-gensyms (x)
    (etypecase task
      (cons (destructuring-bind (head lambda-list &rest body) task
              (assert (eq head 'lambda))
              `(lambda (,x ,@lambda-list)
                 (declare (ignore ,x))
                 ,@body))))))

(defun pmap-into/unparsed (result-seq task seqs)
  (let ((task (maybe-convert-task task)))
    (multiple-value-bind (size parts-hint) (pop-options seqs)
      (let* ((has-fill-p (and (arrayp result-seq)
                              (array-has-fill-pointer-p result-seq)))
             (parts-hint (get-parts-hint parts-hint))
             (size       (or size
                             (let ((limit (if has-fill-p
                                              (array-total-size result-seq)
                                              (length result-seq))))
                               (if seqs
                                   (min limit (find-min-length seqs))
                                   limit)))))
        (prog1 (if seqs
                   (pmap-into/parsed result-seq task seqs
                                     size parts-hint)
                   (pmap-into/parsed result-seq
                                     (pmap-into-thunk-form task)
                                     (list result-seq)
                                     size
                                     parts-hint))
          (when has-fill-p
            (setf (fill-pointer result-seq) size)))))))

(defun pmap-into/fn (result-sequence task &rest sequences)
  (typecase result-sequence
    ((or array list)
     (pmap-into/unparsed result-sequence task sequences))
    (t
     (apply #'map-into result-sequence task sequences)))
  result-sequence)

(defmacro pmap-into (result-sequence task &rest sequences &environment env)
  "Parallel version of `map-into'. Keyword arguments `parts' and
`size' are also accepted (see `pmap')."
  `(pmap-into/fn ,result-sequence
                 ,(maybe-convert-task-form task env)
                 ,@sequences))
