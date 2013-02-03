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

#+sbcl
(progn
  (defmacro without-interrupts (&body body)
    `(sb-sys:without-interrupts
       ,@body))

  (defmacro with-interrupts (&body body)
    `(sb-sys:with-local-interrupts
       ,@body)))

#+ccl
(progn
  (defmacro without-interrupts (&body body)
    `(ccl:without-interrupts
       ,@body))

  (defmacro with-interrupts (&body body)
    `(ccl:with-interrupts-enabled
       ,@body)))

#-(or sbcl ccl)
(progn
  (defmacro without-interrupts (&body body)
    `(progn ,@body))

  (defmacro with-interrupts (&body body)
    `(progn ,@body)))

(defmacro unwind-protect/safe (&key prepare main cleanup abort)
  "Interrupt-safe `unwind-protect'.

`prepare' : executed first, outside of `unwind-protect'
`main'    : protected form
`cleanup' : cleanup form
`abort'   : executed if `main' does not finish
"
  (with-gensyms (finishedp)
    `(without-interrupts
       ,@(unsplice (when prepare
                     `(with-interrupts
                        ,prepare)))
       ,(cond ((and main cleanup abort)
               `(let ((,finishedp nil))
                  (declare (type boolean ,finishedp))
                  (unwind-protect
                       (with-interrupts
                         (multiple-value-prog1 ,main
                           (setf ,finishedp t)))
                    (if ,finishedp
                        ,cleanup
                        (unwind-protect ,abort ,cleanup)))))
              ((and main cleanup)
               `(unwind-protect
                     (with-interrupts
                       ,main)
                  ,cleanup))
              ((and main abort)
               `(let ((,finishedp nil))
                  (declare (type boolean ,finishedp))
                  (unwind-protect
                       (with-interrupts
                         (multiple-value-prog1 ,main
                           (setf ,finishedp t)))
                    (when (not ,finishedp)
                      ,abort))))
              (main main)
              (cleanup `(progn ,cleanup nil))
              (abort nil)
              (t nil)))))

(defmacro unwind-protect/safe-bind (&key bind main cleanup)
  "Bind a variable inside `unwind-protect' with interrupt safety."
  (destructuring-bind (var value) bind
    (with-gensyms (uninitialized)
      `(let ((,var ',uninitialized))
         (without-interrupts
           (unwind-protect (progn
                             (setf ,var (with-interrupts
                                          ,value))
                             (with-interrupts
                               ,main))
             (when (not (eq ,var ',uninitialized))
               ,cleanup)))))))
