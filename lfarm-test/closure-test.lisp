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

(in-package #:lfarm-test)

(defmacro closure-form (lambda-list &body body &environment env)
  (lfarm-client.kernel::maybe-make-closure-form nil lambda-list body env))

(defun closure-a (x)
  (let ((y (1+ x)))
    (closure-form (z)
      (+ x y z))))

(base-test closure-a-test
  (is (equal `(symbol-macrolet ()
                (let ((x '3)
                      (y '4))
                  (lambda (z)
                    (+ x y z))))
             (closure-a 3)))
  (is (= (+ 3 4 5)
         (funcall (eval (closure-a 3)) 5))))

(defun closure-b (x)
  (symbol-macrolet ((w (1- x)))
    (let ((y (1+ x)))
      (closure-form (z)
        (+ w x y z)))))

(base-test closure-b-test
  (is (equal `(symbol-macrolet ((w (1- x)))
                (let ((x '3)
                      (y '4))
                  (lambda (z)
                    (+ w x y z))))
             (closure-b 3)))
  (is (= (+ 2 3 4 5)
         (funcall (eval (closure-b 3)) 5))))

(defun closure-c (x)
  (let ((y (1+ x)))
    (closure-form (z)
      ;; use non-keyword loop keywords
      (loop repeat z collect x collect y))))

(base-test closure-c-test
  (is (equal `(symbol-macrolet ()
                (let ((x '3)
                      (y '4))
                  (lambda (z)
                    (loop repeat z collect x collect y))))
             (closure-c 3)))
  (is (equal '(3 4 3 4)
             (funcall (eval (closure-c 3)) 2))))

(defun closure-d (x)
  (let ((instance '(4 5)))
    (symbol-macrolet ((fst (first instance))
                      (snd (second instance)))
      (closure-form ()
        (+ x fst snd)))))

(base-test closure-d-test
  (is (equal (closure-d 3)
             `(symbol-macrolet ((fst (first instance))
                                (snd (second instance)))
                (let ((instance '(4 5))
                      (x '3))
                  (lambda ()
                    (+ x fst snd))))))
  (is (= (+ 3 4 5)
         (funcall (eval (closure-d 3))))))

(defclass stuff ()
  ((x :initarg :x)
   (y :initarg :y)))

(defun closure-e ()
  (let ((stuff (make-instance 'stuff :x 3 :y 4)))
    (with-slots (x y) stuff
      (closure-form ()
        (+ x y)))))

(base-test closure-e-test
  (is (= (+ 3 4)
         (funcall (eval (closure-e))))))

(defun closure-f ()
  (let ((x 3))
    (closure-form (&key y)
      (* x y))))

(base-test closure-f-test
  (is (equal `(symbol-macrolet ()
                (let ((x '3))
                  (lambda (&key y)
                    (* x y))))
             (closure-f)))
  (is (= (* 3 4)
         (funcall (eval (closure-f)) :y 4))))

(remote-test closure-task-test
  (let ((x 3))
    (submit-task *channel* (lambda (y) (+ x y)) 4)
    (is (= 7 (receive-result *channel*))))
  (let ((x 3))
    (declare (ignorable x))
    (submit-task *channel*
                 (lambda (y)
                   (let ((x 10))
                     (+ x y)))
                 4)
    (is (= 14 (receive-result *channel*))))
  (let ((x 3))
    (symbol-macrolet ((z (1+ x)))
      (submit-task *channel* (lambda (y) (+ x y z)) 4)
      (is (= 11 (receive-result *channel*)))))
  (broadcast-task (lambda () (defclass stuff ()
                               ((x :initarg :x)
                                (y :initarg :y)))))
  (let ((stuff (make-instance 'stuff :x 3 :y 4)))
    (with-slots (x y) stuff
      (submit-task *channel* (lambda () (+ x y)))
      (is (= 7 (receive-result *channel*))))))

(let ((x 3))
  (deftask closure-add-3 (y)
    (declare (fixnum y))
    (+ x y)))

(let ((x 3)
      (y 4))
  (deftask closure-twelve ()
    (* x y)))

(full-test deftask-closure-test
  (submit-task *channel* #'closure-add-3 4)
  (is (= 7 (receive-result *channel*)))
  (submit-task *channel* #'closure-twelve)
  (is (= 12 (receive-result *channel*))))

(defvar *reset-imperative-closure*)

(let ((a 1))
  (setf *reset-imperative-closure* (lambda () (setf a 1)))
  (deftask imperative-closure (n)
    (if (plusp n)
        (+ (incf a) (imperative-closure (- n 1)))
        0)))

(remote-test imperative-closure-test
  (funcall *reset-imperative-closure*)
  (submit-task *channel* #'imperative-closure 3)
  (is (= (imperative-closure 3) (receive-result *channel*)))
  (submit-task *channel* #'imperative-closure 3)
  (is (= (imperative-closure 3) (receive-result *channel*)))
  (submit-task *channel* #'imperative-closure 3)
  (is (= (imperative-closure 3) (receive-result *channel*))))

(local-test symbol-macrolet-test
  (symbol-macrolet ((a 3))
    (submit-task *channel* (lambda () a))
    (is (= 3 (receive-result *channel*)))
    (symbol-macrolet ((b a))
      (submit-task *channel* (lambda () b))
      (is (= 3 (receive-result *channel*)))
      (symbol-macrolet ((c b))
        (submit-task *channel* (lambda () c))
        (is (= 3 (receive-result *channel*)))))))
