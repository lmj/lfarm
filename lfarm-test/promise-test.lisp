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

(base-test promises-test
  (let ((a (promise))
        (b (promise)))
    (fulfill a 3)
    (fulfill b 4)
    (is (= 12 (* (force a) (force b)))))
  (let ((a (promise)))
    (is (eq t (fulfill a 3)))
    (is (eq nil (fulfill a 4)))
    (is (= 3 (force a)))))

(full-test promises-multiple-value-test
  (let ((x (promise)))
    (fulfill x (values 3 4 5))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (= 4 q))
      (is (= 5 r))))
  (let ((x (future (values 3 4 5))))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r))))
  (let ((x (delay (values 3 4 5))))
    (multiple-value-bind (p q r) (force x)
      (is (= 3 p))
      (is (not (null q)))
      (is (= 4 q))
      (is (= 5 r)))))

(defclass some-data ()
  ((x :initarg :x)
   (y :initarg :y)))

(full-test futures-test
  (let ((a (future 3))
        (b (future 4)))
    (is (= 7 (+ (force a) (force b)))))
  (let ((a (future 5)))
    (sleep 0.5)
    (is (fulfilledp a))
    (is (= 5 (force a))))
  (let ((a (future (sleep 1.0) 3)))
    (is (not (fulfilledp a)))
    (sleep 0.5)
    (is (eq nil (fulfill a 4)))
    (is (not (fulfilledp a)))
    (is (= 3 (force a))))
  (let ((a (future 3)))
    (sleep 0.5)
    (is (eq nil (fulfill a 9)))
    (is (= 3 (force a))))
  (let ((f (future (values (+ 3 4) :foo))))
    (is (= 7 (force f)))
    (is (eq :foo (nth-value 1 (force f))))))

#+lfarm.with-closures
(remote-test future-closure-test
  (let ((x 5)
        (y 6))
    (let ((f (future (+ x y))))
      (is (= 11 (force f)))))
  (broadcast-task (lambda ()
                    (defclass some-data ()
                      ((x :initarg :x)
                       (y :initarg :y)))))
  (let ((some-data (make-instance 'some-data :x 7 :y 8)))
    (with-slots (x y) some-data
      (let ((f (future (+ x y))))
        (is (= 15 (force f))))))
  (signals cl-store:store-error
    (let* ((f (future (+ 3 4)))
           (g (future (+ (force f) 5))))
      (force g))))

(full-test future-error-test
  (let ((f (future (error "foo"))))
    (signals task-execution-error
      (force f))
    (signals task-execution-error
      (force f))))
