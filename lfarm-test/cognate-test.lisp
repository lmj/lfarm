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

(full-test plet-test
  (plet ((x 3)
         (y 4))
    (is (= 7 (+ x y))))
  #+lfarm.with-closures
  (let ((a 10))
    (plet ((x (+ a 3))
           (y (+ a 4)))
      (is (= 27 (+ x y))))))

(full-test pmap-into-test
  (let ((a (list nil nil nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15 17 19) a)))
  (let ((a (list nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equal '(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equal '(15) a)))
  (let ((a (vector nil nil nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15 17 19) a)))
  (let ((a (vector nil)))
    (pmap-into a '+ '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 2 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))
    (pmap-into a '+ :parts 3 '(5 6 7) '(10 11 12))
    (is (equalp #(15) a))))

(full-test pmap-test
  (is (equalp (map  'vector (lambda (x) (* x x)) #(3 4 5 6))
              (pmap 'vector (lambda (x) (* x x)) #(3 4 5 6))))
  (is (equalp (map  'vector (lambda (x) (* x x)) '(3 4 5 6))
              (pmap 'vector (lambda (x) (* x x)) '(3 4 5 6))))
  #-lfarm.with-text-serializer
  (let ((type '(simple-array fixnum (*))))
    (is (equalp (map  type (lambda (x) (* x x)) #(3 4 5 6))
                (pmap type (lambda (x) (* x x)) #(3 4 5 6))))
    (is (equalp (map  type (lambda (x) (* x x)) '(3 4 5 6))
                (pmap type (lambda (x) (* x x)) '(3 4 5 6))))))

(full-test degenerate-pmaps-test
  (is (eq (map  nil #'identity '(0 1 2 3))
          (pmap nil #'identity '(0 1 2 3))))
  (is (eq (map  nil 'identity '(0 1 2 3))
          (pmap nil 'identity '(0 1 2 3))))
  (is (eq (map-into  nil '+ '(2 3) '(4 5))
          (pmap-into nil '+ '(2 3) '(4 5))))
  (is (equalp (map  'vector #'identity '(0 1 2 3))
              (pmap 'vector #'identity '(0 1 2 3))))
  (is (equalp (map  'vector 'identity '(0 1 2 3))
              (pmap 'vector 'identity '(0 1 2 3))))
  (is (equalp (map-into  nil '+ '(2 3) '(4 5))
              (pmap-into nil '+ '(2 3) '(4 5)))))

(full-test pmapcar-test
  (is (equal '(15 17 19)
             (pmapcar '+ '(5 6 7) '(10 11 12))))
  (is (equal '(15 17 19)
             (pmapcar '+ :parts 2 '(5 6 7) '(10 11 12)))))

(full-test pmapcar-handles-sequences-test
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ '(1 2 3) #(4 5 6))))
  (is (equal (mapcar  '+ '(1 2 3) '(4 5 6))
             (pmapcar '+ :parts 2 '(1 2 3) #(4 5 6)))))

(deftask sq (x)
  (* x x))

(local-test pmap-parts-arg-test
  (loop
     :for parts :from 1 :to 8
     :do (loop
            :for n :from 1 :to 6
            :do (let ((a (loop :repeat n :collect (random n))))
                  (is (equalp ( map-into (make-array n) #'sq a)
                              (pmap-into (make-array n) #'sq :parts parts a)))
                  (is (equal  ( map-into (make-list n) #'sq a)
                              (pmap-into (make-list n) #'sq :parts parts a)))
                  (is (equalp ( map 'vector #'sq a)
                              (pmap 'vector #'sq :parts parts a)))
                  (is (equal  ( map 'list #'sq a)
                              (pmap 'list #'sq :parts parts a)))
                  (is (equal  ( mapcar #'sq a)
                              (pmapcar #'sq :parts parts a)))))))

(deftask seven ()
  7)

(full-test pmap-into-thunk-test
  (let ((a (make-array 3 :initial-element 1)))
    (is (equalp #(9 9 9)
                (pmap-into a (lambda () 9))))
    (is (equalp #(7 7 7)
                (pmap-into a 'seven)))
    (is (equalp #(7 7 7)
                (pmap-into a #'seven)))
    #+lfarm.with-closures
    (let ((c 10))
      (is (equalp #(19 19 19)
                  (pmap-into a (lambda () (+ c 9))))))))

#+lfarm.with-closures
(remote-test pmap-closure-test
  (let ((a 10))
    (is (equalp #(11 12 13)
                (pmap 'vector (lambda (x) (+ a x)) #(1 2 3))))
    (is (equal '(11 12 13)
               (pmapcar (lambda (x) (+ a x)) #(1 2 3))))
    (let ((result (make-array 3)))
      (is (equalp #(11 12 13)
                  (pmap-into result (lambda (x) (+ a x)) #(1 2 3))))
      (is (equalp #(11 12 13)
                  result)))))

(full-test pmap-with-size-constraint-test
  (is (equal '(2 11)
             (pmapcar '1+ :size 2 '(1 10 100 1000))))
  (is (equal '(2 11)
             (pmap 'list '1+ :size 2 '(1 10 100 1000))))
  (is (equalp #(2 11)
              (pmap 'vector '1+ :size 2 '(1 10 100 1000))))
  (is (equalp #(2 11)
              (pmap 'vector '1+ :size 2 #(1 10 100 1000))))
  (is (equalp #(2 11 99 99)
              (pmap-into (vector 99 99 99 99) '1+ :size 2 #(1 10 100 1000))))
  (is (equal '(2 11)
             (pmap-into (list 'a 'b) '1+ :size 2 '(1 10 100 1000))))
  (is (equal '(2 11 c d)
             (pmap-into (list 'a 'b 'c 'd) '1+ :size 2 '(1 10 100 1000)))))

(full-test pmap-into-edge-test
  (is (equalp #(1 2 3)
              (pmap-into (vector 9 9 9) 'identity (vector 1 2 3))))
  (is (equalp #(1 2 3)
              (pmap-into (vector 9 9 9) 'identity :size 3 (vector 1 2 3))))
  (is (equalp #(1 2 9)
              (pmap-into (vector 9 9 9) 'identity :size 2 (vector 1 2 3))))
  (is (equalp #(9 9 9)
              (pmap-into (vector 9 9 9) 'identity :size 0 (vector 1 2 3))))
  (is (equalp #(9 9 9)
              (pmap-into (vector 9 9 9) 'identity (vector))))
  (is (equalp #()
              (pmap-into (vector) 'identity (vector 1 2 3))))
  (let ((v (make-array 3 :fill-pointer 0)))
    (is (equalp #(1 2 3)
                (pmap-into v 'identity (vector 1 2 3))))
    (is (equalp #(1 2 3) v)))
  (let ((v (make-array 3 :fill-pointer 0)))
    (is (equalp #(1 2)
                (pmap-into v 'identity (vector 1 2))))
    (is (equalp #(1 2) v)))
  (let ((v (make-array 3 :fill-pointer 3)))
    (is (equalp #(1 2)
                (pmap-into v 'identity (vector 1 2))))
    (is (equalp #(1 2) v)))
  (let ((v (make-array 3 :fill-pointer 3)))
    (is (equalp #(1)
                (pmap-into v 'identity :size 1 (vector 1 2))))
    (is (equalp #(1) v))))

(deftask mul3 (x y z)
  (* x y z))

(full-test grind-pmap-test
  (dotimes (n 100)
    (let ((a (map-into (make-array n) (let ((i 0))
                                        (lambda ()
                                          (incf i)))))
          (b (map-into (make-array n) (let ((i 10))
                                        (lambda ()
                                          (incf i)))))
          (c (map-into (make-array n) (let ((i 20))
                                        (lambda ()
                                          (incf i))))))
      (is (equalp (map  'vector #'mul3 a b c)
                  (pmap 'vector #'mul3 a b c))))))

(full-test preduce-partial-test
  (signals simple-error
    (preduce-partial #'+ #() :initial-value 0))
  (signals simple-error
    (preduce-partial #'+ '() :initial-value 0))
  (signals simple-error
    (preduce-partial #'+ '()))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 1)
              #(52)))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 2)
              #(18 34)))
  (is (equalp (preduce-partial #'+ '(3 4 5 6 7 8 9 10) :parts 2 :from-end t)
              #(18 34)))
  (is (equalp (preduce-partial #'+ #(3 4 5 6 7 8) :parts 3 :from-end t)
              #(7 11 15)))
  (is (equalp (preduce-partial #'+ #(3 4 5 6 7 8) :parts 3)
              #(7 11 15))))

(deftask associative/non-commutative (a b)
  (vector (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 2)))
          (+ (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 3)))
          (+ (* (aref a 2) (aref b 0)) (* (aref a 3) (aref b 2)))
          (+ (* (aref a 2) (aref b 1)) (* (aref a 3) (aref b 3)))))

(defmacro collect-n (n &body body)
  "Execute `body' `n' times, collecting the results into a list."
  `(loop :repeat ,n :collect (progn ,@body)))

(full-test preduce-test
  (is (equalp (reduce  (lambda (x y) (+ x y)) #(1 2 3 4 5 6))
              (preduce (lambda (x y) (+ x y)) #(1 2 3 4 5 6))))
  #+lfarm.with-closures
  (let ((z 10))
    (is (equalp (reduce  (lambda (x y) (+ x y z)) #(1 2 3 4 5 6))
                (preduce (lambda (x y) (+ x y z)) #(1 2 3 4 5 6)))))
  (let ((a '(0 1 2 3 4 5 6 7))
        (b '((9 . 0) (9 . 1) (9 . 2) (9 . 3)))
        (c (collect-n 20 (random 100)))
        (d (collect-n 20 (vector (random 10)
                                 (random 10)
                                 (random 10)
                                 (random 10)))))
    (macrolet
        ((verify (test &rest args)
           `(loop :for parts :from 1 :to 10 :do
               (is (funcall ,test
                            (reduce ,@args)
                            (preduce ,@args)))
               (is (funcall ,test
                            (reduce ,@args)
                            (preduce ,@args :from-end t))))))
      (verify #'= #'+ a)
      (verify #'= #'+ a :initial-value 0)
      (verify #'= #'+ b :key #'cdr)
      (verify #'= #'+ c)
      (verify #'= #'+ c :initial-value 0)
      (verify #'= #'+ c :start 5)
      (verify #'= #'+ c :end 5)
      (verify #'= #'+ c :start 5 :end 16)
      (verify #'= #'+ c :start 5 :end 16 :from-end t)
      (verify #'= #'+ c :start 5 :end 16 :initial-value 0)
      (verify #'= #'* c :start 5 :end 16 :initial-value 1)
      (verify #'= #'* c :start 5 :end 16 :initial-value 1 :from-end t)

      (verify #'equalp #'associative/non-commutative d)
      (verify #'equalp #'associative/non-commutative d :start 5)
      (verify #'equalp #'associative/non-commutative d :end 5)
      (verify #'equalp #'associative/non-commutative d :start 5 :end 16)
      (verify #'equalp
              #'associative/non-commutative d
              :start 5
              :end 16
              :initial-value (vector 1 0 0 1))
      (verify #'equalp
              #'associative/non-commutative d
              :start 5
              :end 16
              :initial-value (vector 1 0 0 1)
              :from-end t))))

(full-test pmap-reduce-test
  (let ((c (collect-n 3 (random 100))))
    (is (equal (preduce #'+ c :key (lambda (x) (* x x)))
               (pmap-reduce (lambda (x) (* x x)) #'+ c)))
    (is (equal (+ 9 16 25)
               (pmap-reduce (lambda (x) (* x x)) #'+ '(3 4 5))))))
