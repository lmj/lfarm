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

(full-test basic-test
  (submit-task *channel* '+ 3 4)
  (is (= 7 (receive-result *channel*)))
  (submit-task *channel* '* 5 6)
  (is (= 30 (receive-result *channel*)))
  (submit-task *channel* #'+ 7 8)
  (is (= 15 (receive-result *channel*)))
  (submit-task *channel* 'floor 7 3)
  (is (= 2 (receive-result *channel*)))
  (submit-task *channel* (lambda () (values)))
  (is (equal nil (receive-result *channel*)))
  (let ((fn '+))
    (submit-task *channel* fn 1 2)
    (is (= 3 (receive-result *channel*)))))

(full-test lambda-test
  (submit-task *channel* (lambda (x y) (+ x y)) 3 4)
  (is (= 7 (receive-result *channel*)))
  (submit-task *channel* #'(lambda (x y) (* x y)) 5 6)
  (is (= 30 (receive-result *channel*)))
  (submit-task *channel* (lambda () 9))
  (is (= 9 (receive-result *channel*))))

(deftask foo (x y)
  (+ x y))

(deftask* bar (x y)
  (* x y))

(deftask* fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(deftask add2 (&key x y)
  (+ x y))

(deftask llk-check (&rest args &key x y z)
  (declare (ignore x y z))
  args)

(full-test deftask-test
  (submit-task *channel* 'foo 3 4)
  (is (= 7 (receive-result *channel*)))
  (submit-task *channel* #'foo 5 6)
  (is (= 11 (receive-result *channel*)))
  (submit-task *channel* 'bar 3 4)
  (is (= 12 (receive-result *channel*)))
  (submit-task *channel* #'bar 5 6)
  (is (= 30 (receive-result *channel*)))
  (let ((f 'foo))
    (submit-task *channel* f 1 2)
    (is (= 3 (receive-result *channel*))))
  (let ((f '(lambda (p q) (+ p q))))
    (submit-task *channel* f 1 2)
    (is (= 3 (receive-result *channel*))))
  (submit-task *channel* #'fib 10)
  (is (= 55 (receive-result *channel*)))
  (submit-task *channel* #'add2 :x 1 :y 2)
  (is (= 3 (receive-result *channel*)))
  (submit-task *channel* #'llk-check :x 1 :y 2 :z 3)
  (is (equal '(:x 1 :y 2 :z 3) (receive-result *channel*))))

(defvar *somevar* nil)

(deftask hello (&key world)
  world)

(full-test task-error-test
  (submit-task *channel* #'hello :z 9)
  (signals task-execution-error
    (receive-result *channel*))
  (submit-task *channel* 'blah 3 4)
  (signals task-execution-error
    (receive-result *channel*))
  (submit-task *channel* (lambda () (+ 3 *somevar*)))
  (signals task-execution-error
    (receive-result *channel*))
  (submit-task *channel* (lambda () (error "foo")))
  (signals task-execution-error
    (receive-result *channel*))
  (setf *somevar* nil)
  (submit-task *channel* (lambda () (funcall *somevar*)))
  (signals task-execution-error
    (receive-result *channel*))
  (let ((f #'foo))
    (signals invalid-task-error
      (submit-task *channel* f 1 2))))

(defwith with-temp-package (name)
  (unwind-protect/safe
   :prepare (make-package name)
   :main (call-body)
   :cleanup (delete-package name)))

(full-test package-test ()
  (let ((name :lfarm-test.bar))
    (with-temp-package (name)
      (let ((sym (intern "FOO" name)))
        (submit-task *channel* (lambda (x) x) sym)
        (is (eq sym (receive-result *channel*)))))))

#+lfarm.with-closures
(full-test package-test/closure ()
  (let ((name :lfarm-test.bar))
    (with-temp-package (name)
      (let ((sym (intern "FOO" name)))
        (submit-task *channel* (lambda () sym))
        (is (eq sym (receive-result *channel*)))))))

(full-test invalid-task-test
  (signals invalid-task-error
    (submit-task* *channel* #'+))
  (signals invalid-task-error
    (submit-task* *channel* '(junk 9)))
  (signals invalid-task-error
    (broadcast-task* *channel* #'+))
  (signals invalid-task-error
    (broadcast-task* *channel* '(junk 9)))
  (let ((f #'+))
    (signals invalid-task-error
      (submit-task *channel* f 3 4))
    (signals invalid-task-error
      (broadcast-task *channel* f 3 4))))

(base-test raw-local-test
  (let ((host *local-host*)
        (port (next-port)))
    (with-server (host port)
      (with-connection (connection host port)
        (let ((stream (socket-stream connection)))
          (send-object '(1111 + 3 4) stream)
          (is (= 7 (receive-object stream))))))))

(base-test raw-remote-test
  (let* ((host *remote-host*)
         (port (next-port)))
    (with-remote-servers (`((,host ,port)))
      (with-connection (connection host port)
        (let ((stream (socket-stream connection)))
          (send-object '(1111 + 3 4) stream)
          (is (= 7 (receive-object stream))))))))

(remote-test broadcast-test
  (is (not (find-package :lfarm-test.foo)))
  (broadcast-task (lambda () (make-package :lfarm-test.foo) nil))
  (submit-task *channel* (lambda () (and (find-package :lfarm-test.foo) 3)))
  (is (not (find-package :lfarm-test.foo)))
  (is (eql 3 (receive-result *channel*))))

#-abcl
(base-test reconnect-test
  (let ((host *local-host*)
        (port (next-port)))
    (with-server (host port)
      (with-kernel (*kernel* `((,host ,port)))
        (let ((channel (make-channel)))
          (lparallel:submit-task (lfarm-client.kernel::internal-channel channel)
                                 (lambda () lfarm-client.kernel::*connection*))
          (let ((connection (lparallel:receive-result
                             (lfarm-client.kernel::internal-channel channel))))
            (lfarm-client.kernel::end-connection connection)
            (submit-task channel #'+ 3 4)
            (is (= 7 (receive-result channel)))))))))

(base-test local-ping-test
  (let ((host *local-host*)
        (port (next-port)))
    (with-server (host port)
      (is (truep (ping host port))))))

(base-test remote-ping-test
  (let ((host *remote-host*)
        (port (next-port)))
    (with-remote-servers (`((,host ,port)))
      (is (truep (ping host port))))))

#-abcl
(base-test no-server-test
  (with-thread-count-check (0.4)
    (let ((host *local-host*)
          (port (next-port)))
      (is (null (ping host port :timeout 1)))
      (let (kernel)
        (unwind-protect/safe
         :main (progn
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (setf kernel (make-kernel `((,host ,port))))))
                 (sleep 0.4)
                 (with-server (host port)
                   (sleep 0.4)
                   (is (truep kernel))
                   (let* ((*kernel* kernel)
                          (channel (make-channel)))
                     (submit-task channel '+ 7 8)
                     (is (= 15 (receive-result channel))))))
         :cleanup (when kernel
                    (let ((*kernel* kernel))
                      (end-kernel :wait t))))))))

(remote-test unreadable-result-test
  (submit-task *channel* (lambda ()
                           (intern (string '#:blah)
                                   (make-package :abc :use nil))))
  (signals (or type-error reader-error package-error)
    (receive-result *channel*)))

(base-test big-data-test
  (let ((addresses `((,*local-host* ,(next-port)))))
    (with-local-servers (addresses)
      (with-kernel (*kernel* addresses)
        (is (= (length addresses) (kernel-worker-count)))
        (let ((channel (make-channel))
              (data (make-array 100 :initial-element 9)))
          (submit-task channel
                       (lambda (data)
                         (map 'vector (lambda (x) (* x x)) data))
                       data)
          (is (equalp (map 'vector (lambda (x) (* x x)) data)
                      (receive-result channel))))))))

(full-test circular-test
  (let ((list (list 1 2 3)))
    (setf (cdr (last list)) list)
    (submit-task *channel*
                 (lambda (list)
                   (+ (first list) (second list) (third list)))
                 list))
  (is (= 6 (receive-result *channel*))))

#-lparallel.without-kill
(base-test kill-test
  (let ((addresses `((,*local-host* ,(next-port))
                     (,*local-host* ,(next-port)))))
    (with-local-servers (addresses)
      ;; manually muffle warnings from worker threads
      (let ((*error-output* (make-broadcast-stream)))
        (with-kernel (kernel addresses)
          (let* ((*kernel* kernel)
                 (*channel* (make-channel)))
            (submit-task *channel* #'+ 3 4)
            (is (= 7 (receive-result *channel*)))
            (let ((lfarm-client:*task-category* 'sleeper))
              (submit-task *channel* 'sleep 9999))
            (sleep 0.2)
            (is (= 1 (count 'sleeper (task-categories-running))))
            (kill-tasks 'sleeper)
            (sleep 0.2)
            (is (every #'null (task-categories-running)))
            (signals task-killed-error
              (receive-result *channel*))
            (submit-task *channel* #'+ 5 6)
            (is (= 11 (receive-result *channel*)))
            (sleep 2)))))))

(base-test kernel-error-test
  (let ((host *local-host*)
        (port (next-port))
        (handler-called-p nil)
        (*kernel* nil))
    (with-server (host port)
      (signals no-kernel-error
        (make-channel))
      (with-kernel (kernel `((,host ,port)))
        (let ((channel (handler-bind
                           ((no-kernel-error
                             (lambda (err)
                               (declare (ignore err))
                               (setf handler-called-p t)
                               (invoke-restart 'store-value kernel))))
                         (make-channel))))
          (submit-task channel '+ 4 5)
          (is (not (null handler-called-p)))
          (is (= 9 (receive-result channel))))))))

(local-test submit-timeout-test
  (let ((channel (make-channel)))
    (submit-timeout channel 0.1 'timeout)
    (submit-task channel (lambda () 3))
    (is (eql 3 (receive-result channel)))
    (is (eq 'timeout (receive-result channel)))))

#-lparallel.without-kill
(local-test cancel-timeout-test
  (let* ((channel (make-channel))
         (timeout (submit-timeout channel 999 'timeout)))
    (sleep 0.2)
    (cancel-timeout timeout 'a)
    (is (eq 'a (receive-result channel)))))

(local-test try-receive-test
  (multiple-value-bind (a b) (try-receive-result *channel*)
    (is (null a))
    (is (null b)))
  (submit-task *channel* (lambda () 3))
  (sleep 0.5)
  (multiple-value-bind (a b) (try-receive-result *channel*)
    (is (eq t b))
    (is (= 3 a)))
  (multiple-value-bind (a b) (try-receive-result *channel*)
    (is (null a))
    (is (null b))))

#-lparallel.without-bordeaux-threads-condition-wait-timeout
(local-test try-receive-result-timeout-test
  (submit-task *channel*
               (lambda ()
                 (sleep 1.0)
                 99))
  (let ((flag nil))
    (make-thread (lambda ()
                   (sleep 0.25)
                   (setf flag t)))
    (multiple-value-bind (a b) (try-receive-result *channel* :timeout 0.5)
      (is (null a))
      (is (null b)))
    (is (eq t flag))
    (multiple-value-bind (a b) (try-receive-result *channel* :timeout 1.0)
      (is (= 99 a))
      (is (eq t b)))))

(local-test multi-receive-test
  (submit-task *channel* '+ 3 4)
  (submit-task *channel* '+ 5 6)
  (submit-task *channel* '+ 7 8)
  (let ((results nil))
    (do-fast-receives (r *channel* 3)
      (push r results))
    (is (equal '(7 11 15) (sort results #'<)))))

(full-test many-task-test
  (let ((n (ecase *log-level*
             (:info 5)
             (:error 1000))))
    (repeat n
      (submit-task *channel* (lambda ()))
      (is (null (receive-result *channel*))))
    (repeat n
      (submit-task *channel* (lambda ())))
    (repeat n
      (is (null (receive-result *channel*))))
    (repeat n
      (let ((*task-priority* :low))
        (submit-task *channel* (lambda ())))
      (is (null (receive-result *channel*))))
    (repeat n
      (let ((*task-priority* :low))
        (submit-task *channel* (lambda ()))))
    (repeat n
      (is (null (receive-result *channel*))))))

(base-test task-categories-test
  (with-local-setup (2)
    (is (notany #'identity (task-categories-running)))
    (let ((channel (make-channel)))
      (submit-task channel (lambda () (sleep 0.4)))
      (sleep 0.2)
      (is (eql 1 (count :default (task-categories-running))))))
  (with-local-setup (2)
    (let ((channel (make-channel)))
      (let ((*task-category* :foo))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 1 (count :foo (task-categories-running))))))
  (with-local-setup (2)
    (let ((channel (make-channel)))
      (let ((*task-category* 999))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 1 (count 999 (task-categories-running))))))
  (with-local-setup (2)
    (let ((channel (make-channel)))
      (let ((*task-category* :foo))
        (submit-task channel (lambda () (sleep 0.4)))
        (submit-task channel (lambda () (sleep 0.4))))
      (sleep 0.2)
      (is (eql 2 (count :foo (task-categories-running)))))))

(defparameter *nil* nil)

#-lparallel.without-kill
(base-test default-kill-task-test
  (let ((*error-output* (make-broadcast-stream)))
    (with-local-setup (2)
      (submit-task *channel* (lambda () (loop :until *nil*)))
      (submit-task *channel* (lambda () (loop :until *nil*)))
      (sleep 0.2)
      (submit-task *channel* (lambda () 'survived))
      (sleep 0.2)
      (kill-tasks *task-category*)
      (sleep 0.2)
      (let ((errors nil)
            (regulars nil))
        (repeat 3
          (handler-case (push (receive-result *channel*) regulars)
            (task-killed-error (e)
              (push e errors))))
        (is (= 2 (length errors)))
        (is (equal '(survived) regulars))))))

#-lparallel.without-kill
(base-test custom-kill-task-test
  (let ((*error-output* (make-broadcast-stream)))
    (with-remote-setup (2)
      (let ((*task-category* 'blah))
        (submit-task *channel* (lambda ()
                                 (let ((*nil* nil))
                                   (declare (special *nil*))
                                   (loop :until *nil*))))
        (submit-task *channel* (lambda ()
                                 (let ((*nil* nil))
                                   (declare (special *nil*))
                                   (loop :until *nil*)))))
      (sleep 0.2)
      (submit-task *channel* (lambda () 'survived))
      (sleep 0.2)
      (kill-tasks 'blah)
      (sleep 0.2)
      (let ((errors nil)
            (regulars nil))
        (repeat 3
          (handler-case (push (receive-result *channel*) regulars)
            (task-killed-error (e)
              (push e errors))))
        (is (= 2 (length errors)))
        (is (equal '(survived) regulars))))))

(local-test submit-after-end-kernel-test
  (let ((channel (make-channel)))
    (end-kernel :wait t)
    (signals error
      (submit-task channel (lambda ())))))

(local-test double-end-kernel-test
  (let ((addresses `((,*local-host* ,(next-port))
                     (,*local-host* ,(next-port)))))
    (with-local-servers (addresses)
      (let* ((kernel (make-kernel addresses))
             (*kernel* kernel))
        (end-kernel :wait t)
        (let ((*kernel* kernel))
          (end-kernel :wait t)))))
  ;; got here without an error
  (is (= 1 1)))

(defparameter *memo* nil)

#-lparallel.without-kill
(base-test resubmit-test
  (setf *memo* 0)
  (with-local-setup (1)
    (submit-task *channel* (lambda ()
                             (incf *memo*)
                             (sleep 0.6)
                             :done))
    (sleep 0.2)
    (dolist (thread (bordeaux-threads:all-threads))
      (when (string= "lfarm-server response TASK-LOOP"
                     (bordeaux-threads:thread-name thread))
        (bordeaux-threads:destroy-thread thread)))
    (is (eq :done (receive-result *channel*)))
    (is (= 2 *memo*))))
