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

(defpackage #:lfarm-test
  (:documentation
   "Test suite for lfarm.")
  (:use #:cl
        #:lfarm-common
        #:lfarm-server
        #:lfarm-client
        #:lfarm-launcher
        #:lfarm-admin
        #:lfarm-1am)
  (:export #:execute))

(in-package #:lfarm-test)

(defvar *local-host* "127.0.0.1")
(defvar *remote-host* "127.0.0.1")

(defvar *remote-log* nil)

;;; Probably too long, but better safe than sorry.
(defvar *wait-interval* 3)

(defvar *channel*)

;;; different ports for simultaneous testing
(defparameter *start-port* (or #+sbcl      10000
                               #+ccl       11000
                               #+lispworks 12000
                               #+allegro   13000
                               #+abcl      14000
                                           15000))

(defparameter *end-port* 65536)

(defvar *port* *start-port*)

(defun next-port ()
  (setf *port* (let ((next (1+ *port*)))
                 (if (< next *end-port*)
                     next
                     *start-port*))))

;;; Avoid having to type "yes" for verifying local hosts.
(defvar *extra-ssh-options* '("-o" "StrictHostKeyChecking=no"))

(defun truep (x) (not (null x)))

(defwith with-local-servers (addresses)
  (unwind-protect/safe
   :prepare (start-local-servers addresses)
   :main (call-body)
   :cleanup (end-local-servers addresses)))

(defwith with-remote-servers (addresses)
  (unwind-protect/safe
   :prepare (start-remote-servers addresses)
   :main (call-body)
   :cleanup (end-remote-servers addresses)))

(defwith with-kernel ((:vars kernel) addresses)
  (unwind-protect/safe-bind
   :bind (kernel (make-kernel addresses))
   :main (call-body kernel)
   :cleanup (let ((*kernel* kernel))
              (end-kernel :wait t))))

(defwith with-server (host port &rest args)
  (unwind-protect/safe
   :prepare (apply #'start-server host port :background t args)
   :main (call-body)
   :cleanup (end-server host port)))

(defwith with-connection ((:vars connection) host port)
  (unwind-protect/safe-bind
   :bind (connection (lfarm-client::make-connection host port))
   :main (call-body connection)
   :cleanup (lfarm-client::end-connection connection)))

(defun thread-count ()
  ;; ccl can spontaneously lose the initial thread
  #+ccl
  (count "Initial"
         (bordeaux-threads:all-threads)
         :key #'bordeaux-threads:thread-name
         :test-not #'string=)
  ;; allegro launches a domain name service
  #+allegro
  (count-if-not (lambda (name) (search "Domain Name" name :test #'equalp))
                (bordeaux-threads:all-threads)
                :key #'bordeaux-threads:thread-name)
  #-(or ccl allegro)
  (length (bordeaux-threads:all-threads)))

(defwith with-thread-count-check (sleep-sec)
  (sleep 0.4)
  (let ((old-thread-count (thread-count)))
    (multiple-value-prog1 (call-body)
      (sleep sleep-sec)
      (is (eql old-thread-count (thread-count))))))

(defwith with-local-setup (server-count)
  (with-thread-count-check (0.4)
    (let ((addresses (loop
                        :repeat server-count
                        :collect (list *local-host* (next-port)))))
      (with-local-servers (addresses)
        (with-kernel (*kernel* addresses)
          (let ((*channel* (make-channel)))
            (call-body)))))))

(defwith with-remote-setup (server-count)
  (with-thread-count-check (*wait-interval*)
    (let ((addresses (loop
                        :repeat server-count
                        :collect (list *remote-host* (next-port)))))
      (with-remote-servers (addresses)
        (with-kernel (*kernel* addresses)
          (let ((*channel* (make-channel)))
            (call-body)))))))

(defun stamp ()
  `(apply #'format nil
          "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d-~6,'0d"
          (append (reverse (butlast (multiple-value-list (get-decoded-time)) 3))
                  (list (let ((*random-state* (make-random-state t)))
                          (random (expt 10 6)))))))

(defun boot-log-form (path)
  `(progn
     (setf (symbol-value (intern (string '#:*log-stream*) :lfarm-server))
           (open (concatenate 'string (namestring ,path) "-" ,(stamp))
                 :direction :output
                 :if-exists :append
                 :if-does-not-exist :create))
     (setf (symbol-value (intern (string '#:*log-level*) :lfarm-server))
           ,*log-level*)))

(defun local-lisp ()
  (or #+sbcl (list (first sb-ext:*posix-argv*) "--eval")
      #+ccl (list (first (ccl::command-line-arguments)) "--eval")
      #+lispworks (list (first sys:*line-arguments-list*) "-eval")
      #+allegro (list (first (sys:command-line-arguments)) "-e")
      #+ecl (list (si:argv 0) "-eval")
      #+clisp (append (coerce (ext:argv) 'list) '("-x"))
      :need-to-supply-remote-lisp))

(defwith with-test-env ()
  (let ((*ssh-options* (append *extra-ssh-options* *ssh-options*))
        (*boot-form* (if *remote-log*
                         `(progn
                            ,*boot-form*
                            ,(boot-log-form *remote-log*))
                         *boot-form*))
        (*remote-lisp* (or *remote-lisp* (local-lisp)))
        (*connect-retry-interval* 0.1))
    (call-body)))

(defmacro base-test (name &body body)
  `(test ,name
     (format t "~&~a~%" ',name)
     (finish-output)
     (with-test-env
       ,@body)))

(defmacro local-test (name &body body)
  `(base-test ,name
     (with-local-setup (3)
       ,@body)))

(defmacro remote-test (name &body body)
  `(base-test ,name
     (with-remote-setup (3)
       ,@body)))

(defmacro full-test (name &body body)
  (let ((local (alexandria:symbolicate name '#:/local))
        (remote (alexandria:symbolicate name '#:/remote)))
    `(progn
       (local-test ,local ,@body)
       (remote-test ,remote ,@body)
       (defun ,name ()
         (,local)
         (,remote)))))

(defun execute (&key
                ((:remote-lisp *remote-lisp*) *remote-lisp*)
                ((:ssh-executable *ssh-executable*) *ssh-executable*)
                ((:ssh-options *ssh-options*) *ssh-options*)
                ((:ssh-user *ssh-user*) *ssh-user*)
                ((:boot-form *boot-form*) *boot-form*)
                ((:remote-log *remote-log*) *remote-log*)
                ((:remote-host *remote-host*) *remote-host*)
                ((:wait-interval *wait-interval*) *wait-interval*))
"Run the lfarm test suite.

`remote-lisp' -- The command to execute lisp on the remote machine (list
of strings). It must end with the implementation's eval switch. E.g.,
'(\"sbcl\" \"--eval\").

`ssh-executable' -- Local ssh executable (string). Default is \"ssh\".

`ssh-options' -- Command-line options passed to ssh (list of strings).

`ssh-user' -- Username on the remote machine (string).

`boot-form' -- Code to load lfarm-server.asd in the remote lisp image
\(and perhaps do other initialization). Default loads it with quicklisp.
Ensure that all symbols are in the CL or CL-USER package.

`remote-log' -- Pathname for the remote server log.

`wait-interval' -- Seconds to wait before verifying that all threads
have exited gracefully."
  (run!))

;;;; tests

(in-suite* :lfarm-test)

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

(local-test deftask-test
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
        (let ((stream (usocket:socket-stream connection)))
          (serialize '(1111 + 3 4) stream)
          (is (= 7 (deserialize stream))))))))

(base-test raw-remote-test
  (let* ((host *remote-host*)
         (port (next-port)))
    (with-remote-servers (`((,host ,port)))
      (with-connection (connection host port)
        (let ((stream (usocket:socket-stream connection)))
          (serialize '(1111 + 3 4) stream)
          (is (= 7 (deserialize stream))))))))

(remote-test broadcast-test
  (is (not (find-package :foo)))
  (broadcast-task (lambda () (defpackage :foo) nil))
  (submit-task *channel* (lambda () (and (find-package :foo) 3)))
  (is (not (find-package :foo)))
  (is (eql 3 (receive-result *channel*))))

#-abcl
(base-test reconnect-test
  (let ((host *local-host*)
        (port (next-port)))
    (with-server (host port)
      (with-kernel (*kernel* `((,host ,port)))
        (let ((channel (make-channel)))
          (lparallel:submit-task (lfarm-client::internal-channel channel)
                                 (lambda () lfarm-client::*connection*))
          (let ((connection (lparallel:receive-result
                             (lfarm-client::internal-channel channel))))
            (lfarm-client::end-connection connection)
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
