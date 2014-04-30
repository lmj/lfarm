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
        #:lfarm-test.1am)
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

(defmacro unwind-protect/safe* (&key prepare main cleanup abort)
  `(unwind-protect/safe
    :prepare ,prepare
    :main ,main
    :cleanup (lfarm-common::with-interrupts ,cleanup)
    :abort (lfarm-common::with-interrupts ,abort)))

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

(defwith with-kernel ((:vars kernel) addresses &rest args)
  (unwind-protect/safe-bind
   :bind (kernel (apply #'make-kernel addresses args))
   :main (call-body kernel)
   :cleanup (let ((*kernel* kernel))
              (end-kernel :wait t))))

(defwith with-server (host port &rest args)
  (unwind-protect/safe*
   :prepare (apply #'start-server host port :background t args)
   :main (call-body)
   :cleanup (end-server host port)))

(defwith with-connection ((:vars connection) host port)
  (unwind-protect/safe-bind
   :bind (connection (lfarm-client.kernel::make-connection host port))
   :main (call-body connection)
   :cleanup (lfarm-client.kernel::end-connection connection)))

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
                ((:wait-interval *wait-interval*) *wait-interval*)
                ((:auth *auth*) *auth*))
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
  (run))
