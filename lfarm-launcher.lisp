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

(defpackage #:lfarm-launcher
  (:documentation
   "Launch servers locally or remotely.")
  (:use #:cl
        #:lfarm-common
        #:lfarm-server
        #:lfarm-admin)
  (:export #:start-local-servers
           #:end-local-servers)
  (:export #:start-remote-servers
           #:end-remote-servers)
  (:export #:*remote-lisp*
           #:*ssh-executable*
           #:*ssh-options*
           #:*ssh-user*
           #:*boot-form*)
  (:import-from #:external-program
                #:run)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread))

(in-package #:lfarm-launcher)

;;;; remote servers

(defun boot-form ()
  ;; If this running lisp is using text serialization, assume the
  ;; remote lisp should as well.
  `(progn
     #+lfarm.with-text-serializer
     (pushnew :lfarm.with-text-serializer *features*)
     (unless (find-package :quicklisp-client)
       (let ((namestring (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
         (when (probe-file namestring)
           (load namestring))))
     (funcall (intern (string :quickload) :quicklisp-client) :lfarm-server)))

(defvar *remote-lisp* nil)
(defvar *ssh-executable* "ssh")
(defvar *ssh-options* nil)
(defvar *ssh-user* nil)
(defvar *boot-form* (boot-form))

(defparameter *ssh-base-opt* '("-o" "ServerAliveInterval=10"
                               "-o" "ServerAliveCountMax=1"))

(defun scrub-form (form)
  "Replace objects which cause \" (double-quote) to be printed. Does
not descend into vectors."
  (typecase form
    (cons (cons (scrub-form (car form))
                (scrub-form (cdr form))))
    (string `(string ',(make-symbol form)))
    (character (case form
                 (#\" `(code-char ,(char-code #\")))
                 (otherwise form)))
    (pathname `(parse-namestring ,(scrub-form (namestring form))))
    (otherwise form)))

(defun start-form (host port boot)
  "Code passed to --eval."
  `(progn
     ,boot
     (funcall (intern (string :start-server) :lfarm-server) ,host ,port)
     (ignore-errors (funcall (intern (string :quit) :cl-user)))
     (funcall (intern (string :exit) :cl-user))))

(defun start-string (host port boot)
  "String passed to --eval."
  (with-standard-io-syntax
    (prin1-to-string (scrub-form (start-form host port boot)))))

(defun surround-quotes (string)
  (concatenate 'string "\"" string "\""))

(defun splat (args)
  "(splat '(1 2 (3 4) 5 nil (6))) => (1 2 3 4 5 6)"
  (mapcan (lambda (arg)
            (typecase arg
              (list (copy-list arg))
              (otherwise (list arg))))
          args))

(defun ssh-host (user host)
  (if user
      (concatenate 'string user "@" host)
      host))

(defun start-remote-server (host port lisp boot ssh ssh-opt ssh-user)
  (let* ((ssh-host (ssh-host ssh-user host))
         (start (surround-quotes (start-string host port boot)))
         (ssh-args (splat (list *ssh-base-opt* ssh-opt ssh-host lisp start))))
    (info "start-remote-server" (prin1-to-string (cons ssh ssh-args)))
    (multiple-value-bind (status code) (run ssh ssh-args)
      (unless (zerop code)
        (error "The ssh command ~a with code ~a:~% ~s"
               (string-downcase (string status)) code (cons ssh ssh-args))))))

(defun spawn-remote-server (host port lisp boot ssh ssh-opt ssh-user)
  (make-thread (lambda ()
                 (start-remote-server host port lisp boot ssh ssh-opt ssh-user))
               :name (format nil "lfarm-launcher ssh ~a ~a" host port)))

(defun check-string-list (list)
  (unless (every #'stringp list)
    (error "Not a list of strings: ~s" list)))

(defun check-args (lisp ssh ssh-opt ssh-user)
  ;; Check as much as possible before handing it to ssh.
  (check-string-list lisp)
  (check-type ssh string)
  (check-string-list ssh-opt)
  (check-type ssh-user (or null string)))

(defun start-remote-servers (addresses
                             &key
                             ((:remote-lisp lisp) *remote-lisp*)
                             ((:boot-form boot) *boot-form*)
                             ((:ssh-executable ssh) *ssh-executable*)
                             ((:ssh-options ssh-opt) *ssh-options*)
                             ((:ssh-user ssh-user) *ssh-user*))
  "Launch servers on remote machines via ssh. Assumes non-interactive
login has been set up for each remote machine.

`addresses' -- a list of (host port) string-integer pairs.

`remote-lisp' -- The command to execute lisp on the remote machine (list
of strings). It must end with the implementation's eval switch. E.g.,
'(\"sbcl\" \"--eval\").

`ssh-executable' -- Local ssh executable (string). Default is \"ssh\".

`ssh-options' -- Command-line options passed to ssh (list of strings).

`ssh-user' -- Username on the remote machine (string).

`boot-form' -- Code to load lfarm-server.asd in the remote lisp image
\(and perhaps do other initialization). Default loads it with quicklisp.
Ensure that all symbols are in the CL or CL-USER package."
  (check-args lisp ssh ssh-opt ssh-user)
  (with-each-address (host port addresses)
    (spawn-remote-server host port lisp boot ssh ssh-opt ssh-user)))

(defun end-servers (addresses fn-name)
  (with-each-address/handle-error (host port addresses fn-name)
    (end-server host port)))

(defun end-remote-servers (addresses)
  "Shut down remote servers.

`addresses' is a list of (host port) string-integer pairs."
  (end-servers addresses 'end-remote-servers))

;;;; local servers

(defmacro with-thread-tracker (&body body)
  (with-gensyms (threads)
    `(let ((,threads nil))
       (flet ((track-thread (thread)
                (push thread ,threads)))
         (unwind-protect/safe
          :main (multiple-value-prog1 ,@body)
          :abort (dolist (thread ,threads)
                   (ignore-errors (destroy-thread thread))))))))

(defun start-local-servers (addresses)
  "Locally launch servers.

`addresses' is a list of (host port) string-integer pairs."
  (with-thread-tracker
    (with-each-address (host port addresses)
      (track-thread (start-server host port :background t)))))

(defun end-local-servers (addresses)
  "Shut down local servers.

`addresses' is a list of (host port) string-integer pairs."
  (end-servers addresses 'end-local-servers))
