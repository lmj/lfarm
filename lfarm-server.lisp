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

(defpackage #:lfarm-server
  (:documentation
   "A server accepts tasks, executes them, and returns the results.")
  (:use #:cl
        #:lfarm-common)
  (:export #:start-server))

(in-package #:lfarm-server)

(import-now bordeaux-threads:make-thread
            bordeaux-threads:make-lock
            bordeaux-threads:with-lock-held
            bordeaux-threads:current-thread
            bordeaux-threads:destroy-thread
            usocket:socket-stream
            usocket:socket-close
            usocket:with-connected-socket
            usocket:connection-aborted-error)

;;;; util

(defwith ignore-errors/log ()
  (handler-case (call-body)
    (error (err)
      (info "ignoring error" err)
      (values nil err))))

(defun socket-accept** (server)
  (handler-case (socket-accept* server)
    (connection-aborted-error ())))

(defun socket-close* (socket)
  (ignore-errors/log (socket-close socket)))

(defwith with-close-on-abort (socket)
  (unwind-protect/safe
   :main (call-body)
   :abort (socket-close* socket)))

;;; CCL sometimes balks at connection attempts (issue #1050)
#+ccl
(defwith with-bug-handler ()
  (with-tag :retry
    (handler-bind (((or usocket:unknown-error usocket:invalid-argument-error)
                    (lambda (err)
                      (info "socket error bug, retrying" err)
                      (go :retry))))
      (call-body))))

#-ccl
(defwith with-bug-handler ()
  (call-body))

(defmacro dynamic-closure (vars &body body)
  "Capture the values of the dynamic variables in `vars' and return a
closure in which those variables are bound to the captured values."
  (let ((syms (loop :repeat (length vars) :collect (gensym))))
    `(let ,(mapcar #'list syms vars)
       (lambda ()
         (let ,(mapcar #'list vars syms)
           ,@body)))))

;;;; package generator

;;; Allegro doesn't signal PACKAGE-ERROR for an undefined package.
;;; Work around by parsing the error description string.
#+allegro
(progn
  (defun match-delimited-seq (delim seq left-match right-match)
    ;; (match-delimited-seq #\! "hello !want this! world" "hello " " world")
    ;; => "want this"
    (when-let* ((left-match-pos (search left-match seq))
                (left-delim (let ((pos (+ left-match-pos (length left-match))))
                              (and (eql (elt seq pos) delim) pos)))
                (right-delim (position delim seq :start (1+ left-delim)))
                (right-match-pos (1+ right-delim))
                (end-pos (+ right-match-pos (length right-match))))
      (unless (mismatch right-match seq
                        :start2 right-match-pos
                        :end2 (min end-pos (length seq)))
        (subseq seq (1+ left-delim) right-delim))))

  (defun extract-package-name (err)
    (let ((desc (princ-to-string err)))
      (or (match-delimited-seq #\" desc "Package " " not found")
          (match-delimited-seq #\" desc "" " is not a package")))))

(defun make-package* (pkg)
  (info "creating package" pkg)
  (make-package pkg :use nil))

(defwith with-package-generator ()
  (with-tag :retry
    (handler-bind ((package-error
                    (lambda (err)
                      (make-package* (package-error-package err))
                      (go :retry)))
                   #+allegro
                   ((or reader-error type-error)
                    (lambda (err)
                      (when-let (name (extract-package-name err))
                        (make-package* name)
                        (go :retry)))))
      (call-body))))

;;;; task category tracking

;;; Vector of task category ids currently running.
(defvar *tasks*)

;;; Lock for *tasks*.
(defvar *tasks-lock*)

;;; Each task loop thread has an index into the `*tasks*' vector.
(defvar *task-index*)

;;; Value when no job is running.
(defconstant +idle+ 'idle)

(defwith with-task-tracking ()
  (let ((*tasks* (make-array 0 :fill-pointer 0 :adjustable t))
        (*tasks-lock* (make-lock)))
    (call-body)))

(defwith with-tasks-lock ()
  (with-lock-held (*tasks-lock*)
    (call-body)))

(defwith environment-closure ()
  (dynamic-closure (*tasks* *tasks-lock*)
    (call-body)))

(defun acquire-task-index ()
  (with-tasks-lock
    (let ((index (position nil *tasks*)))
      (if index
          (prog1 index
            (setf (aref *tasks* index) +idle+))
          (prog1 (length *tasks*)
            (vector-push-extend +idle+ *tasks*))))))

(defun release-task-index ()
  (setf (aref *tasks* *task-index*) nil))

(defwith with-task-index ()
  (unwind-protect/safe-bind
   :bind (*task-index* (acquire-task-index))
   :main (call-body)
   :cleanup (release-task-index)))

(defwith with-task-category-id (task-category-id)
  (let ((previous (aref *tasks* *task-index*)))
    (assert previous)
    (unwind-protect/safe
     :prepare (setf (aref *tasks* *task-index*)
                    (cons task-category-id (current-thread)))
     :main    (call-body)
     :cleanup (setf (aref *tasks* *task-index*) previous))))

(defun kill-tasks (task-category-id)
  (dosequence (elem (with-tasks-lock (copy-seq *tasks*)))
    (etypecase elem
      (cons (destructuring-bind (id . thread) elem
              (when (eql id task-category-id)
                (info "killing task loop" id thread)
                (ignore-errors/log (destroy-thread thread)))))
      (null)
      (symbol (assert (eq elem +idle+))))))

;;;; task loop

(defun maybe-compile (fn-form)
  (etypecase fn-form
    (symbol fn-form)
    (cons (compile nil fn-form))))

(defun exec-task (task)
  (destructuring-bind (task-category-id fn-form &rest args) task
    (with-task-category-id (task-category-id)
      (apply (maybe-compile fn-form) args))))

(defun deserialize-task (buffer corrupt-handler)
  (with-package-generator
    (handler-bind ((end-of-file corrupt-handler))
      (deserialize-buffer buffer))))

(defun process-task (stream buffer task-handler corrupt-handler)
  (let* ((task (deserialize-task buffer corrupt-handler))
         (result (handler-bind ((error task-handler))
                   (exec-task task))))
    (info "task result" result stream)
    (handler-bind ((error task-handler))
      (serialize result stream))))

(defun read-task-buffer (stream clean-return corrupt-handler)
  (handler-bind ((end-of-file clean-return)
                 (corrupted-stream-error corrupt-handler))
    (read-serialized-buffer stream)))

(defun read-and-process-task (stream clean-return corrupt-handler next-task)
  (let ((buffer (read-task-buffer stream clean-return corrupt-handler)))
    (info "new task" buffer stream)
    (flet ((task-handler (err)
             (info "error during task execution" err stream)
             (serialize (make-task-error-data err) stream)
             (funcall next-task)))
      (process-task stream buffer #'task-handler corrupt-handler))))

(defun task-loop (stream)
  (info "start task loop" stream (current-thread))
  (with-tag :next-task
    (info "reading next task")
    (flet ((clean-return (err)
             (declare (ignore err))
             (info "end task loop" stream)
             (return-from task-loop))
           (corrupt-handler (err)
             (info "corrupted stream" err stream)
             (ignore-errors/log (serialize :corrupt stream))
             (go :next-task))
           (next-task ()
             (go :next-task)))
      (read-and-process-task
       stream #'clean-return #'corrupt-handler #'next-task))
    (go :next-task)))

;;;; responses

(defgeneric respond (message stream))

(defmethod respond ((message (eql :ping)) stream)
  (serialize :pong stream))

(defmethod respond ((message (eql :task-loop)) stream)
  (serialize :in-task-loop stream)
  (with-task-index
    (task-loop stream)))

(defmethod respond ((message (eql :kill-tasks)) stream)
  (kill-tasks (deserialize stream)))

;;;; dispatch

(defun call-respond (message socket)
  (with-errors-logged
    (unwind-protect/safe
     :main (respond message (socket-stream socket))
     :cleanup (socket-close* socket))))

(defun spawn-response (message socket)
  (make-thread (environment-closure
                 (call-respond message socket))
               :name (format nil "lfarm-server response ~a" message)))

(defun dispatch (socket)
  (let ((message (deserialize (socket-stream socket))))
    (info "message" message socket)
    (case message
      (:end-server (socket-close* socket))
      (otherwise (spawn-response message socket)))
    message))

;;;; start-server

(defwith with-server ((:vars server) host port)
  (with-errors-logged
    (with-bug-handler
      (with-connected-socket (server (socket-listen* host port
                                                     :reuse-address t))
        (with-task-tracking
          (call-body server))))))

(defun server-loop (server)
  (loop (when-let (client (socket-accept** server))
          (with-close-on-abort (client)
            (case (dispatch client)
              (:end-server (return)))))))

(defun %start-server (host port)
  (info "server starting" host port)
  (with-server (server host port)
    (server-loop server))
  (info "server ending" host port))

(defun spawn-server (host port name)
  (make-thread (lambda () (%start-server host port))
               :name name))

(defun start-server (host port
                     &key
                     background
                     (name (format nil "lfarm-server ~a:~a" host port)))
  "Start a server instance listening at host:port.

If `background' is true then spawn the server in a separate thread
named `name'."
  (if background
      (spawn-server host port name)
      (%start-server host port)))
