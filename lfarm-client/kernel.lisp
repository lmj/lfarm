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

(defpackage #:lfarm-client.kernel
  (:documentation
   "Encompasses the scheduling and execution of remote tasks by
    connecting to a set of servers.")
  (:use #:cl
        #:lfarm-common)
  (:export #:make-kernel
           #:check-kernel
           #:end-kernel
           #:kernel-worker-count
           #:kernel-name)
  (:export #:make-channel
           #:submit-task
           #:submit-timeout
           #:cancel-timeout
           #:receive-result
           #:try-receive-result
           #:do-fast-receives
           #:kill-tasks
           #:task-categories-running)
  (:export #:*kernel*
           #:*kernel-spin-count*
           #:*task-category*
           #:*task-priority*
           #:*debug-tasks-p*)
  (:export #:kernel
           #:channel
           #:no-kernel-error
           #:kernel-creation-error
           #:task-killed-error)
  ;; specific to lfarm
  (:export #:deftask
           #:deftask*
           #:submit-task*
           #:broadcast-task
           #:broadcast-task*
           #:task-execution-error
           #:invalid-task-error)
  ;; present in lparallel.kernel but not available in lfarm
  #+(or)
  (:export #:kernel-bindings
           #:kernel-context
           #:task-handler-bind
           #:transfer-error
           #:invoke-transfer-error)
  (:import-from #:alexandria
                #:simple-style-warning)
  (:import-from #:lparallel.queue
                #:make-queue
                #:push-queue
                #:pop-queue))

(in-package #:lfarm-client.kernel)

;;;; util

(defun random-fixnum ()
  (let ((*random-state* (make-random-state t)))
    (random (1+ most-positive-fixnum))))

(defun interact (&rest prompt)
  "Read from user and eval."
  (apply #'format *query-io* prompt)
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defvar *error-output-lock* (make-lock))

(defun warn* (&rest args)
  (with-lock-held (*error-output-lock*)
    (apply #'warn args)))

;;;; classes

(defclass kernel ()
  ((internal-kernel :reader internal-kernel :initarg :internal-kernel)
   (addresses :reader addresses :initarg :addresses)
   (id :reader id :initform (random-fixnum))))

(defclass channel ()
  ((internal-channel :reader internal-channel :initarg :internal-channel)
   (kernel :reader kernel :initarg :kernel)))

(define-condition task-error (error) ())

(define-condition task-execution-error (task-error)
  ((report :reader task-execution-error-report :initarg :report)
   (desc :reader task-execution-error-desc :initarg :desc))
  (:report (lambda (err stream)
             (format stream
                     "A remote task signaled the following error:~%~%  ~a~%~%~
                      Full error description:~%~%  ~a"
                     (task-execution-error-report err)
                     (task-execution-error-desc err))))
  (:documentation
   "Error signaled when a task failed to execute on a remote server."))

(define-condition invalid-task-error (task-error)
  ((task :reader invalid-task-error-task :initarg :task))
  (:report (lambda (err stream)
             (format stream
                     "Not a symbol or a lambda form: ~s"
                     (invalid-task-error-task err))))
  (:documentation
   "Error signaled when a non-symbol or non-lambda task is submitted"))

(define-condition task-killed-error (task-error)
  ()
  (:report
   "The task was killed.")
  (:documentation
   "Error signaled when attempting to obtain the result of a killed task."))

(define-condition no-kernel-error (error)
  ()
  (:report
"Welcome to lfarm. To get started, you need to connect a set of
servers. Choose the MAKE-KERNEL restart to connect now.

Adding the following line to your startup code will prevent this
message from appearing in the future (replace with your own servers):

  (setf lfarm:*kernel* (lfarm:make-kernel '((\"192.168.14.31\" 11111)
                                            (\"192.168.14.32\" 22222))))

Running a server is done by loading lfarm-server.asd and calling e.g.

  (lfarm-server:start-server \"192.168.14.31\" 11111)
")
  (:documentation
   "Error signaled when `*kernel*' is nil."))

(define-condition kernel-creation-error (error) ()
  (:report
   "Failed to create a kernel.")
  (:documentation
   "Error signaled when `make-kernel' fails."))

;;;; vars

(defvar *kernel* nil
  "The current kernel, or nil.")

(defvar *task-category* :default
  "See `kill-tasks'. Default value is `:default'.")

(defvar *task-priority* :default
  "When bound to `:low', the kernel schedules submitted tasks at low
priority. Default value is `:default'.")

(defvar *kernel-spin-count* :default
  "A do-nothing placeholder for pseudo-compatibility with lparallel.")

(defvar *debug-tasks-p* :default
  "A do-nothing placeholder for pseudo-compatibility with lparallel.")

;;; Thread-local variables inside workers.
(defvar *addresses-queue*)
(defvar *connection*)
(defvar *host*)
(defvar *port*)

;;;; connection

(defun make-connection (host port)
  (info "make-connection" host port)
  (multiple-value-bind (connection stream)
      (socket-connect/retry host port :timeout nil)
    (info "connected" host port connection)
    (send-object :task-loop stream)
    (unless (eq :in-task-loop (receive-object stream))
      (error 'corrupted-stream-error :stream stream))
    (info "handshake complete" host port connection)
    connection))

(defun end-connection (connection)
  (info "end-connection" connection)
  (ignore-errors (socket-close connection)))

;;; Some implementations (ccl, lispworks) don't derive from
;;; stream-error.
#-sbcl
(defun stealth-stream-error-p (err)
  (and (typep err 'simple-error)
       (flet ((match (string)
                (search string (simple-condition-format-control err)
                        :test #'equalp)))
         (or (match "stream")
             (match "closed")))))

(defwith with-reconnect-handler ()
  (with-tag :retry
    (labels ((reconnect (err)
               (info "connection is bad; reconnecting" err)
               (end-connection *connection*)
               (setf *connection* (make-connection *host* *port*))
               (info "reconnected" *host* *port*)
               (go :retry))
             #-sbcl
             (maybe-reconnect (err)
               (when (stealth-stream-error-p err)
                 (reconnect err))))
      (handler-bind (((and stream-error (not reader-error)) #'reconnect)
                     #-sbcl
                     (simple-error #'maybe-reconnect))
        (call-body)))))

;;;; kernel

(defwith with-auth-error-handler ()
  (handler-case (call-body)
    (lfarm-common.data-transport:auth-error (err)
      (bad "auth error:" (princ-to-string err)))))

(defwith with-connection ()
  (let ((*connection* (make-connection *host* *port*)))
    (info "worker connected" *host* *port*)
    (unwind-protect/safe
     :main (call-body)
     :cleanup (progn
                (info "worker ending" *host* *port*)
                (end-connection *connection*)))))

(defun worker-context (worker-loop)
  (with-errors-logged
    (info "new worker")
    (destructuring-bind (*host* *port*) (pop-queue *addresses-queue*)
      (info "worker got address" *host* *port*)
      (with-auth-error-handler
        (with-connection
          (funcall worker-loop))))))

(defun make-bindings (addresses-queue)
  `((*debug-io* . ,*debug-io*)
    (*error-output* . ,*error-output*)
    (*auth* . ,*auth*)
    (*addresses-queue* . ,addresses-queue)))

(defun creation-error-translator (err)
  (declare (ignore err))
  (error 'kernel-creation-error))

(defun make-internal-kernel (&rest args)
  (handler-bind ((lparallel:kernel-creation-error #'creation-error-translator))
    (apply #'lparallel:make-kernel args)))

(defun make-kernel (addresses
                    &key (name "lfarm-client") ((:auth *auth*) *auth*))
  "Connect to a set of existing servers.

`addresses' is a list of (host port) string-integer pairs.

`name' is a string for identifying the connection threads."
  (let* ((addresses (ensure-addresses addresses))
         (addresses-queue (make-queue :initial-contents addresses))
         (internal-kernel (make-internal-kernel
                           (length addresses)
                           :bindings (make-bindings addresses-queue)
                           :context #'worker-context
                           :name name)))
    (make-instance 'kernel
                   :internal-kernel internal-kernel
                   :addresses addresses)))

(defwith with-internal-kernel (kernel)
  (let ((lparallel:*kernel* (internal-kernel kernel)))
    (call-body)))

(defun end-kernel (&key wait)
  "Disconnect this client. If `wait' is true then wait for servers to end."
  (when-let ((kernel *kernel*))
    (with-internal-kernel (kernel)
      (lparallel:end-kernel :wait wait))
    (setf *kernel* nil)))

(defun check-kernel ()
  "Ensures the value of `*kernel*' is a kernel instance. Provides the
MAKE-KERNEL and STORE-VALUE restarts. Returns `*kernel*'."
  (or *kernel*
      (restart-case (error 'no-kernel-error)
        (make-kernel (worker-count)
          :report "Make a kernel now (prompt for list of addresses)."
          :interactive (lambda ()
                         (interact "Enter list of (host port) pairs: "))
          (setf *kernel* (make-kernel worker-count)))
        (store-value (value)
          :report "Assign a value to lfarm:*kernel*."
          :interactive (lambda () (interact "Value for lfarm:*kernel*: "))
          (check-type value kernel)
          (setf *kernel* value)))))

(defun kernel-name ()
  "Return the name passed to `make-kernel'."
  (with-internal-kernel ((check-kernel))
    (lparallel:kernel-name)))

;;;; tasks

(defconstant +make-named-lambda-key+ 'make-named-lambda-key)

(defmacro deftask* (name lambda-list &body body &environment env)
  "Same as `deftask' except the function is not compiled locally."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name +make-named-lambda-key+)
           ;; Wrap this inside a lambda in order to obtain the latest
           ;; lexicals on each submit-task.
           (lambda () ,(serialize-lambda lambda-list body env :name name)))
     ',name))

(defmacro deftask (name lambda-list &body body)
  "Same as `defun' except the function definition is recorded so that
it may be submitted as a remote task."
  `(progn
     (defun ,name ,lambda-list ,@body)
     (deftask* ,name ,lambda-list ,@body)))

(defun maybe-convert-symbol (form)
  (typecase form
    (symbol (let ((make-named-lambda (get form +make-named-lambda-key+)))
              (if make-named-lambda
                  (funcall make-named-lambda)
                  form)))))

(defun ensure-lambda-form (form)
  (if (lambda-form-p form)
      form
      (error 'invalid-task-error :task form)))

(defun maybe-convert-task (form)
  (or (maybe-convert-symbol form)
      (ensure-lambda-form form)))

(defun task-category-id (task-category kernel)
  ;; Susceptible to the birthday problem, but simple and portable.
  (sxhash (cons task-category (id kernel))))

(defun submit-task-to-server (task-category-id task &rest args)
  (info "submitting" (cons task args) *connection*)
  (send-object `(,task-category-id ,task ,@args) (socket-stream *connection*)))

(defun receive-result-from-server ()
  (info "receiving" *connection*)
  (let ((result (receive-object (socket-stream *connection*))))
    (info "received result" result *connection*)
    (when (eq result +corrupt-stream-flag+)
      (error 'corrupted-stream-error :stream (socket-stream *connection*)))
    result))

(defun %make-channel (&key fixed-capacity)
  (let* ((kernel (check-kernel))
         (internal-channel (with-internal-kernel (kernel)
                             (lparallel:make-channel
                              :fixed-capacity fixed-capacity))))
    (make-instance 'channel
                   :kernel kernel
                   :internal-channel internal-channel)))

(defun make-channel (&rest args)
  "Create a channel for submitting and receiving tasks. The current
value of `*kernel*' is stored for use in `submit-task'."
  (apply #'%make-channel (if (= 1 (length args))
                             nil
                             args)))

(define-compiler-macro make-channel (&whole whole &rest args)
  (when (= 1 (length args))
    (simple-style-warning
     "Calling `make-channel' with one argument is deprecated.~%~
      Pass no arguments instead."))
  whole)

(defmacro unwind-protect/kill (&key main kill)
  ;; This doesn't detect other causes of unwinding, but none of those
  ;; are being used here.
  (with-gensyms (abort-caused-by-error-p)
    `(let ((,abort-caused-by-error-p nil))
       (unwind-protect/safe
        :main  (handler-bind ((error (lambda (err)
                                       (declare (ignore err))
                                       (setf ,abort-caused-by-error-p t))))
                 ,main)
        :abort (when (not ,abort-caused-by-error-p)
                 ,kill)))))

(defwith with-kill-monitor ()
  (unwind-protect/kill
   :main (call-body)
   :kill (progn
           (info "worker killed, relinquishing address" *host* *port*)
           (push-queue (list *host* *port*) *addresses-queue*))))

(defun internal-task (task-category-id task args hook)
  (with-kill-monitor
    (with-reconnect-handler
      (when hook
        (funcall hook))
      (apply #'submit-task-to-server task-category-id task args)
      (receive-result-from-server))))

(defun submit-internal-task (channel task &rest args)
  (let ((lparallel:*task-category* *task-category*)
        (lparallel:*task-priority* *task-priority*)
        ;; ensure that no task handlers leak into here
        (lparallel.kernel::*client-handlers* nil))
    (lparallel:task-handler-bind ((error #'lparallel:invoke-transfer-error))
      (apply #'lparallel:submit-task (internal-channel channel) task args))))

(defun submit-task* (channel task &rest args)
  "Function version of `submit-task'. `task' must be a symbol or a
lambda form."
  (submit-internal-task channel
                        #'internal-task
                        (task-category-id *task-category* (kernel channel))
                        (maybe-convert-task task) args nil))

(defwith with-kill-translator ()
  (handler-bind ((lparallel:task-killed-error
                  (lambda (err)
                    (declare (ignore err))
                    (error 'task-killed-error))))
    (call-body)))

(defun internal-try-receive (channel timeout)
  (with-kill-translator
    (lparallel:try-receive-result (internal-channel channel)
                                  :timeout timeout)))

(defun internal-receive (channel)
  (with-kill-translator
    (lparallel:receive-result (internal-channel channel))))

(defun unwrap-result (result)
  (typecase result
    (task-error-data (error 'task-execution-error
                            :report (task-error-data-report result)
                            :desc (task-error-data-desc result)))
    (otherwise result)))

(defun try-receive-result (channel &key timeout)
  "Non-blocking version of `receive-result'.

If a result is available then it is returned as the primary value
in (values result t). Otherwise (values nil nil) is returned."
  (multiple-value-bind (result presentp) (internal-try-receive channel timeout)
    (if presentp
        (values (unwrap-result result) t)
        (values nil nil))))

(defun receive-result (channel)
  "Remove a result from `channel'. If nothing is available the call
will block until a result is received."
  (unwrap-result (internal-receive channel)))

(defun internal-broadcast-task (task-category-id task args
                                from-workers to-workers)
  (internal-task task-category-id task args
                 (lambda ()
                   (push-queue t from-workers)
                   (pop-queue to-workers))))

(defun submit-broadcast-tasks (channel task args worker-count)
  ;; Pause all workers in order to ensure that each gets a task.
  (let ((from-workers (make-queue))
        (to-workers (make-queue))
        (converted-task (maybe-convert-task task)))
    (repeat worker-count
      (submit-internal-task channel
                            #'internal-broadcast-task
                            (task-category-id *task-category* (kernel channel))
                            converted-task args from-workers to-workers))
    (repeat worker-count
      (pop-queue from-workers))
    (repeat worker-count
      (push-queue t to-workers))))

(defun receive-broadcast-results (channel worker-count)
  (let ((results (make-array worker-count)))
    (map-into results (lambda ()
                        (lparallel:receive-result (internal-channel channel))))
    (map nil #'unwrap-result results)
    results))

(defun broadcast-task* (task &rest args)
  "Function version of `broadcast-task'. `task' must be a symbol or a
lambda form."
  (let* ((kernel (check-kernel))
         (task (maybe-convert-task task))
         (channel (let ((*kernel* kernel)) (make-channel)))
         (worker-count (with-internal-kernel (kernel)
                         (lparallel:kernel-worker-count))))
    (submit-broadcast-tasks channel task args worker-count)
    (receive-broadcast-results channel worker-count)))

(defun kernel-worker-count ()
  "Return the number of servers to which in the current kernel is
connected."
  (with-internal-kernel ((check-kernel))
    (lparallel:kernel-worker-count)))

(defun task-categories-running ()
  "Return a vector containing the task category currently running for
each server."
  (with-internal-kernel ((check-kernel))
    (lparallel:task-categories-running)))

(defun send-kill (host port task-category-id)
  (with-connected-stream (stream (socket-connect host port))
    (send-object :kill-tasks stream)
    (send-object task-category-id stream)))

(defun broadcast-kill (addresses task-category-id)
  (with-each-address/handle-error (host port addresses 'kill-jobs)
    (send-kill host port task-category-id)))

#-lparallel.without-kill
(defun kill-tasks (task-category &key dry-run)
  "Every task has an associated task category. When a task is submitted,
it is assigned the category of `*task-category*' which has a default
value of `:default'.

`kill-tasks' interrupts running tasks whose category is `eql' to
`task-category'. Pending tasks are not affected.

If you don't know what to pass for `task-category' then you should
probably pass `:default', though this may kill more tasks than you
wish. Binding `*task-category*' around `submit-task' enables targeted
task killing.

If `dry-run' is nil, the function returns the number of tasks killed.

If `dry-run' is non-nil then no tasks are killed. In this case the
return value is the number of tasks that would have been killed if
`dry-run' were nil.

`kill-tasks' is not available in ABCL."
  (let ((kernel *kernel*))
    (when kernel
      (with-internal-kernel (kernel)
        (if dry-run
            (lparallel:kill-tasks task-category :dry-run t)
            (prog1 (lparallel:kill-tasks task-category)
              (broadcast-kill (addresses kernel)
                              (task-category-id task-category kernel))))))))

(defun submit-timeout (channel timeout-seconds timeout-result)
  "Effectively equivalent to

  (submit-task channel (lambda () (sleep timeout-seconds) timeout-result))

The difference is that `submit-timeout' does not occupy a remote server.

A timeout object is returned, which may be passed to `cancel-timeout'."
  (declare (notinline lparallel:submit-timeout))
  (lparallel:submit-timeout (internal-channel channel)
                            timeout-seconds
                            timeout-result))

#-lparallel.without-kill
(defun cancel-timeout (timeout timeout-result)
  "Attempt to cancel a timeout. If successful, the channel passed to
`submit-timeout' will receive `timeout-result'.

At most one call to `cancel-timeout' will succeed; others will be
ignored. If the timeout has expired on its own then `cancel-timeout'
will have no effect.

`cancel-timeout' is not available in ABCL."
  (declare (notinline lparallel:cancel-timeout))
  (lparallel:cancel-timeout timeout timeout-result))

(defun deprecated-timeout ()
  (simple-style-warning
   "`submit-timeout' and `cancel-timeout' are deprecated; use the new~%~
   `:timeout' option in `try-receive-result'."))

(define-compiler-macro submit-timeout (&whole whole &rest args)
  (declare (ignore args))
  (deprecated-timeout)
  whole)

(define-compiler-macro cancel-timeout (&whole whole &rest args)
  (declare (ignore args))
  (deprecated-timeout)
  whole)

(defmacro do-fast-receives ((result channel count) &body body)
  "Receive `count' number of results from `channel', executing `body'
each time with the result bound to `result'."
  (with-gensyms (channel-var)
    `(let ((,channel-var ,channel))
       (repeat ,count
         (let ((,result (receive-result ,channel-var)))
           ,@body)))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~{~s~^ ~}"
            (with-internal-kernel (kernel)
              (list :worker-count (length (addresses kernel)))))))

;;;; submit-task and broadcast-task macro facade

(defun strip-sharp-quote (form)
  (typecase form
    (cons (case (first form)
            (function `',(second form))))))

(defun maybe-strip-sharp-quote (form)
  (or (strip-sharp-quote form)
      form))

(defun lambda-form-p (form)
  (typecase form
    (cons (case (first form)
            (lambda t)))))

(defun maybe-quote-lambda (form)
  (if (lambda-form-p form)
      `',form
      form))

(defun maybe-serialize-lambda (form env)
  (if (lambda-form-p form)
      (destructuring-bind (lambda-list &body body) (rest form)
        (serialize-lambda lambda-list body env))
      form))

(defun maybe-convert-task-form (form env)
  (maybe-quote-lambda
   (maybe-serialize-lambda
    (maybe-strip-sharp-quote form)
    env)))

(defmacro submit-task (channel task &rest args &environment env)
  "Submit a task and its arguments through `channel'. `task' must be a
lambda form, a function defined by `deftask', or a function that
exists on the servers.

`submit-task' translates the syntax of `task' as follows:

    #'f -> 'f
    (lambda ..) -> '(lambda ...)

with the result being passed to `submit-task*'. This provides the
semblance of function syntax when referring to remotely executed
code."
  `(submit-task* ,channel ,(maybe-convert-task-form task env) ,@args))

(defmacro broadcast-task (task &rest args &environment env)
  "Same as `submit-task' except the task is executed on every server.
A possible use for this might be to load common code onto servers."
  `(broadcast-task* ,(maybe-convert-task-form task env) ,@args))
