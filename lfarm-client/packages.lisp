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
           #:invoke-transfer-error))

(defpackage #:lfarm-client.promise
  (:documentation
   "Promises and futures.")
  (:use #:cl
        #:lfarm-common
        #:lfarm-client.kernel)
  (:export #:promise
           #:future
           #:speculate
           #:delay
           #:force
           #:fulfill
           #:fulfilledp
           #:chain))

(defpackage #:lfarm-client.cognate
  (:documentation
   "Promises and futures.")
  (:use #:cl
        #:lfarm-common
        #:lfarm-client.kernel
        #:lfarm-client.promise)
  (:export #:plet))

;;; Avoid polluting CL-USER by choosing names in CL.
(macrolet
    ((package (package-name package-nicknames documentation &rest list)
       `(defpackage ,package-name
          (:documentation ,documentation)
          (:nicknames ,@package-nicknames)
          (:use #:cl ,@list)
          (:export
           ,@(loop
                :for package :in list
                :nconc (loop
                          :for symbol :being :the :external-symbols :in package
                          :collect (make-symbol (string symbol))))))))
  (package #:lfarm-client (#:lfarm)
"This is a convenience package which exports the external symbols of:
   lfarm-client.kernel
   lfarm-client.promise
   lfarm-client.cognate"
    #:lfarm-client.kernel
    #:lfarm-client.promise
    #:lfarm-client.cognate))
