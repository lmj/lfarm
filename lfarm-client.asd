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

#+(or sbcl ccl allegro lispworks)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :lfarm.with-closures *features*))

(defsystem :lfarm-client
  :description
  "Client component of lfarm, a library for distributing work across machines."
  :long-description "See http://github.com/lmj/lfarm"
  :version "0.1.0"
  :licence "BSD"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:usocket
               :lparallel
               :lfarm-common
               #+lfarm.with-hu-walker
               :hu.dwim.walker)
  :serial t
  :components ((:module "lfarm-client"
                :serial t
                :components ((:file "packages")
                             (:file "lambda")
#+lfarm.with-closures        (:file "closure")
                             (:file "kernel")
                             (:file "promise")))))

(defmethod perform ((o test-op) (c (eql (find-system :lfarm-client))))
  (declare (ignore o c))
  (load-system '#:lfarm-test)
  (test-system '#:lfarm-test))

(defmethod perform :after ((o load-op) (c (eql (find-system :lfarm-client))))
  (declare (ignore o c))
  (pushnew :lfarm-client *features*))
