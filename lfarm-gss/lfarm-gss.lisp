(in-package #:lfarm-gss)

(defclass gss-auth ()
  ((allowed-users :type list
                  :initform nil)))

(defgeneric name-accepted (auth name)
  (:method (auth name) t))

(defmethod lfarm-common.data-transport:initialize-client-stream ((auth gss-auth) stream server-name)
  (let ((name (cl-gss:make-name (format nil "lfarm@~a" server-name))))
    (loop
       with need-reply
       with context = nil
       with reply-buffer = nil
       do (multiple-value-bind (continue-reply context-result buffer flags-reply)
              (cl-gss:init-sec name :flags '(:mutual :replay :sequence :integ :conf)
                               :context context :input-token reply-buffer)
            (declare (ignore flags-reply))
            (setq need-reply continue-reply)
            (setq context context-result)
            (write-with-length buffer stream)
            (when need-reply
              (setq reply-buffer (read-with-length stream))))
       while need-reply
       finally (return (make-instance 'wrapper-stream :delegate stream :context context)))))

(defmethod lfarm-common.data-transport:initialize-server-stream ((auth gss-auth) stream)
  (loop
     with need-reply
     with context = nil
     do (let ((reply (read-with-length stream)))
          (multiple-value-bind (continue-reply context-reply name buffer flags-reply)
              (cl-gss:accept-sec reply :context context)
            (declare (ignore flags-reply))
            (unless (name-accepted auth name)
              (error 'auth-error))
            (setq need-reply continue-reply)
            (setq context context-reply)
            (when buffer
              (write-with-length buffer stream))))
     while need-reply
     finally (return (make-instance 'wrapper-stream :delegate stream :context context))))

(defmethod lfarm-common.data-transport:send-buffer ((auth gss-auth) buffer stream)
  (let ((b (cl-gss:wrap (wrapper-stream-context stream) buffer :conf t)))
    (write-with-length b stream)))

(defmethod lfarm-common.data-transport:receive-buffer ((auth gss-auth) stream)
  (let ((buffer (read-with-length stream)))
    (cl-gss:unwrap (wrapper-stream-context stream) buffer)))

(defun read-with-length (stream &key (length 8))
  (let* ((buf (make-array length :element-type '(unsigned-byte 8))))
    (unless (= (read-sequence buf stream) length)
      (error "Stream truncated when reading length"))
    (let ((buf-length (loop
                         with result = 0
                         for i from 0 below length
                         do (setq result (logior result (ash (aref buf i) (* (- length i 1) 8))))
                         finally (return result))))
      (let ((result-seq (make-array buf-length :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence result-seq stream) buf-length)
          (error "Stream truncated when reading buffer"))
        result-seq))))

(defun write-with-length (buffer stream &key (length 8))
  (let ((length-buffer (make-array length :element-type '(unsigned-byte 8))))
    (loop
       for i from 0 below length
       do (setf (aref length-buffer i) (logand #xFF (ash (length buffer) (- (* (- length i 1) 8))))))
    (write-sequence length-buffer stream)
    (write-sequence buffer stream)
    (finish-output stream)))
