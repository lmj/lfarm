(in-package #:lfarm-gss)

(declaim (optimize (speed 0) (safety 0) (debug 3)))

(defclass wrapper-stream (trivial-gray-streams:fundamental-binary-input-stream
                          trivial-gray-streams:fundamental-binary-output-stream)
  ((delegate :initarg :delegate
             :initform (error "~s is a required argument for class ~s" :delegate 'wrapper-stream)
             :reader wrapper-stream-delegate)
   (context  :initarg :context
             :initform (error "~s is a required argument for class ~s" :context 'wrapper-stream)
             :reader wrapper-stream-context)))

 (defmethod trivial-gray-streams:stream-read-sequence ((stream wrapper-stream) seq start end &rest rest)
  (apply #'trivial-gray-streams:stream-read-sequence (wrapper-stream-delegate stream) seq start rest))

(defmethod trivial-gray-streams:stream-write-sequence ((stream wrapper-stream) seq start end &rest rest)
  (apply #'trivial-gray-streams:stream-write-sequence (wrapper-stream-delegate stream) seq start rest))

(defmethod trivial-gray-streams:stream-file-position ((stream wrapper-stream))
  (trivial-gray-streams:stream-file-position (wrapper-stream-delegate stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (newval (stream wrapper-stream))
  (setf (trivial-gray-streams:stream-file-position (wrapper-stream-delegate stream)) newval))

(defmethod trivial-gray-streams:stream-write-string ((stream wrapper-stream) seq &optional start end)
  (trivial-gray-streams:stream-write-string (wrapper-stream-delegate stream) seq start end))

(defmethod trivial-gray-streams:stream-terpri ((stream wrapper-stream))
  (trivial-gray-streams:stream-terpri (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-write-byte ((stream wrapper-stream) char)
  (trivial-gray-streams:stream-write-byte (wrapper-stream-delegate stream) char))

(defmethod trivial-gray-streams:stream-write-char ((stream wrapper-stream) char)
  (trivial-gray-streams:stream-write-char (wrapper-stream-delegate stream) char))

(defmethod trivial-gray-streams:stream-line-column ((stream wrapper-stream))
  (trivial-gray-streams:stream-line-column (wrapper-stream-delegate stream)))

#+sbcl (defmethod sb-gray:stream-line-length ((stream wrapper-stream))
         (sb-gray:stream-line-length (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-clear-input ((stream wrapper-stream))
  (trivial-gray-streams:stream-clear-input (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream wrapper-stream))
  (trivial-gray-streams:stream-clear-output (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream wrapper-stream))
  (trivial-gray-streams:stream-finish-output (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream wrapper-stream))
  (trivial-gray-streams:stream-force-output (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream wrapper-stream))
  (trivial-gray-streams:stream-fresh-line (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-listen ((stream wrapper-stream))
  (trivial-gray-streams:stream-listen (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-peek-char ((stream wrapper-stream))
  (trivial-gray-streams:stream-peek-char (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-byte ((stream wrapper-stream))
  (trivial-gray-streams:stream-read-byte (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream wrapper-stream))
  (trivial-gray-streams:stream-read-char (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream wrapper-stream))
  (trivial-gray-streams:stream-read-char-no-hang (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-line ((stream wrapper-stream))
  (trivial-gray-streams:stream-read-line (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream wrapper-stream))
  (trivial-gray-streams:stream-start-line-p (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-unread-char ((stream wrapper-stream) char)
  (trivial-gray-streams:stream-unread-char (wrapper-stream-delegate stream) char))

