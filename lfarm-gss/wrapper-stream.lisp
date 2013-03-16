(in-package #:lfarm-gss)

(defclass wrapper-stream (trivial-gray-streams:trivial-gray-stream-mixin
                          trivial-gray-streams:fundamental-binary-input-stream
                          trivial-gray-streams:fundamental-binary-output-stream)
  ((delegate :initarg :delegate
             :initform (error "~s is a required argument for class ~s" :delegate 'wrapper-stream)
             :reader wrapper-stream-delegate)
   (context  :initarg :context
             :initform (error "~s is a required argument for class ~s" :context 'wrapper-stream)
             :reader wrapper-stream-context)
   (description :type string
                :initarg :description
                :initform ""
                :reader wrapper-stream-description)))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream wrapper-stream))
  (read-byte (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-write-byte
    ((stream wrapper-stream) char)
  (write-byte char (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream wrapper-stream) seq start end &key)
  (read-sequence seq (wrapper-stream-delegate stream)
                 :start start :end end))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream wrapper-stream) seq start end &key)
  (write-sequence seq (wrapper-stream-delegate stream)
                  :start start :end end))

(defmethod trivial-gray-streams:stream-finish-output
    ((stream wrapper-stream))
  (finish-output (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-force-output
    ((stream wrapper-stream))
  (force-output (wrapper-stream-delegate stream)))
