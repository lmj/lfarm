(in-package #:lfarm-ssl)

(defclass cert-data ()
  ((path     :initarg :path
             :initform nil
             :reader cert-data-path)
   (key      :initarg :key
             :initform nil
             :reader cert-data-key)
   (password :initarg :password
             :initform nil
             :reader cert-data-password)))

(defclass ssl-auth ()
  ((server-cert-data :initarg :server-cert-data
                     :reader ssl-auth-server-cert-data)
   (client-cert-data :initarg :client-cert-data
                     :reader ssl-auth-client-cert-data))
  (:documentation "SSL-based secutiry for lfarm"))

(defmethod lfarm-common.data-transport:initialize-client-stream ((auth ssl-auth) stream server-name)
  (let ((d (ssl-auth-client-cert-data auth)))
    (cl+ssl:make-ssl-client-stream stream
                                   :certificate (cert-data-path d)
                                   :key (cert-data-key d)
                                   :password (cert-data-password d))))

(defmethod lfarm-common.data-transport:initialize-server-stream ((auth ssl-auth) stream)
  (let ((d (ssl-auth-server-cert-data auth)))
    (cl+ssl:make-ssl-server-stream stream
                                   :certificate (cert-data-path d)
                                   :key (cert-data-key d)
                                   :password (cert-data-password d))))
