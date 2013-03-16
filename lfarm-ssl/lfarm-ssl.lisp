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
             :reader cert-data-password))
  (:documentation "Certificate information for SSL auth models"))

(defclass ssl-auth-server (cert-data)
  ()
  (:documentation "Server ssl auth model"))

(defclass ssl-auth-client (cert-data)
  ()
  (:documentation "Client ssl auth model"))

(defmethod lfarm-common.data-transport:initialize-client-stream ((auth ssl-auth-client) stream server-name)
  (cl+ssl:make-ssl-client-stream stream
                                 :certificate (cert-data-path auth)
                                 :key (cert-data-key auth)
                                 :password (cert-data-password auth)))

(defmethod lfarm-common.data-transport:initialize-server-stream ((auth ssl-auth-server) stream)
  (cl+ssl:make-ssl-server-stream stream
                                 :certificate (cert-data-path auth)
                                 :key (cert-data-key auth)
                                 :password (cert-data-password auth)))
