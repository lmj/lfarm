(in-package #:lfarm-ssl)

(defclass ssl-auth ()
  ((certificate-path :initform (error "Certificate path needed")
                     :initarg :certificate-path
                     :accessor ssl-auth-certificate-path)
   (password         :initform nil
                     :initarg :password
                     :reader certificate-path-password))
  (:documentation "SSL-based secutiry for lfarm"))

(defmethod lfarm-common.data-transport:initialize-client-stream ((auth ssl-auth) stream server-name)
  )

(defmethod lfarm-common.data-transport:initialize-server-stream ((auth gss-auth) stream)
  )
