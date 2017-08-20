

(in-package :server)


(defvar server-instance nil
  "The global server instance, when running")

(defun start-server (&optional force)
  "Start the server.."
  (when (and server-instance
             (not force))
    (cerror "Continue anyway" "Server already running. Really start again?!"))
  
  (setf server-instance (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (start server-instance)
  :ok)


(defun stop-server ()
  "Stop the server"
  (hunchentoot:stop server-instance)
  (setf server-instance nil)
  :ok)


(hunchentoot:define-easy-handler (say-yo :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hello, World!"))
