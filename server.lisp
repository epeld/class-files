

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


(defvar known-classes nil
  "List of known classes")

(hunchentoot:define-easy-handler (say-yo :uri "/") (jar-file)
  (when jar-file
    (setf known-classes (read-jar (first jar-file))))
  
  (format t "Jar file: ~a ~a~%" jar-file (type-of jar-file))
  (cl-who:with-html-output-to-string (s)
    (:h1 "Hello")
    (:div "Jar file is" (str jar-file))
    (:div "Please POST a jar file below and you will receive info")
    (:form :name "file-upload"
           :method "post"
           :enctype "multipart/form-data"
           (:label :for "jar-file" "Jar File:")
           (:input :type "file" :name "jar-file" :size "1000000")
           (:input :type "submit" :name "upload" :value "Upload"))))
