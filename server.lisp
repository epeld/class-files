

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


(defun load-jar (path)
  "Load a new jar file with all contained classes into the list of known classes. Returns the number of new classes added"
  (let ((len (length known-classes)))
    (setf known-classes (union (read-jar path)
                               known-classes
                               :key #'java-class-name
                               :test #'string=))
    (- (length known-classes)
       len)))


(hunchentoot:define-easy-handler (say-yo :uri "/") (jar-file)
  (let ((num-known (length known-classes)))
    (cl-who:with-html-output-to-string (s)
      (:h1 "Hello")
      (unless (zerop num-known)
        (htm (:div (str (format nil "~a known java classes. " num-known))
                   (:a :href "/classes" "View the Class List!"))))
      (:div )
      (:div "To add classes, please POST a jar file below and you will receive info")
      (:hr)
      (when jar-file
        (htm (:p (str (format nil "Jar file ~s loaded (~a new classes)~%" (second jar-file) (load-jar (first jar-file)))))
             (:hr)))
      (:form :name "file-upload"
             :method "post"
             :enctype "multipart/form-data"
             (:label :for "jar-file" "Jar File:")
             (:input :type "file" :name "jar-file" :size "1000000")
             (:input :type "submit" :name "upload" :value "Upload")))))


(hunchentoot:define-easy-handler (say-yo :uri "/classes") ()
  (cl-who:with-html-output-to-string (s)
    (:h1 "List of Known Classes")
    (:div "You can add more classes by uploading jar-files on " (:a :href "/" "the index-page"))
    (:ul
     (loop for c in known-classes do
          (htm (:li (:a :href (str (format nil "/~a" (java-class-name c))) (str (java-class-name c)))))))))



;; TODO write a handler for viewing an individual class!
