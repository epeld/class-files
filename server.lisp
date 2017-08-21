

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

    (setf known-classes (sort known-classes #'string<= :key #'java-class-name))
    
    (- (length known-classes)
       len)))


(hunchentoot:define-easy-handler (home :uri "/home") (jar-file)
  (let ((num-known (length known-classes))
        (num-loaded (if jar-file (load-jar (first jar-file)) 0)))
      
    (cl-who:with-html-output-to-string (s)
      (:h1 "Hello")
      (unless (zerop (+ num-loaded num-known))
        (htm (:div (str (format nil "~a known java classes. " (+ num-loaded num-known)))
                   (:a :href "/classes" "View the Class List!"))))
      (:div )
      (:div "To add classes, please POST a jar file below and you will receive info")
      (:hr)
      (when jar-file
        (htm (:p (str (format nil "Jar file ~s loaded (~a new classes)~%" (second jar-file) num-loaded)))
             (:hr)))
      (:form :name "file-upload"
             :method "post"
             :enctype "multipart/form-data"
             (:label :for "jar-file" "Jar File:")
             (:input :type "file" :name "jar-file" :size "1000000")
             (:input :type "submit" :name "upload" :value "Upload")))))


(hunchentoot:define-easy-handler (class-list :uri "/classes") ()
  (cl-who:with-html-output-to-string (s)
    (:h1 "List of Known Classes")
    (:div "You can add more classes by uploading jar-files on " (:a :href "/home" "the index-page"))
    (:ul
     (loop for c in known-classes do
          (htm (:li (:a :href (str (format nil "/class/~a" (java-class-name c))) (str (java-class-name c)))))))))


(defun render-not-found-page ()
  (with-html-output-to-string (s)
    (:h1 "404 - Not Found")
    (:a :href "/classes" "Back to Class List")))


(defun render-class-page (class)
  (with-html-output-to-string (s)
    
    (:h1 (str "Class Overview - ")
         (str (java-class-name class)))
    (:h3 (str (class-info-string class)))
    (:a :href "/classes" "Back to Class List")
    
    (:hr)
    
    (:div
     (:h2 "Fields")
     (:ul
      (loop for f in (class-fields class) do
           (htm (:li (:b (str (escape-string (field-name f))))
                     (str " - ")
                     (str (escape-string (field-info-string f))))))))
    (:div
     (:h2 "Methods")
     (:ul
      (loop for m in (class-methods class) do
           (htm (:li (:b (str (escape-string (method-name m))))
                     (str " - ")
                     (str (escape-string (method-info-string m))))))))
    (:div
     (:h2 "Referenced Classes")
     (:ul
      (loop for c in (referenced-classes class)
           unless (standard-class-p c)
         do
           (htm (:li (:a :href (str (format nil "/class/~a" c)) (str c)))))))))


(defun class-handler ()
  (when (search "/class/" (request-uri*) :end2 (length "/class/"))
    (let* ((name (subseq (request-uri*) (length "/class/")))
           (class (find name known-classes :key #'java-class-name :test #'string=)))
      (if class
          (render-class-page class)
          (render-not-found-page)))))


(defun my-handler ()
  (class-handler))


(defvar class-dispatcher
  (create-prefix-dispatcher "/class/" #'my-handler))

(pushnew class-dispatcher *dispatch-table*)
