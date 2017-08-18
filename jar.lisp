
(ql:quickload :zip)
(ql:quickload :flexi-streams)

(defun suffixp (suffix string)
  "Return true if STRING is suffixed by SUFFIX"
  (search suffix string :from-end t))

(defparameter
    test-classes
  (read-jar "~/jedit.jar"))


(defun read-jar (file-path)
  (zip:with-zipfile (z file-path)
    (let (classes)
      (zip:do-zipfile-entries (name entry z)
        (when (suffixp ".class" name)
          (let ((stream (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents entry))))
            (push `(,name . ,(parse-class-stream stream)) classes))))
      classes)))



(defun java-class-reference-p (c)
  (eq (first c) :class-reference))


(defun java-string-p (constant)
  "Return if a constant pool entry is a java string"
  (and (consp constant)
       (eq (first constant) :string)))

(defun java-string-string (constant)
  "Return the string value of a java constant pool entry"
  (unless (java-string-p constant)
    (error "Not a string"))
  (second constant))



(defun constant-string-value (constant table)
  (if (java-string-p constant)
      (java-string-string constant)
      (constant-string-value (aref table (1- (second constant)))
                             table)))


(defun java-super-class-name (class-info)
  (let ((c (aref (class-constants class-info) (1- (class-super-index class-info)))))
    (assert (java-class-reference-p c))
    (constant-string-value c (class-constants class-info))))

(defun java-class-name (class-info)
  (let ((c (aref (class-constants class-info) (1- (class-this-index class-info)))))
    (assert (java-class-reference-p c))
    (constant-string-value c (class-constants class-info))))


(defun referenced-classes (class-info)
  (let ((constants (class-constants class-info)))
    (loop for ref across constants
       when (java-class-reference-p ref)
       collect (cadr (aref constants (1- (second ref)))))))


(defun dot-graph (classes &optional (path "graph.dot"))
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (let ((names (mapcar #'java-class-name classes)))
      
      (format out "digraph class_diagram {~% node [shape=box, color=blue]~%rankdir=TB~%")
      
      (loop for class in classes do
           (let* ((name (java-class-name class))
                  (class-ix (search (list name)
                                   names :test #'string=)))
             
           (format out "C_~a [label=\"~a\"]~%" class-ix name)
           (loop for ref in (referenced-classes class) do
                (let ((ref-ix (search (list ref)
                                      names :test #'string=)))
                  (when ref-ix
                    (format out "C_~a -> C_~a~%" class-ix
                            ref-ix))))))
      (format out "~%}~%"))))



(java-class-name (parse-class-file "Sample.class"))
(java-super-class-name (parse-class-file "Sample.class"))

(referenced-classes (parse-class-file "Sample.class"))

;; dot -Granksep=2.0 -Gortho -Tsvg graph.dot -o test.svg -Nmargin="0.3,0.2"
(dot-graph (mapcar #'cdr test-classes))

(loop for c in (mapcar #'cdr test-classes) collect (list (java-class-name c) (referenced-classes c)))

(defvar strange (cdr (nth 6 test-classes)))

(remove-duplicates (loop for x across (class-constants strange) collect (first x)))
(class-this-index strange)


(defun constant-types (class)
  (remove-duplicates (loop for x across (class-constants class) collect (first x))))

(mapcar #'constant-types (mapcar #'cdr test-classes))

(defun class-strings (class)
  (loop for c across (class-constants class)
     when (java-string-p c)
     collect (java-string-string c)))


(defun filter-constants (class type)
  (remove-if-not (lambda (x)
                   (eq (first x) type))
                 (class-constants class)))

(defvar foo
  (loop for (k . c) in test-classes
     when (find :long (constant-types c))
     collect (remove-duplicates (filter-constants c :class-reference)
                                :test #'equal)))


(class-constants (first (loop for (k . c) in test-classes
                           when (find :long (constant-types c))
                           collect c)))



'(#((:CLASS-REFERENCE 158) (:CLASS-REFERENCE 142) (:CLASS-REFERENCE 124)
   (:CLASS-REFERENCE 151) (:CLASS-REFERENCE 161) (:CLASS-REFERENCE 162)
   (:CLASS-REFERENCE 164) (:CLASS-REFERENCE 160))
 #((:CLASS-REFERENCE 156) (:CLASS-REFERENCE 168) (:CLASS-REFERENCE 208)
   (:CLASS-REFERENCE 209) (:CLASS-REFERENCE 196) (:CLASS-REFERENCE 150)
   (:CLASS-REFERENCE 198) (:CLASS-REFERENCE 211) (:CLASS-REFERENCE 212)
   (:CLASS-REFERENCE 213) (:CLASS-REFERENCE 210) (:CLASS-REFERENCE 235))
 #((:CLASS-REFERENCE 135) (:CLASS-REFERENCE 143) (:CLASS-REFERENCE 176)
   (:CLASS-REFERENCE 177) (:CLASS-REFERENCE 178) (:CLASS-REFERENCE 186)
   (:CLASS-REFERENCE 191) (:CLASS-REFERENCE 195)))

(#1="java/lang/StringBuffer" #2="installer/TarHeader"
 #3="java/lang/CloneNotSupportedException" "java/lang/Object" #2#
 #4="java/lang/String" #1# #3# "[B" "installer/InvalidHeaderException"
 "java/lang/System" #4#)
