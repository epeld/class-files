
(in-package :java-class)

(defclass java-class ()
  ((major :accessor class-major-version
          :initarg :major)
   (minor :accessor class-minor-version
          :initarg :minor)
   (constants :accessor class-constants
              :initarg :constants)
   (access-flags :accessor class-access-flags
                 :initarg :access-flags)
   (interfaces :accessor class-interfaces
               :initarg :interfaces)
   (fields :accessor class-fields
           :initarg :fields)
   (methods :accessor class-methods
            :initarg :methods)
   (attributes :accessor class-attributes
               :initarg :attributes)
   (this-class-index :accessor class-this-index
                  :initarg :this-index)
   (super-class-index :accessor class-super-index
                  :initarg :super-index))
  (:documentation "Represents a java class file"))


(defclass java-method ()
  ((access-flags :accessor method-access-flags
                 :initarg :access-flags)
   (name :accessor method-name
         :initarg :name)
   (descriptor :accessor method-descriptor
               :initarg :descriptor)
   (attributes :accessor method-attributes
               :initarg :attributes))
  (:documentation "Represents a method of a Java class"))

(defvar class-access-flag-alist
  '((:public . #x0001)
    (:final . #x0010)
    (:super . #x0020)
    (:interface . #x0200)
    (:abstract . #x0400)
    (:synthetic . #x1000)
    (:annotation . #x2000)
    (:enum . #x4000))
  "Constants from Java VM Spec document (access flags classes)")


(defvar field-access-flag-alist
  '((:public . #x0001)
    (:private . #x0002)
    (:protected . #x0004)
    (:static . #x0008)
    (:final . #x0010)
    (:volatile . #x0040)
    (:transient . #x0080)
    (:synthetic . #x1000)
    (:enum . #x4000))
  "Constants from Java VM Spec document (access flags for fields)")


(defvar method-access-flag-alist
  '((:public . #x0001)
    (:private . #x0002)
    (:protected . #x0004)
    (:static . #x0008)
    (:final . #x0010)
    (:synchronized . #x0020)
    (:bridge . #x0040)
    (:varargs . #x0080)
    (:native . #x0100)
    (:abstract . #x0400)
    (:strict . #x0800)
    (:synthetic . #x1000)
    )
  "Constants from Java VM Spec document (access flags for methods)")

(defun decode-class-access-flags (flags)
  (loop for pair in class-access-flag-alist
     when (not (zerop (logand flags (cdr pair)))) collect (car pair)))

(defun decode-field-access-flags (flags)
  (loop for pair in field-access-flag-alist
       when (not (zerop (logand flags (cdr pair)))) collect (car pair)))


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
      (constant-string-value (aref table (second constant))
                             table)))


(defun java-super-class-name (class-info)
  (let ((c (aref (class-constants class-info) (class-super-index class-info))))
    (assert (java-class-reference-p c))
    (constant-string-value c (class-constants class-info))))

(defun java-class-name (class-info)
  (let ((c (aref (class-constants class-info) (class-this-index class-info))))
    (assert (java-class-reference-p c))
    (constant-string-value c (class-constants class-info))))


(defun referenced-classes (class-info)
  (let ((constants (class-constants class-info)))
    (remove-duplicates
     (loop for ref across constants
        when (java-class-reference-p ref)
        collect (cadr (aref constants (second ref))))
     :test #'string=)))


(defun constant-types (class)
  (remove-duplicates (loop for x across (class-constants class) collect (first x))))

(defun class-strings (class)
  (loop for c across (class-constants class)
     when (java-string-p c)
     collect (java-string-string c)))


(defun filter-constants (class type)
  (remove-if-not (lambda (x)
                   (eq (first x) type))
                 (class-constants class)))




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
                  (when (and ref-ix
                             (not (eq ref-ix class-ix)))
                    (format out "C_~a -> C_~a~%" class-ix
                            ref-ix))))))
      (format out "~%}~%"))))


;; dot -Granksep=2.0 -Gortho -Tsvg graph.dot -o test.svg -Nmargin="0.3,0.2"
;; (dot-graph (mapcar #'cdr test-classes))