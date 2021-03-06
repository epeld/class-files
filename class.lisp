
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
   (descriptor :accessor method-type-descriptor
               :initarg :descriptor)
   (attributes :accessor method-attributes
               :initarg :attributes))
  (:documentation "Represents a method of a Java class"))


(defclass java-field ()
  ((access-flags :accessor field-access-flags
                 :initarg :access-flags)
   (name :accessor field-name
         :initarg :name)
   (descriptor :accessor field-type-descriptor
               :initarg :descriptor)
   (attributes :accessor field-attributes
               :initarg :attributes))
  (:documentation "Represents a field in a Java class"))


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

(defun decode-access-flags (flags flag-alist)
  (loop for pair in flag-alist
     when (not (zerop (logand flags (cdr pair)))) collect (car pair)))

(defun decode-class-access-flags (flags)
  (decode-access-flags flags class-access-flag-alist))

(defun decode-field-access-flags (flags)
  (decode-access-flags flags field-access-flag-alist))

(defun decode-method-access-flags (flags)
  (decode-access-flags flags method-access-flag-alist))


(defun class-interface-names (class)
  (declare (optimize debug))
  (the java-class class)
  (loop for i in (class-interfaces class) collect (string-constant (class-constants class) i)))


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


(defun java-super-class-name (class-info &optional (objectp t))
  (let ((c (aref (class-constants class-info) (class-super-index class-info))))
    (assert (java-class-reference-p c))
    (let ((s (constant-string-value c (class-constants class-info))))
      (when (or objectp (not (string= "java/lang/Object" s)))
        s))))

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


(defun string-constant (constants index)
  (assert (the array constants))
  (let  ((cr (aref constants index)))
    (cond ((and (consp cr)
                (eq (first cr) :string))
           (second cr))

          ((consp cr)
           (string-constant constants (second cr)))


          (t
           cr))))


(defun class-string-constant (class index)
  (string-constant (class-constants class) index))


(defun class-label (class)
  "Produce a short label describing the class"
  (let ((interfaces (class-interfaces class)))
    (format nil "~a~@[ extends ~a~@[ implements ~{~a~^, ~}~]~]"
            (java-class-name class)
            (java-super-class-name class nil)
            (loop for i in interfaces
               collect (class-string-constant class i)))))


(defun standard-class-p (string)
  "Very simple heuristic predicate for determining if a class is a builtin java class"
  (or (search "java/" string)
      (search "javax/" string)))


(defun array-class-p (string)
  "Some of the class references will be to arrays. This predicate finds them"
  (string= "[" string :end2 1))


(defun dot-graph (classes &optional (path "graph.dot") (ignore-missing nil))
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (let ((names (mapcar #'java-class-name classes))

          ;; TODO use
          (refs (quote (mapcar #'referenced-classes classes))))

      ;; TODO:
      (quote
       (loop for g in (subgraphs (loop for i from 0 below (length names))
                                 (delabel-edges names refs))
          do
            (format out "digraph {~% node [shape=box, color=blue]~%rankdir=TB~%")
            ()
            (format out "~%}~%")))
      
      (format out "digraph class_diagram {~% node [shape=box, color=blue]~%rankdir=TB~%")
      
      (loop for class in classes do
           (let* ((name (java-class-name class))
                  (class-ix (search (list name)
                                   names :test #'string=)))
             
           (format out "C_~a [label=\"~a\"]~%" class-ix name)
           (loop for ref in (referenced-classes class) do
                (let ((ref-ix (search (list ref)
                                      names :test #'string=)))
                  (unless (or ignore-missing
                              ref-ix
                              (standard-class-p ref)
                              (array-class-p ref))
                    (cerror "Ignore" "Class ~a missing" ref))
                  
                  (when (and ref-ix
                             (not (eq ref-ix class-ix)))
                    (format out "C_~a -> C_~a~%" class-ix
                            ref-ix))))))
      (format out "~%}~%"))))


(defun delabel-edges (nodes edges)
  "Go from a graph of string nodes to a graph of integer nodes"
  (loop for node-edges in edges collect
       (loop for neighbor in node-edges
          when
            (search (list neighbor) nodes :test #'string=)
          collect
            (search (list neighbor) nodes :test #'string=))))


(defun visited-nodes (start-node edges &optional (visited nil))
  "Compute all the nodes that can be visited from start-node by following edges"
  (remove-duplicates (cons start-node
                           (loop for neighbor in (nth start-node edges)
                              when (not (find neighbor visited :test #'equal))
                              nconc (visited-nodes neighbor edges (list start-node))))
   :test #'equal))


(defun subgraphs (nodes edges)
  "Convert a graph represented as a list of nodes and a list of edges
into a list of node subsets"
  (let ((queue nodes)
        gs)

    (loop do
         (let ((subgraph (visited-nodes (pop queue) edges)))
           (push subgraph gs)
           (setf queue (set-difference queue subgraph :test #'equal)))
       until
         (endp queue))

    gs))
    

(defun svg-dot-graph (classes &optional (path "graph.svg"))
  "Helper that produces an svg image from a list of classes"
  (let ((dot-name "temp.dot"))
    (dot-graph classes dot-name)
    (unless (zerop (sb-ext:process-exit-code
                    (sb-ext:run-program "dot"
                                        `("-Granksep=2.0" "-Nfontsize=25" "-Tsvg" "-o" ,path "-Nmargin=0.3,0.2" ,dot-name)
                                        :search t)))
      (error "Failed to generate graph"))))

;; (svg-dot-graph test-classes)

(defun strings (class indexes)
  (loop for ix in indexes collect (class-string-constant class ix)))

#|
(let ((c (first test-classes)))
  (loop for m in (class-methods c) collect
       (strings c (mapcar #'second (fifth m)))))
|#

(defun decode-primitive-type (c)
  (ecase c
    (#\Z
     :boolean)

    (#\B
     :byte)

    (#\I
     :integer)  

    (#\S
     :short)

    (#\C
     :char)

    (#\J
     :long)

    (#\F
     :float)

    (#\D
     :double)

    (#\V
     :void)))

(defun decode-type-descriptor (desc)
  "Tries to decode java type signature strings"
  (let ((c (aref desc 0)))
    (cond
      ;; Array
      ((eq c #\[)
       (multiple-value-bind (type incr) (decode-type-descriptor (subseq desc 1))
         (values `(:array-of ,type)
                 (1+ incr))))

      ;; Method
      ((eq c #\()
       (let ((i 1)
             args r)
         ;; Args
         (loop until (eq #\) (aref desc i)) do
              (multiple-value-bind (type incr) (decode-type-descriptor (subseq desc i))
                (incf i incr)
                (push type args)))

         ;; Skip the closing #\)
         (incf i)
         
         ;; Return value
         (multiple-value-bind (type incr) (decode-type-descriptor (subseq desc i))
           (setf r type)
           (incf i incr))

         (values `(:method-signature ,(reverse args) ,r) i)))

      ;; Fully qualified class
      ((eq c #\L)
       (let ((name (loop for i from 1 until (eq #\; (aref desc i))
                      finally (return (subseq desc 1 i)))))

         ;; We add #\L and #\; to the length of the string
         (values `(:class-instance ,name) (+ 2 (length name)))))

      ;; Primitives
      (t
       (values (decode-primitive-type c) 1)))))


(defun human-readable-type-string (type)
  (cond ((keywordp type)
         (string-downcase type))

        ((and (consp type)
              (eq (first type) :class-instance))
         (second type))

        (t (format nil "~a" type))))


(defun class-info-string (class)
  (declare (optimize debug))
  (format nil "~(~{~a~^ ~}~) ~a ~@[extends ~a~]~@[ implements ~{~a~^, ~}~]"
          (class-access-flags class)
          (java-class-name class)
          (java-super-class-name class)
          (class-interface-names class)))


(defun method-info-string (method)
  (let* ((td (method-type-descriptor method))
         (args (second (method-type-descriptor method))))
    (assert (eq (first td) :method-signature))

    (format nil "~(~{~a~^ ~}~) ~a ~a(~{~a~^, ~})"
            (method-access-flags method)
            (human-readable-type-string (car (last td)))
            (method-name method)
            (mapcar #'human-readable-type-string args))))


(defun field-info-string (field)
  (let* ((td (field-type-descriptor field)))
    (format nil "~(~{~a~^ ~}~) ~a ~a"
            (field-access-flags field)
            (human-readable-type-string td)
            (field-name field))))



(defun method-parsed-machine-code (method)
  (let ((code (find-if (lambda (attr)
                         (eq (first attr) :code))
                       (method-attributes method))))
    (parse-instructions (sixth code))))

