
(in-package :java-parse)

(defun read-chunk (stream &optional (count 512))
  "Rpead a chunk of unsigned bytes from a file stream. START can be negative and indicates
an offset from the end of the file"
  (unless (< count 100000)
    (error "Too high count ~a" count))
  
  (let ((buffer (make-array `(,count) :element-type '(unsigned-byte 8))))

    ;; Read as far as possible, then pad with zeroes
    (loop for i from (read-sequence buffer stream)
       below (length buffer)
       do (setf (aref buffer i) 0))
      
    buffer))


(defun read-chunk-from-file (path &optional (count 512))
  "Rpead a chunk of unsigned bytes from a file. START can be negative and indicates
an offset from the end of the file"
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array `(,count) :element-type '(unsigned-byte 8))))

      ;; Read as far as possible, then pad with zeroes
      (loop for i from (read-sequence buffer in)
         below (length buffer)
         do (setf (aref buffer i) 0))
      
      buffer)))


(defun asciip (x)
  (and (>= x 32) (< x 127)))

(defun find-strings (seq &optional (start 0) end)
  (let ((count 0)
        found
        result)
    
    (dolist (c seq)
      
      ;; Exit the loop
      (when (and end (zerop (- end start count)))
        (return))
      (incf count)

      (cond ((< 0 start)
             (decf start))
            
            (found
             (if (asciip c)
                 (push (code-char c) found)
                 (progn (push (coerce (reverse found) 'string) result)
                        (setf found nil))))

            ((asciip c)
             (push (code-char c) found))

            (t
             (push c result))))

    (when found
      (push (coerce (reverse found) 'string) result))

    (reverse result)))


(defparameter magic-number #xcafebabe
  "Java magic number")


(defun number-from-bytes (buffer &optional (count (length buffer)))
  (let ((number 0))
    (loop for i from 0 below count do
         (setf number (logior number (ash (aref buffer i) (* 8 (- count i 1)))))
       finally (return number))))


(defun read-unsigned-int (stream count)
  "Read an unsigned integer from the stream, consisting of COUNT shifted unsigned bytes
ored together. BIG ENDIAN"
  (declare (optimize debug))
  (let ((buffer (make-array `(,count)
                            :element-type '(unsigned-byte 8))))
    (unless (eql count (read-sequence buffer stream :end count))
      (error "Expected to read ~a unsigned bytes" count))

    (number-from-bytes buffer count)))


(defun read-signed-int (stream count)
  "Read an signed integer from the stream, consisting of COUNT shifted unsigned bytes
ored together. BIG ENDIAN twos complement"
  (let ((buffer (make-array `(,count))))
    (unless (eql count (read-sequence buffer stream :end count))
      (error "Expected to read ~a unsigned bytes" count))
    
    (number-from-bytes buffer count)))


(defun read-string (stream length)
  "Read a UTF-8 encoded string consisting of LENGTH bytes"
  ;;(declare (optimize debug))
  (let ((buffer (make-array `(,length))))
    
    (unless (eql length (read-sequence buffer stream :end length))
      (error "Expected to read a string of byte-length ~a" length))

    ;; Convert to character
    (loop for ix from 0 below length do
         (setf (aref buffer ix) (code-char (aref buffer ix))))
    
    (coerce buffer 'string)))

(defun read-length-string (stream)
  "Read a length followed by a utf-8-like string"
  (read-string stream (read-unsigned-int stream 2)))


(defun read-constant-pool-string-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((string (read-string stream (read-unsigned-int stream 2))))
    `(:string ,string)))


(defun read-constant-pool-integer-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((int (read-signed-int stream 4)))
    `(:integer ,int)))

(defun read-constant-pool-long-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((int (read-signed-int stream 8)))
    `(:long ,int)))

(defun read-constant-pool-float-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((uint (read-unsigned-int stream 4)))
    `(:float ,(ieee-floats:decode-float32 uint))))


(defun read-constant-pool-double-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((uint (read-unsigned-int stream 8)))
    `(:double ,(ieee-floats:decode-float64 uint))))


(defun read-constant-pool-method-reference (stream)
  "Read an entry from the constant pool entry table"
  (let* ((class-ix (read-unsigned-int stream 2))
         (name-ix (read-unsigned-int stream 2)))
    `(:method-reference ,class-ix ,name-ix)))

(defun read-constant-pool-interface-method-reference (stream)
  "Read an entry from the constant pool entry table"
  (let* ((class-ix (read-unsigned-int stream 2))
         (name-ix (read-unsigned-int stream 2)))
    `(:interface-method-reference ,class-ix ,name-ix)))


(defun read-constant-pool-field-reference (stream)
  "Read an entry from the constant pool entry table"
  (let* ((class-ix (read-unsigned-int stream 2))
         (name-ix (read-unsigned-int stream 2)))
    `(:field-reference ,class-ix ,name-ix)))


(defun read-constant-pool-string-reference (stream)
  "Read an entry from the constant pool entry table"
  (let* ((ix (read-unsigned-int stream 2)))
    `(:string-reference ,ix)))


(defun read-constant-pool-class-reference (stream)
  "Read an entry from the constant pool entry table"
  (let* ((ix (read-unsigned-int stream 2)))
    `(:class-reference ,ix)))


(defun read-constant-pool-type-decriptor (stream)
  "Read an entry from the constant pool entry table"
  (let* ((name-ix (read-unsigned-int stream 2))
         (type-ix (read-unsigned-int stream 2)))
    `(:type-descriptor ,name-ix ,type-ix)))


(defun read-constant-pool-method-handle (stream)
  "Read an entry from the constant pool entry table"
  (let* ((type-descriptor (read-unsigned-int stream 8))
         (index (read-unsigned-int stream 2)))
    `(:method-handle ,type-descriptor ,index)))


(defun read-constant-pool-method-type (stream)
  "Read an entry from the constant pool entry table"
  (let* ((index (read-unsigned-int stream 2)))
    `(:method-type ,index)))


(defun read-constant-pool-invoke-dynamic (stream)
  "Read an entry from the constant pool entry table"
  (declare (ignore stream))
  (cerror "Use placeholder"
          "Invoke dynamic constant pool entries currently unsupported")
  '(:invoke-dynamic-placeholder))


(defun resolve-constant-references (entry table)
  (loop for item in entry collect
       (if (numberp item)
           (resolve-constant-references (aref table (1- item)) table)
           item)))

(defun compile-constant-table (constants)
  (let ((table (make-array `(,(length constants))
                           :initial-contents constants)))
    (loop for ix from 0 below (length table) do
         (setf (aref table ix)
               (resolve-constant-references (aref table ix)
                                            table)))
    table))


(defun read-constant-pool-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((tag (read-unsigned-int stream 1)))
    (cond ((eql tag 5)
           (list (read-constant-pool-long-entry stream) nil))

          ((eql tag 6)
           (list (read-constant-pool-double-entry stream) nil))

          (t
           (list
            (ecase tag
              (1 (read-constant-pool-string-entry stream))
              (3 (read-constant-pool-integer-entry stream))
              (4 (read-constant-pool-float-entry stream))
              (7 (read-constant-pool-class-reference stream))
              (8 (read-constant-pool-string-reference stream))
              (9 (read-constant-pool-field-reference stream))
              (10 (read-constant-pool-method-reference stream))
              (11 (read-constant-pool-interface-method-reference stream))
              (12 (read-constant-pool-type-decriptor stream))
              (15 (read-constant-pool-method-handle stream))
              (16 (read-constant-pool-invoke-dynamic stream))))))))


(defun read-utf8-info (stream)
  (let* ((tag (read-unsigned-int stream 1)))
    `(:utf8 ,tag ,(read-length-string stream))))


(defun read-attribute (stream constants)
  ;;(declare (optimize debug))
  (let ((name (string-constant constants (read-unsigned-int stream 2)))
        (length (read-unsigned-int stream 4)))

    (cond ((string= name "Code")
           `(:code ,(read-chunk stream length)))

          ((string= name "SourceFile")
           `(:source-file ,(string-constant constants (read-unsigned-int stream 2))))


          ((string= name "Exceptions")
           `(:exceptions ,(loop for i from 0 below (read-unsigned-int stream 2)
                               collect (string-constant constants (read-unsigned-int stream 2)))))
          (t
           `(:attribute ,name ,(read-chunk stream length))))))


(defun read-field (stream constants)
  "Read a field_info structure"
;;  (declare (optimize debug))
  (let* ((access-flags (read-unsigned-int stream 2))
         (name-index (read-unsigned-int stream 2))
         (descriptor-index (read-unsigned-int stream 2))
         (attributes-count (read-unsigned-int stream 2))
         ;;(foo (break "HERE"))
         (attributes (loop for i from 1 upto attributes-count collect
                          (read-attribute stream constants))))

    `(:field ,(decode-field-access-flags access-flags)
             ,(string-constant constants name-index)
             ,(decode-type-descriptor (string-constant constants descriptor-index))
             ,attributes)))


(defun read-method (stream constants)
  "Read a method_info structure"
  (let* ((access-flags (read-unsigned-int stream 2))
         (name-index (read-unsigned-int stream 2))
         (descriptor-index (read-unsigned-int stream 2))
         (attributes-count (read-unsigned-int stream 2))
         (attributes (loop for i from 1 upto attributes-count collect
                          (read-attribute stream constants))))

    `(:method ,access-flags ,name-index ,descriptor-index ,attributes)))


;; See https://en.wikipedia.org/wiki/Class_(file_format)#File_layout_and_structure
(defun parse-class-stream (stream)
  (declare (optimize debug))
  (let* (major
         minor
         constant-pool-count
         constants
         access-flags
         interface-count
         interfaces
         fields-count
         fields
         methods-count
         methods
         attributes-count
         attributes
         this-class-ix
         super-class-ix)

    
    ;; Magic number checking 4 bytes
    (let ((number (read-unsigned-int stream 4)))
      (unless (eql magic-number number)
        (error "Not a class file ~a ~a" magic-number number)))
    


    ;; Minor version 2 bytes
    (setf minor (read-unsigned-int stream 2))
    
    ;; Major version 2 bytes
    (setf major (read-unsigned-int stream 2))


    ;; Constant pool count 2 bytes
    (setf constant-pool-count (read-unsigned-int stream 2))
    (let ((index 1)
          (entries (make-array (list constant-pool-count)
                               :initial-element nil)))

      (loop
         do
           (loop for item in (read-constant-pool-entry stream) do
                (setf (aref entries index) item)
                (incf index))
         until
           (>= index constant-pool-count))

      (setf constants entries))
    


    ;; Access flags
    (setf access-flags (read-unsigned-int stream 2))


    ;; This class
    (setf this-class-ix (read-unsigned-int stream 2))

    ;; Super class
    (setf super-class-ix (read-unsigned-int stream 2))

    ;; Interface count
    (setf interface-count (read-unsigned-int stream 2))
    (setf interfaces
          (loop for i from 1 upto interface-count collect (read-unsigned-int stream 2)))

    (setf fields-count (read-unsigned-int stream 2))
    (setf fields
          (loop for i from 1 upto fields-count collect (read-field stream constants)))

    (setf methods-count (read-unsigned-int stream 2))
    (setf methods
          (loop for i from 1 upto methods-count collect (read-method stream constants)))


    (setf attributes-count (read-unsigned-int stream 2))
    (setf attributes
          (loop for i from 1 upto attributes-count collect (read-attribute stream constants)))

    (make-instance 'java-class
                   :major major
                   :minor minor
                   :constants constants
                   :interfaces interfaces
                   :fields fields
                   :methods methods
                   :attributes attributes
                   :this-index this-class-ix
                   :super-index super-class-ix)))


(defun parse-class-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (parse-class-stream in)))


(defun read-exception-table-entry (stream)
  (let* ((start-pc (read-unsigned-int stream 2))
         (end-pc (read-unsigned-int stream 2))
         (handler-pc (read-unsigned-int stream 2))
         (catch-type (read-unsigned-int stream 2)))
    `(:exception-table-entry ,start-pc ,end-pc ,handler-pc ,catch-type)))


(defun parse-code-attribute (stream constants)
  (let* ((name-index (read-unsigned-int stream 2))
         (attr-length (read-unsigned-int stream 4))
         (max-stack (read-unsigned-int stream 2))
         (max-locals (read-unsigned-int stream 2))
         (code-length (read-unsigned-int stream 4))
         (code (read-chunk stream code-length))
         (exception-table-length (read-unsigned-int stream 2))
         (exception-table (loop for i from 1 upto exception-table-length collect (read-exception-table-entry stream)))
         (attr-count (read-unsigned-int stream 2))
         (attrs (loop for i from 1 upto attr-count collect (read-attribute stream constants))))

    `(:code ,name-index ,attr-length ,max-stack ,max-locals ,code ,exception-table ,attrs)))

