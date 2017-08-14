
(ql:quickload 'ieee-floats)


(defun read-chunk (stream &optional (count 512))
  "Rpead a chunk of unsigned bytes from a file stream. START can be negative and indicates
an offset from the end of the file"
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

      ;; Handle negative start
      (when (< start 0)
        (setf start (+ (file-length in) start)))

      (file-position in start)

      ;; Read as far as possible, then pad with zeroes
      (loop for i from (read-sequence buffer in)
         below (length buffer)
         do (setf (aref buffer i) 0))
      
      buffer)))


(defparameter example-chunk (read-chunk "Main.class"))



(loop for x across (read-chunk "Main.class")
              if 
              collect
                (code-char x)
              else
   collect x)


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

    (cerror "HERE" "SDS")
    
    (when found
      (push (coerce (reverse found) 'string) result))

    (reverse result)))


(let ((foo ))
  (make-array `(,(length foo)) :initial-contents foo))


(find-strings (loop for c across (read-chunk-from-file "Main.class") collect c))

(mapcar #'first opcodes)

(first opcodes)

(defparameter magic-number #xcafebabe)
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

    
    (let ((x (number-from-bytes buffer count)))
      (when (and nil (< 10000 x))
        (break "BIG NR"))

      x)))


(defun read-signed-int (stream count)
  "Read an signed integer from the stream, consisting of COUNT shifted unsigned bytes
ored together. BIG ENDIAN twos complement"
  (let ((buffer (make-array `(,count)
                            :element-type '(signed-byte 8))))
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
    (ecase tag
      (1 (read-constant-pool-string-entry stream))
      (3 (read-constant-pool-integer-entry stream))
      (4 (read-constant-pool-float-entry stream))
      (5 (read-constant-pool-long-entry stream))
      (6 (read-constant-pool-double-entry stream))
      (7 (read-constant-pool-class-reference stream))
      (8 (read-constant-pool-string-reference stream))
      (9 (read-constant-pool-field-reference stream))
      (10 (read-constant-pool-method-reference stream))
      (12 (read-constant-pool-type-decriptor stream))
      (15 (read-constant-pool-method-handle stream))
      (16 (read-constant-pool-invoke-dynamic stream)))))


(defun read-utf8-info (stream)
  (let* ((tag (read-unsigned-int stream 1)))
    `(:utf8 ,tag ,(read-length-string stream))))


(defun read-attribute (stream)
  ;;(declare (optimize debug))
  (let ((name-index (read-unsigned-int stream 2))
        (length (read-unsigned-int stream 4)))

    `(:attribute ,name-index ,(read-chunk stream length))))


(defun read-field (stream)
  "Read a field_info structure"
;;  (declare (optimize debug))
  (let* ((access-flags (read-unsigned-int stream 2))
         (name-index (read-unsigned-int stream 2))
         (descriptor-index (read-unsigned-int stream 2))
         (attributes-count (read-unsigned-int stream 2))
         ;;(foo (break "HERE"))
         (attributes (loop for i from 1 upto attributes-count collect
                          (read-attribute stream))))

    `(:field ,access-flags ,name-index ,descriptor-index ,attributes)))


(defun read-method (stream)
  "Read a method_info structure"
  (let* ((access-flags (read-unsigned-int stream 2))
         (name-index (read-unsigned-int stream 2))
         (descriptor-index (read-unsigned-int stream 2))
         (attributes-count (read-unsigned-int stream 2))
         (attributes (loop for i from 1 upto attributes-count collect
                          (read-attribute stream))))

    `(:method ,access-flags ,name-index ,descriptor-index ,attributes)))


;; See https://en.wikipedia.org/wiki/Class_(file_format)#File_layout_and_structure
(defun parse-class-stream (stream)
  (declare (optimize debug))
  (let (major
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
    (let ((entries (loop for index from 1 below constant-pool-count collect
                        (read-constant-pool-entry stream))))
      (setf constants
            (make-array `(,(length entries))
                        :initial-contents entries)))
    


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
          (loop for i from 1 upto fields-count collect (read-field stream)))

    (setf methods-count (read-unsigned-int stream 2))
    (setf methods
          (loop for i from 1 upto methods-count collect (read-method stream)))


    (setf attributes-count (read-unsigned-int stream 2))
    (setf attributes
          (loop for i from 1 upto attributes-count collect (read-attribute stream)))
    
    `(,major ,minor ,constants ,interfaces ,fields ,methods ,attributes)
    ))


(defun parse-class-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (parse-class-stream in)))

(parse-class-file "Main.class")
(parse-class-file "Sample.class")


(defvar raw-table
  '((:METHOD-REFERENCE 6 15) (:FIELD-REFERENCE 16 17) (:STRING-REFERENCE 18)
    (:METHOD-REFERENCE 19 20) (:CLASS-REFERENCE 11) (:CLASS-REFERENCE 21)
    (:STRING "<init>") (:STRING "()V") (:STRING "Code")
    (:STRING "LineNumberTable") (:STRING "Main")
    (:STRING "([Ljava/lang/String;)V") (:STRING "SourceFile")
    (:STRING "Main.java") (:TYPE-DESCRIPTOR 7 8) (:CLASS-REFERENCE 22)
    (:TYPE-DESCRIPTOR 23 24) (:STRING "HELLO") (:CLASS-REFERENCE 25)
    (:TYPE-DESCRIPTOR 26 27) (:STRING "java/lang/Object")
    (:STRING "java/lang/System") (:STRING "out")
    (:STRING "Ljava/io/PrintStream;") (:STRING "java/io/PrintStream")
    (:STRING "println") (:STRING "(Ljava/lang/String;)V")))


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
