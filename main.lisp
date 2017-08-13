

(defun read-chunk (path &optional (start 0) (count 512))
  "Read a chunk of unsigned bytes from a file. START can be negative and indicates
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

    (reverse result))))


(let ((foo ))
  (make-array `(,(length foo)) :initial-contents foo))


(find-strings (loop for c across (read-chunk "Main.class") collect c))

(mapcar #'first opcodes)

(first opcodes)

(defparameter magic-number '(#xca #xfe #xba #xbe)
  "Java magic number")

(defun number-from-bytes (buffer count)
  (let ((number 0))
    (loop for i from 0 below count do
         (setf number (logior number (aref buffer i)))
       finally (return number))))


(defun read-unsigned-int (stream count)
  "Read an unsigned integer from the stream, consisting of COUNT shifted unsigned bytes
ored together. BIG ENDIAN"
  (let ((buffer (make-array `(,count)
                            :element-type '(unsigned-byte 8))))
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


(defun read-constant-pool-string-entry (stream)
  "Read an entry from the constant pool entry table"
  (let ((string (read-string stream (read-unsigned-int stream 2))))
    `(:string ,string)))


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
      (7 (read-constant-pool-class-reference stream))
      (8 (read-constant-pool-string-reference stream))
      (9 (read-constant-pool-field-reference stream))
      (10 (read-constant-pool-method-reference stream))
      (12 (read-constant-pool-type-decriptor stream)))))


;; See https://en.wikipedia.org/wiki/Class_(file_format)#File_layout_and_structure
(defun parse-class-stream (stream)
  (let (major
        minor
        constant-pool-count
        constants
        access-flags
        interface-count
        this-class-ix
        super-class-ix
              
        (buffer (make-array '(4048)
                            :initial-element 0
                            :element-type '(unsigned-byte 8))))

    
    ;; Magic number checking 4 bytes
    (read-sequence buffer stream :end (length magic-number))
    (unless (search magic-number buffer
                    :end1 (length magic-number)
                    :end2 (length magic-number))
      (error "Not a class file"))


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

    ;; TODO read interface table!
    
    `(,major ,minor ,constants)
    )

  
  )


(defun parse-class-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (parse-class-stream in)))

(parse-class-file "Main.class")


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
