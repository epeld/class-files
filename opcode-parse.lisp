
(in-package :java-parse)

(loop for op in raw-opcodes when (search "invoke" (first op)) collect op)

(defvar raw-opcodes nil
  "Cached list of opcodes")

(defun read-raw-opcodes ()
  (with-open-file (in "opcodes.lisp"
                      :direction :input)
    (read in)))

(setf raw-opcodes (read-raw-opcodes))

;;
;; Opcode Parsing
;;
(defun parse-opcodes ()
  (let ((codes (read-raw-opcodes)))
    (loop for opcode in codes collect
         `(,(parse-integer (second opcode) :radix 16)
            .
            ,(lispify-opcode-mnemonic (first opcode))))))

(defparameter opcode-alist (parse-opcodes))

(defun opcode-string (opcode)
  "Translate an opcode number to its mnemonic string value"
  (let ((s (cdr (assoc opcode opcode-alist))))
    (unless s
      (cerror "Ignore" "Unknown opcode ~a" opcode))

    s))

(defun string-opcode (string)
  "Lookup an opcode's byte value, given its string mnemonic"
  (let ((op (rassoc string opcode-alist :test #'string=)))
    (unless op
      (cerror "Ignore" "Unknown opcode mnemonic ~a" string))
    op))


(defun parse-operand-string (string)
  (unless (zerop (length string))
    (let ((nr (subseq string 0 (search '(#\:) string))))
      (if (or (find #\+ nr)
              (find #\/ nr))
          (cerror "OK" "Unhandled ~a" nr)
          (parse-integer nr)))))


(defun operand-string-special-case-p (string)
  (or (find #\+ string)
      (find #\/ string)))


(defparameter operand-special-cases
  (loop for op in raw-opcodes
     when (operand-string-special-case-p (fourth op))
     collect (parse-integer (second op) :radix 16))
  "All opcodes that require special treatment when counting their operands")


(quote (loop for op in raw-opcodes
          unless (or (zerop (length (fourth op)))
                     (operand-string-special-case-p (fourth op)))
          collect (fourth op)))

(defparameter operand-opcodes-alist
  (loop for op in raw-opcodes
     unless (or (zerop (length (fourth op)))
                (operand-string-special-case-p (fourth op)))
     collect `(,(parse-integer (second op) :radix 16) . ,(parse-operand-string (fourth op))))
  "All opcodes that take operands (minus special cases) in an alist with their respective operand count")


(defun opcode-operand-count (opcode)
  "Return how many operands an opcode requires"
  (when (member opcode operand-special-cases)
    (error "Operand special case ~s requires context" (opcode-string opcode)))
  
  (or (cdr (assoc opcode operand-opcodes-alist))
      0))

(defun parse-instruction (stream)
  "Parse an opcode along with its operands (if any)"
  (let* ((opcode (read-unsigned-int stream 1))
         (operands (opcode-operand-count opcode)))

    (if (find opcode operand-special-cases)
        (error "Special case operands not supported yet")
        `(,(opcode-string opcode) ,@(loop for i from 1 upto operands collect
                                       (read-unsigned-int stream 1))))))


(defparameter code-example #(42 43 3 43 190 182 0 16 172))

;; (parse-instructions code-example)

(defun parse-instructions (machine-code)
  "Given a sequence of machine codes, parse it into 'human readable' form"
  (flexi-streams:with-input-from-sequence (in machine-code)
    (let (codes)
      (loop do
           (push (ignore-errors (parse-instruction in))
                 codes)
         until
           (null (car codes)))
      (reverse (cdr codes)))))


(defun lispify-opcode-mnemonic (string)
  "Convert an opcode mnemonic into a keyword"
  (loop for i from 0 below (length string) do
       (setf (aref string i)
             (if (eq (aref string i) #\_)
                 #\-
                 (aref string i))))
  (intern (string-upcase string)))


(defparameter invoking-opcodes
  (loop for op in raw-opcodes when (search "invoke" (first op)) collect (lispify-opcode-mnemonic (first op))))

(defparameter storing-opcodes
  (loop for op in raw-opcodes when (search "store" (first op)) collect (lispify-opcode-mnemonic (first op))))

(defparameter loading-opcodes
  (loop for op in raw-opcodes when (search "load" (first op)) collect (lispify-opcode-mnemonic (first op))))

