
(defun read-cell-content ()
  (let (start content)
    ;; Enter cell
    (nxml-down-element)
    (setq start (point))

    ;; Go to end of cell content
    (nxml-up-element)
    (backward-sexp)

    (setq content (buffer-substring-no-properties start (point)))
    (nxml-up-element)

    content))


(defun parse-row-content ()
  (nxml-down-element)
  (let (cells)
    (dolist (ix '(1 2 3 4 5 6))
      (push (read-cell-content) cells))
    
    (nxml-up-element)
    (reverse cells)))


(defun skip-header-row ()
  ;; Find first header cell
  (search-forward "th")

  ;; Exit th-cell
  (nxml-up-element)

  ;; Exit header row
  (nxml-up-element))


(defun parse-opcodes ()
  "Parse opcode table in an nxml-mode buffer in emacs"

  (skip-header-row)

  (let (current all)
    (while (setq current (ignore-errors (parse-row-content)))
      (unless (string-match-p "(no name)" (car current))
        (push current all)))

    (reverse all)))


(defun compile-opcode-file ()
  (find-file "opcodes2.html")
  (goto-char (point-min))
  (let ((result (parse-opcodes)))
    (find-file "opcodes.lisp")
    (erase-buffer)
    (insert (prin1-to-string `(defparameter opcodes ',result)))))
