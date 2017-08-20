
(in-package :java-jar)

(defun suffixp (suffix string)
  "Return true if STRING is suffixed by SUFFIX"
  (search suffix string :from-end t))


(defun system-class-p (string)
  "Predicate for filtering out 'system'-like classes, e.g junit etc so that only user defined classes remain"
  (or (search "org/junit" string)
      (search "org/hamcrest" string)
      (search "junit/" string)))

(defun read-jar (file-path &optional (user t))
  (zip:with-zipfile (z file-path)
    (let (classes)
      (zip:do-zipfile-entries (name entry z)
        (when (suffixp ".class" name)
          (let ((stream (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents entry))))
            (push (parse-class-stream stream) classes))))

      ;; Filter if necessary
      (if user
          (let ((user-classes (remove-if #'system-class-p classes :key #'java-class-name)))
            (values user-classes (- (length classes) (length user-classes))))
          (values classes 0)))))



(defparameter
    test-classes
  (read-jar "~/jedit.jar"))
