
(in-package :java-jar)

(defun suffixp (suffix string)
  "Return true if STRING is suffixed by SUFFIX"
  (search suffix string :from-end t))


(defun read-jar (file-path)
  (zip:with-zipfile (z file-path)
    (let (classes)
      (zip:do-zipfile-entries (name entry z)
        (when (suffixp ".class" name)
          (let ((stream (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents entry))))
            (push (parse-class-stream stream) classes))))
      classes)))



(defparameter
    test-classes
  (read-jar "~/jedit.jar"))
