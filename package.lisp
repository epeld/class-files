
(ql:quickload :zip)
(ql:quickload :flexi-streams)
(ql:quickload 'ieee-floats)

(defpackage :java
  (:nicknames :java-class :java-parse :java-jar)
  (:use :cl :ieee-floats :flexi-streams :zip))

#|
(defpackage :java-class
  (:use :cl)
  (:export :java-class
           :java-method))


(defpackage :java-parse
  (:use :cl :java-class)
  (:export :parse-class-stream
           :parse-class-file))


(defpackage :java-jar
  (:use :cl :java-parse :java-class)
  (:export :read-jar))
|#
