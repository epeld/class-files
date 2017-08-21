
(ql:quickload :zip)
(ql:quickload :flexi-streams)
(ql:quickload 'ieee-floats)

(defpackage :java
  (:nicknames :java-class :java-parse :java-jar)
  (:use :cl :ieee-floats :flexi-streams :zip)
  (:export :read-jar
           :java-class-name
           :referenced-classes
           :system-class-p
           :standard-class-p

           :class-info-string
           :class-fields
           :class-methods

           :field-info-string
           :field-access-flags
           :field-name
           :field-type-descriptor

           :method-parsed-machine-code
           :method-info-string
           :method-access-flags
           :method-name
           :method-type-descriptor))


(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(defpackage :server
  (:use :cl :hunchentoot :cl-who :java))

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
