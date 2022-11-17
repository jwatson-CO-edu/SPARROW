#|
accessor-inherit.lisp
James Watson, 2013 May
Testing if accessors are inherited by subclasses
|#

(defclass animal ()
  ((legs :initform 4 :accessor legs :initarg :legs))) ;Added :initarg see spider

(defclass bird (animal)
  ((legs :initform 2)))

(defparameter parrot (make-instance 'bird))

(print (legs parrot)) ; prints "2", accesor was inherited!

; Define a new class to inherit ANIMAL, but also new accessor for LEGS
(defclass arthropod (animal)
  ((legs :accessor appendages)))

(defparameter spider (make-instance 'arthropod :legs 8))

(print (legs spider)) ; prints "8", accessor LEGS available to sub-classes
(print (appendages spider)) ; prints "8"
;(print (appendages parrot)) ; ERROR!, this accessor not available to superclass