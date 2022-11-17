#|
accessor-custom.lisp
James Watson, 2013 May
Testing definition of custom accessors
|#

#| Opting here not to use the actual accessor, but to create a function that
behaves like an accessor |#
(defgeneric contents (obj)
  (:documentation "return the contents of an object"))

(defclass box ()
  ((contents :initarg :contents)))

(defmethod contents ((obj box)) (slot-value obj 'contents))

(defclass crate (box)
  ((label :initarg :label)
   (representative)))

(defgeneric set-rep (obj rep)
  (:documentation "set the representative of a meta-container"))

(defmethod set-rep ((obj crate) rep)
  (setf (slot-value obj 'representative) rep))

(defmethod contents ((obj crate))
  (with-slots (representative) obj ; Assumes that REPRESENTATIVE has been set
    (slot-value representative 'contents)))

(let* ((a (make-instance 'box :contents "apples"))
       (b (make-instance 'box :contents "oranges"))
       (c (make-instance 'box :contents "mangoes"))
       (d (make-instance 'crate :label "meta-box" :contents (list a b c))))
  (print (contents a)) ; prints "apples"
  (set-rep d a) ; set the "apples" BOX (a) to represent the CRATE for CONTENTS
  (print (slot-value d 'label)) ; prints "meta-box"
  (print (contents d))) ; prints "apples"
  
; 2013-05-18: This was exactly the behavior I was looking for

