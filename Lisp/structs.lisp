#|
structs.lisp
James Watson, 2013 July
Structs in Common Lisp
|#

(defstruct nums
  (one 1)
  (two 2)
  (three 3)
  (four 4))

(defvar the-struct (make-nums))

(defun struct-test ()
  (nums-one the-struct) ; Accessor functions structname-slotname created
  (nums-two the-struct)
  (nums-three the-struct)
  (nums-four the-struct))

(defvar the-alist 
  (pairlis (list 'p-one 'p-two 'p-three 'p-four) (list 1 2 3 4)))

(defun alist-test ()
  (assoc 'p-one the-alist)
  (assoc 'p-two the-alist)
  (assoc 'p-three the-alist)
  (assoc 'p-four the-alist))

(defvar test-times (expt 10 8))

(time (dotimes (n test-times) struct-test)) ; 1,505,318,319 processor cycles
(time (dotimes (n test-times) alist-test))  ; 1,505,624,571 processor cycles
; For a small number of slots, 
;the struct is only marginally faster according to TIME