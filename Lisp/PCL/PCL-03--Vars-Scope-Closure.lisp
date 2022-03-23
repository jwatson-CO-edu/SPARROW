#|
PCL-03--Vars-Scope-Closure.lisp
James Watson, 2011 December
General comments on Variables, assignment, scope, and closures
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

; == Variables ==
#| Common Lisp supports two kinds of variables: lexical and dynamic. Common Lisp
variables are named places that can hold a value. A variable can hold values of 
any type and the values carry type information that can be used to check types 
at runtime. Thus, Common Lisp is dynamically typed--type errors are detected 
dynamically. Common Lisp is also a strongly typed language in the sense that all
type errors will be detected--there's no way to treat an object as an instance 
of a class that it's not. 

All values in Common Lisp are, conceptually at least, references to objects. 
Consequently, assigning a variable a new value changes what object the variable 
refers to but has no effect on the previously referenced object. However, if a 
variable holds a reference to a mutable object, you can use that reference to 
modify the object, and the modification will be visible to any code that has a 
reference to the same object.

Each time a function is called, Lisp creates new bindings to hold the arguments 
passed by the function's caller. A binding is the runtime manifestation of a 
variable. A single variable--the thing you can point to in the program's source 
code--can have many different bindings during a run of the program. A single 
variable can even have multiple bindings at the same time; parameters to a 
recursive function, for example, are rebound for each call to the function. If 
the object passed to a function is mutable and you change it in the function, 
the changes will be visible to the caller since both the caller and the callee 
will be referencing the same object. |#

#| Common Lisp provides two ways to create global variables: DEFVAR and 
DEFPARAMETER. Both forms take a variable name, an initial value, and an optional
documentation string. After it has been DEFVARed or DEFPARAMETERed, the name can
be used anywhere to refer to the current binding of the global variable. |#

; = defvar =
(defvar a 3) ; Create variable 'a', bind number 3 to it
a #| --> |# 3

#| The difference between the two forms is that DEFPARAMETER always assigns the 
initial value to the named variable while DEFVAR does so only if the variable is
undefined. A DEFVAR form can also be used with no initial value to define a 
global variable without giving it a value. Such a variable is said to be 
unbound.|#

; = Assignment =
; = setq =
; A special operator
(setq a 5) ; bind the value 5 to the previously created variable 'a'
a #| --> |# 5

; = setf =
#| A macro, will expand the 'place' form, then perform the assignment with 
'setq'|#
(setf place value) ; assign a new value to a binding
(setf x 1 y 2) ; SETF can also assign to multiple places in sequence.
; SETF returns the newly assigned value, so you can also nest calls to SETF
(setf x (setf y (random 10))) ; assigns both x and y the same random value
#| Simple variable: |#    (setf x 10) 
#| Array: |#              (setf (aref a 0) 10) ; AREF - array access function
#| Hash table: |#         (setf (gethash 'key hash) 10) 
                               ; GETHASH -  hash table lookup
#| Slot named 'field': |# (setf (field o) 10)
; field might be a function that accesses a slot named field of a user-defined 
;object

; = Increment and Decrement =
(incf x)    #| === |# (setf x (+ x 1))  ; increment (default 1)
(decf x)    #| === |# (setf x (- x 1))  ; decrement (default 1)
(incf x 10) #| === |# (setf x (+ x 10)) ; increment by 10
; increments the value of an arbitrary element of an array
(incf (aref *array* (random (length *array*))))
#| The expansion of this macro must only call 'random' once, or else access and
assignment will target different random elements. incf/decf handle this.|#

; = Constants =
#| All constants are global and are defined with DEFCONSTANT. The basic form of 
DEFCONSTANT is like DEFPARAMETER. |#
(defconstant name initial-value-form [ documentation-string ])
#| thereafter the name can be used only to refer to the constant; it can't be 
used as a function parameter or rebound with any other binding form. Thus, many 
Lisp programmers follow a naming convention of using names starting and ending 
with + for constants. |#

; = Dynamic (Special) Variables =
#| In situations when you may want a variable to temporarily assune a new value
the 'let' keyword may be used to create an environment where this temporary
binding holds. |#

; = let =
; Temporarily bind new values to previously created variables within scope of
;the 'let' form
(let (variable*)
  body-form*)
; in addition to the above
(defvar b 7) 
; Create a temporary environment where 'a' and 'b' have new values
(let ((a 30)(b 70)) (+ a b)) #| --> |# 100 ; sum of temporary values was 100
; Evaluation of 'let' completed, therefore:
a #| --> |# 5 ; 'a' still bound to previous value
b #| --> |# 7 ; 'b' still bound to previous value

#| The scope of function parameters and LET variables is delimited by the form 
that introduces the variable. If you nest binding forms that introduce variables
with the same name, then the bindings of the innermost variable shadows the 
outer bindings.|#
(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |

; = let* =
#| In a LET*, the initial value forms for each variable can refer to variables 
introduced earlier in the variables list. Thus, you can write the following: |#
(let* ((x 10) (y (+ x 10))) (list x y))

; = Lexical Variables and Closures =
#| By default all binding forms in Common Lisp introduce lexically scoped 
variables. Lexically scoped variables can be referred to only by code that's 
textually within the binding form. |#
(let ((count 0)) (lambda () (setf count (1+ count))))
#| As it turns out, when count is a lexical variable, it just works. The binding
of count created when the flow of control entered the LET form will stick around
for as long as needed, in this case for as long as someone holds onto a 
reference to the function object returned by the LET form. The anonymous 
function is called a closure because it "closes over" the binding created by the
LET.

The key thing to understand about closures is that it's the binding, not the 
value of the variable, that's captured. Thus, a closure can not only access the 
value of the variables it closes over but can also assign new values that will 
persist between calls to the closure. |#

(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
; Then each time you invoke it, the value of count will increase by one.
(funcall *fn*) #| --> |# 1
(funcall *fn*) #| --> |# 2
(funcall *fn*) #| --> |# 3

#| A single closure can close over many variable bindings simply by referring to
them. Or multiple closures can capture the same binding. |#