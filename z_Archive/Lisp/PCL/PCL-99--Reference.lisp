#|
PCL-99--Reference.lisp
James Watson, 2011 December
General comments on Lisp language features
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#


;;; === Common Lisp Built-In Functions ===
#| Please refer to the Hyperspec for a complete listing.  This portion of the 
document is offered as a local, convenient listing of functions presented in
PCL and SICP.
URL: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm |#

; = append =
; Combine two or more lists and return the result as a new list
(append (list 1 2) (list 3 4)) #| --> |# (1 2 3 4)

; = cons =
(cons object-1 object-2)
#| Takes two arguments and returns a compound data object (a cons) that contains
the two arguments as parts: the car of which is object-1 and the cdr of which is
object-2. |#

; = consp =
; Returns true if argument is of type cons; otherwise returns false. 
(consp x)

; = error =
#| ERROR gives your program a standard way to announce a fatal error. You simply
compose an appropriate message using the format string and (optional) arguments,
and ERROR takes care of the rest. Your program never returns from the call to 
ERROR. Instead, the Lisp debugger will be entered. This makes ERROR a rather 
extreme response to a problem detected by your program. |#
(defun divide (numerator denominator)
    (when (zerop denominator)
      (error "Sorry, you can't divide by zero."))
    (/ numerator denominator))
; URL: http://www.psg.com/~dlamkins/sl/chapter23.html

; = exp =
; returns the value of e raised to the power of its argument

; = expt =
(expt base exponent) ; returns value of 'base' raised to 'exponent'

; = gcd =
; returns the greatest common denominator of the arguments

; = length =
; Returns the length of a proper sequence, including lists and strings
(length "abc")          #| --> |# 3
(length (list 1 2 3 4)) #| --> |# 4

; = list =
; Creates a list data structure object populated with the arguments
(list 1 2 3) #| --> |# (1 2 3)
#| A list is a chain of conses in which the car of each cons is an element of 
the list, and the cdr of each cons is either the next link in the chain or a 
terminating atom. |#

; = map =
#| Applies function to successive sets of arguments in which one argument is 
obtained from each sequence. The function is called first on all the elements 
with index 0, then on all those with index 1, and so on. |#
; (map <result-type> <function-to-apply> <list-to-transform>)
  (map 'list (lambda(x) (* x x)) (list 1 2 3 4)) #| --> |# (1 4 9 16)

; = max =
; Takes any number of real number arguments and returns the maximum value
(max 10 11) #| --> |# 11

; = min =
; Takes any number of real number arguments and returns the minimum value
(min -12 -10) #| --> |# -12

; = minusp =
; Returns true if the real number is less than zero

; = mod =
#| performs the operation floor on number and divisor and returns the remainder 
of the floor operation |#

; = plusp =
; Returns true if the real number is greater than zero

; = random =
(random n) #| accepts a positive number n and returns a number of the same kind 
between zero (inclusive) and n (exclusive). The number n may be an integer or a 
floating-point number. |#

; = rem =
#| performs the operation truncate on number and divisor and returns the 
remainder of the truncate operation |#

; = sqrt =
#| Returns the square root of the argument |#

; = zerop =
#| Returns true if argument is zero |#

;; == Common Lisp Built-In Math Functions ==
#| LOG, EXP, EXPT, SIN, COS, TAN, ASIN, ACOS, ATAN, SINH, COSH, TANH, ASINH,
ACOSH, ATANH, and more |#

;; == Named List Accessor Functions ==
; Retrieve the item in the list at the position with the same name as the func
(first list)    ==   (car list)
(second list)   ==   (car (cdr list))
(third list)    ==   (car (cddr list))
(fourth list)   ==   (car (cdddr list))
(fifth list)    ==   (car (cddddr list))
(sixth list)    ==   (car (cdr (cddddr list)))
(seventh list)  ==   (car (cddr (cddddr list)))
(eighth list)   ==   (car (cdddr (cddddr list)))
(ninth list)    ==   (car (cddddr (cddddr list)))
(tenth list)    ==   (car (cdr (cddddr (cddddr list))))

;; == Common Lisp Time Functions ==
; = get-universal-time =
#| return number of seconds that have elapsed since 00:00 of January 1, 1900 in 
the GMT time zone |#
(get-universal-time) #|-->|# 3535928759

; = decode-universal-time = 
#| return objects representing the tradtional components to build the date and 
time: |# (decode-universal-time (get-universal-time)) ; -->
27   ;- Seconds
29   ;- Minutes
20   ;- Hours [8pm]
18   ;- Day (Calendar)
1    ;- Month [January]
2012 ;- Year
2    ;- Day of Week (0:Monday to 6:Sunday)
NIL  ;- Daylight 
6    ;- Time Zone, a number of hours offset from GMT
      ; Time zone values increase with motion to the west

; = get-decoded-time =
#| get the calendar time representation of the current time directly,
a shortcut for |# (decode-universal-time (get-universal-time))

; = encode-universal-time =
#| encode a calendar time into the corresponding universal time. This function 
takes six mandatory arguments (seconds, minutes, hours, date, month and year) 
and one optional argument (the time zone) |#

; == Common Lisp Special Operators ==
#| Regular functions evaluate all the argument elements before passing them to
the function.  However, this would impose a restriction on some desired 
behaviors, and Common Lisp has defined 25 Special Operators to address this. |#

; = append =
; appends one list to another
(append '(a b c) '(c d e)) #| --> |# (A B C C D E)

; = function =
#| The special operator FUNCTION provides the mechanism for getting at a 
function object itself. It takes a single argument and returns the function 
with that name.|# 
CL-USER> (defun foo (x) (* 2 x))
FOO
CL-USER> (function foo)
#<Interpreted Function FOO>
; #' is syntactic sugar for 'function'
(function foo) ; identical
#'foo          ; identical

; = let =
; Allows the use of local variables within the LET form
(let ((var1 init-form-1)
      (var2 init-form-2)
      ;...
      (varm init-form-m))
  ; Code making use of vars 1 to m
)
#|first evaluates the expressions init-form-1, init-form-2, and so on, in that 
order, saving the resulting values. Then all of the variables varj are bound to 
the corresponding values; each binding is lexical unless there is a special 
declaration to the contrary.|#

; = let* =
#| let* is similar to let, but the bindings of variables are performed 
sequentially rather than in parallel. The expression for the init-form of a var 
can refer to vars previously bound in the let*.  |#

; = quote =
; Takes a single expression as its "argument" and returns it unevaluated.
; ' is syntax sugar for the quote operator
(quote (+ 1 2)) ; identical
'(+ 1 2)        ; identical






; == Common Lisp Reserved Names and Constants ==
nil ; The empty list, equivalent to "false"
; The word nil is a contraction of the Latin word nihil, which means "nothing"

PI ; a constant variable whose value is the best possible floating-point 
;approximation to the mathematical constant pi

call-arguments-limit ; a constant whose value is equal to the maximum number
;of arguments that may be passed to a function, value is implementation-specific


; == Common Lisp Built-In Macros ==
; = Modify Macros =
#| Modify macros are macros built on top of SETF that modify places by assigning
a new value based on the current value of the place. The main benefit of modify 
macros is that they're more concise than the same modification written out using
SETF. |#

; = incf and decf =
(incf x)    #| equivalent to |# (setf x (+ x 1))  ; increment (default 1)
(decf x)    #| equivalent to |# (setf x (- x 1))  ; decrement (default 1)

; = push =
(push [newItem] [listVar]) 
; Push item onto the front of a list or other structure,
;returns the new value of the variable (listVar) it's modifying.

; = pop =
; POP performs the inverse operation of the PUSH macro

; = rotatef =
(rotatef a b) ; swaps the values of the two variables and returns NIL

; = shiftf =
#| the last argument provides a value that's moved to the second-to-last 
argument while the rest of the values are moved one to the left. The original 
value of the first argument is simply returned. |#
(shiftf a b 10)

; = End Modify Macros =

; = compile-file =
; Compile a Lisp file into a form that loads faster, with all macros expanded
(compile-file "myfile.lisp")
#| When a file is compiled, the object file created has a .fas or .fsl or .fasl 
or .afasl extension. Depends on the Lisp compiler. |#

; = compile-file-if-needed =
; Compiles a Lisp source file if it is newer than the corresponding 
;compiled file.

; = defparameter =
#| DEFPARAMETER defines global variable with dynamic scoping. Value for variable
is reevaluated for each occurence (unlike with DEFVAR). |#

; = format =
(format t "~{~a:~10t~a~%~}~%" cd)

#|The ~a directive is the aesthetic directive; it means to consume one argument
and output it in a human-readable form. This will render keywords without the 
leading : and strings without quotation marks.|#
(format t "~a" "Dixie Chicks")
; Result: 
Dixie Chicks
NIL

#|The ~t directive is for tabulating. 
The ~10t tells FORMAT to emit enough spaces to move to the tenth column before 
processing the next ~a. A ~t doesn't consume any arguments.|# 
(format t "~a:~10t~a" :artist "Dixie Chicks") ; Results in:
ARTIST:   Dixie Chicks
NIL

#| 'format' will also iterate over lists. 
When FORMAT sees ~{ the next argument to be consumed must be a list. 
FORMAT loops over that list, processing the directives between the ~{ and ~}, 
consuming as many elements of the list as needed each time through the list. |#

; The ~% emits a newline, and does not consume any arguments.

#| The ~S accepts any LISP object and is replaced by a printed representation of
that object.  Consumes one argument. Arguments should be listed after the 
string, in the order that they should appear in the final formatted string |#
(format t "~%~S / ~S" (numer rat-num) (denom rat-num)) #| --> |# 2 / 3
; Where numer and denom return the numerator and denominator of rat-num
; URL: http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial-8.html


; = load =
; Load a lisp file to make its resources available to the environment,
;compiling first if needed
(load "myfile.fas")
#| You can omit the extension (".lisp", ".afasl", etc.) from the filename, but 
what happens as a result is implementation-dependent.  In general, to be safe, 
always load the full name of the file including the extension. 

When the compiler compiles the file, one common thing it will complain of is 
special variables. For all intents and purposes, a special variable is a global 
variable. With very few exceptions, you should never use global variables when 
you can use local variables instead. |#

; = macroexpand =
#| keeps expanding the result as long as the first element of the resulting 
expansion is the name of the macro. However, this will often show you a much 
lower-level view of what the code is doing than you want, since basic control 
constructs such as DO are also implemented as macros. |#

; = macroexpand-1 =
; Expands a macro call into the full expression that it outputs, the way it 
;would be read by the Lisp interpreter

; = parse-integer =
; Parse a string into an integer value
(parse-integer (prompt-read "Rating") :junk-allowed t)
#|  The default behavior of PARSE-INTEGER is to signal an error if it can't 
parse an integer out of the string or if there's any non-numeric junk in the 
string. However, it takes an optional keyword argument :junk-allowed, which 
tells it to relax a bit when set to 't'.|#

; = remove-if-not =
#| REMOVE-IF-NOT takes a predicate and a list and returns a list containing 
only the elements of the original list that match the predicate. However, 
REMOVE-IF-NOT doesn't really remove anything: it creates a new list, 
leaving the original list untouched. |#
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10)) ; results in:
(2 4 6 8 10)
#| You can also pass REMOVE-IF-NOT an anonymous function. For instance, 
if EVENP didn't exist, you can write the previous expression as the following:|#
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10)); Result:
(2 4 6 8 10)

; = reverse =
; takes a list as an argument and returns a new list that is its reverse
(reverse (list 1 2 3)) #| --> |# (3 2 1)

; = unless =
; opposite of WHEN
(unless t 3)   #|-->|# NIL ; t is true, return nil
(unless nil 3) #|-->|# 3 ; nil is false, evaluate expression
(unless nil (prin1 1) (prin1 2) (prin1 3)) #|-->|# 123

; = when =
; opposite of UNLESS
(when t 3)   #| --> |# 3 ; t is true, evaluate expression
(when nil 3) #| --> |# NIL ; nil is false, return nil

; = with-open-file =
#| The WITH-OPEN-FILE macro opens a file, binds the stream to a variable, 
executes a set of expressions, and then closes the file. It also makes sure 
the file is closed even if something goes wrong while evaluating the body. |#

; = with-standard-io-syntax =
#| The macro WITH-STANDARD-IO-SYNTAX ensures that certain variables that 
affect the behavior of PRINT are set to their standard values. |#
(with-standard-io-syntax (print *db* out))
;                         ^- print database to text stream 'out'

; = y-or-n-p =
; Prompt the user with a "Y or N" question and  return a boolean
(y-or-n-p "Do you want this to happen? [y/n]: ")



;; == Special Operators ==
; = flet = 
#|  defines functions that can be referred to only within the scope of the FLET 
form. See LABELS. |#

; = labels =
#| defines functions that can be referred to only within the scope of the LABELS
form.  The names introduced by LABELS can be used immediately, including in the 
bodies of the functions defined by the LABELS. Thus, LABELS can define recursive
functions, while FLET can't. The LABELS special operator, as well as 'flet', is 
handy when you need local functions that are a bit too complex to define inline 
as LAMBDA expressions or that you need to use more than once. |#
(defun foo(x)
  (labels (
	   (add-two(y) (+ y 2))
	   (mult-two(z) (* z 2))))
  (add-two (mult-two x)))