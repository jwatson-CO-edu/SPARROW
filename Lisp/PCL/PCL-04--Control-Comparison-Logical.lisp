#|
PCL-04--Control-Comparison-Logical.lisp
James Watson, 2011 December
General comments on Control flow and logical comparison and operations
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

; == Control, Comparison, Logical Operators ==
#| LISP uses the self-evaluating symbol nil to mean false. Anything other than 
nil means true. Unless we have a reason not to, we usually use the 
self-evaluating symbol t to stand for true. |#

; - Logical Operators -
(not nil)             #| --> |# T
(not (= 1 1))         #| --> |# NIL
(and (= 1 2) (= 3 3)) #| --> |# NIL
(or (= 1 2) (= 3 3))  #| --> |# T

#| Notice that 'and' and 'or' are special forms, not procedures, because the 
subexpressions are not necessarily all evaluated. 'not' is an ordinary 
procedure. |#

; - Numerical Comparison Operators -
#| The functions <, >, <=, and >= order rationals and floating-point numbers (in
other words, the real numbers.) Like = and /=, these functions can be called 
with more than two arguments, in which case each argument is compared to the 
argument to its right. |#
(< 2 3)      #| --> |# T
(> 2 3)      #| --> |# NIL
(> 3 2)      #| --> |# T
(< 2 3 4)    #| --> |# T
(< 2 3 3)    #| --> |# NIL
(<= 2 3 3)   #| --> |# T
(<= 2 3 3 4) #| --> |# T
(<= 2 3 4 3) #| --> |# NIL

; - Character Comparison Operators - 
#|
Numeric | Case-     | Case-
Equiv.  | Sensitive | Insensitive 
--------+-----------+-----------------
=       | CHAR=     | CHAR-EQUAL 
/=      | CHAR/=    | CHAR-NOT-EQUAL 
<       | CHAR<     | CHAR-LESSP 
>       | CHAR>     | CHAR-GREATERP 
<=      | CHAR<=    | CHAR-NOT-GREATERP 
>=      | CHAR>=    | CHAR-NOT-LESSP         


  - String Comparison Operators -

Numeric | Case-     | Case-
Equiv.  | Sensitive | Insensitive 
--------+-----------+--------------------
=       | STRING=   | STRING-EQUAL 
/=      | STRING/=  | STRING-NOT-EQUAL 
<       | STRING<   | STRING-LESSP 
>       | STRING>   | STRING-GREATERP 
<=      | STRING<=  | STRING-NOT-GREATERP 
>=      | STRING>=  | STRING-NOT-LESSP       

Unlike the character and number comparators, the string comparators can compare 
only two strings. That's because they also take keyword arguments that allow you
to restrict the comparison to a substring of either or both strings. The 
arguments--:start1, :end1, :start2, and :end2--specify the starting (inclusive) 
and ending (exclusive) indices of substrings in the first and second string 
arguments. Thus, the following: |#
(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)
#| compares the substring "bar" in the two arguments and returns true. The :end1
and :end2 arguments can be NIL (or the keyword argument omitted altogether) to 
indicate that the corresponding substring extends to the end of the string. |#

; return the index in the first string where the mismatch was detected
(string/= "lisp" "lissome") #| --> |# 3

; If the first string is a prefix of the second, the return value will be one 
;greater than the largest valid index of the first string
(string< "lisp" "lisper") ==> 4

#| When comparing substrings, the resulting value is still an index into the 
string as a whole. For instance, the following compares the substrings "bar" and
"baz" but returns 5 because that's the index of the r in the first string: |#
(string< "foobar" "abaz" :start1 3 :start2 1) #| --> |# 5


; - Control Structure Macros -
; = if =
#| The first argument of if determines whether the second or third argument 
will be executed |#
(if t 5 6)   #| --> |# 5 ; t is true, evaluate first expression
(if nil 5 6) #| --> |# 6 ; nil is false, evaluate second expression
(if 4 5 6)   #| --> |# 5 ; 4 is true, evaluate first expression

#| If you need to put more than one statement in the then or else clause of an 
if statement, you can use the progn special form. Progn executes each statement
in its body, then returns the value of the final one.|#
(setq a 7)(setq b 0)(setq c 5)
(if (> a 5)
    (progn
      (setq a (+ b 7))
      (setq b (+ c 8)))
      (setq b 4)) #| --> |# 13


#| An if statement which lacks either a then or an else clause can be written 
using the 'when' or 'unless' special form.|#

; = when =
; opposite of 'unless'
(when t 3)   #| --> |# 3 ; t is true, evaluate expression
(when nil 3) #| --> |# NIL ; nil is false, return nil

; = unless =
; opposite of 'when'
(unless t 3)   #| --> |# NIL ; t is true, return nil
(unless nil 3) #| --> |# 3 ; nil is false, evaluate expression

; When and unless, unlike if, allow any number of statements in their bodies.
(when t
    (setq a 5)
    (+ a 6)) #| --> |# 11

; = cond =
#| A cond consists of the symbol 'cond' followed by a number of cond clauses, 
each of which is a list. The first element of a cond clause is the condition; 
the remaining elements (if any) are the action. The cond form finds the first 
clause whose condition evaluates to true (ie, doesn't evaluate to nil), 
executes the action, and returns the result; No other conditions are evaluated. 
|#
(setq a 3)
(cond
   ((evenp a) a)        ;if a is even return a
   ((> a 7) (/ a 2))    ;else if a is bigger than 7 return a/2
   ((< a 5) (- a 1))    ;else if a is smaller than 5 return a-1
   (t 17))              ;else return 17 ('t' used to form a default clause)
#| Result --> |# 2
; If the action in the selected cond clause is missing, cond returns what the 
;condition evaluated to:
(cond ((+ 3 4))) #| --> |# 7

; - Looping -
#| As it turns out, none of Lisp's 25 special operators directly support 
structured looping. All of Lisp's looping control constructs are macros built on
top of a pair of special operators that provide a primitive goto facility, 
TAGBODY and GO. |#

; = dolist =
; DOLIST loops across the items of a list, executing the loop body with a 
;variable holding the successive items of the list.
(dolist (var list-form)
  body-form*)
#| When the loop starts, the list-form is evaluated once to produce a list. Then
the body of the loop is evaluated once for each item in the list with the 
variable var holding the value of the item. For instance: |#
(dolist (x '(1 2 3)) (print x)) ; -->
1
2
3
NIL ; Used this way, the DOLIST form as a whole evaluates to NIL.
; If you want to break out of a DOLIST loop before the end of the list, RETURN.
(dolist (x '(1 2 3)) (print x) (if (evenp x) (return))) ; -->
1
2
NIL

; = dotimes =
; the high-level looping construct for counting loops
(dotimes (var count-form) ; count-form must evaluate to an integer
  body-form*)
; Each time through the loop var holds successive integers from 0 to one less 
;than count-form's result
(dotimes (i 4) (print i)) ; -->
0
1
2
3
NIL
; As with DOLIST, you can use RETURN to break out of the loop early.
; Nested dotimes
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

; = do =
#| DO is more general than either DOLIST or DOTIMES. Where DOLIST and DOTIMES 
provide only one loop variable, DO lets you bind any number of variables and 
gives you complete control over how they change on each step through the loop. 
You also get to define the test that determines when to end the loop and can 
provide a form to evaluate at the end of the loop to generate a return value for
the DO expression as a whole. |#
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
#| Each variable-definition introduces a variable that will be in scope in the 
body of the loop. The full form |#
(var init-form step-form)
#| The init-form will be evaluated at the beginning of the loop and the 
resulting values bound to the variable var. Before each subsequent iteration of 
the loop, the step-form will be evaluated and the new value assigned to var. The
step-form is optional; if it's left out, the variable will keep its value from 
iteration to iteration unless you explicitly assign it a new value in the loop 
body. As with the variable definitions in a LET, if the init-form is left out, 
the variable is bound to NIL. 

At the beginning of each iteration, after all the loop variables have been given
their new values, the end-test-form is evaluated. As long as it evaluates to 
NIL, the iteration proceeds, evaluating the statements in order.

Only after all the step forms have been evaluated are the variables given their 
new values.

When the end-test-form evaluates to true, the result-forms are evaluated, and 
the value of the last result form is returned as the value of the 
DO expression.|#

; DO loop that binds no variables
(do () ; empty variables lisp
    ((> (get-universal-time) *some-future-date*))
  (format t "Waiting~%")
  (sleep 60)) ; wait one minute

; = do* =
; assigns each variable its value >>before<< evaluating the step form for 
;subsequent variables.

; = loop =
#| LOOP macro repeatedly executes a body of expressions until it's exited 
by a call to RETURN |#
(loop (add-record (prompt-for-cd))
   (if (not (y-or-n-p "Another? [y/n]: ")) (return)))
; The simplest legal LOOP is an infinite one that binds no variables
(loop
  body-form*)
; Previous 1-minute sleep DO refactored as a LOOP
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 60))

; == Special Loop Clauses ==
#| The loop keywords append, appending, collect, collecting, nconc, and 
nconcing designate clauses that accumulate values in lists and return them. 
There is no semantic difference between the "ing" keywords and their 
non-"ing" counterparts. They are provided purely for the sake of stylistic 
diversity among users. 
   You can combine value-returning accumulation clauses in a loop if all the 
clauses accumulate the same type of data object. By default, the Loop Facility 
returns only one value; thus, the data objects collected by multiple 
accumulation clauses as return values must have compatible types. For example, 
since both the collect and append constructs accumulate objects into a list 
that is returned from a loop, you can combine them safely. |#
(loop for x from 1 to 10 summing (expt x 2)); --> 385, sums first ten squares
; counts the number of vowels in a string:
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou")) ; --> 11

; = collect -or- collecting =
;;; Collect and return odd numbers. 
(loop for i from 1 to 10 
      if (oddp i) collect i) 
#| --> |# (1 3 5 7 9)
