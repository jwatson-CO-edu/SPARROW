#|
PCL-02--Functions-and-Lambda.lisp
James Watson, 2011 December
General comments on first and higher order functions
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

; == Functions ==
; Functions are defined by the 'defun' macro.  defun skeleton:
(defun name (parameters*)
  "Optional documentation string"
  body-forms*)
#| defun: define function, function name, parameter list, definition
The entire function definition is inside parens.  A function's parameter list 
defines the variables that will be used to hold the arguments passed to the 
function when it's called. If the function takes no arguments, 
the list is empty, written as ()

Any symbol can be used as a function name. Usually function names contain 
only alphabetic characters and hyphens, but other characters are allowed and 
are used in certain naming conventions. 

If a string literal follows the parameter list, it's a documentation string 
that should describe the purpose of the function. When the function is defined, 
the documentation string will be associated with the name of the function and 
can later be obtained using the DOCUMENTATION function.|#

#| The evaluation rule for function call forms is simple: evaluate the 
remaining elements of the list after the function name as Lisp forms and pass 
the resulting values to the named function.  Therefore, all the elements of the
list after the first must themselves be well-formed Lisp forms.|#
(defun hello-world() (format t "hello, world"))
#| The body of a DEFUN consists of any number of Lisp expressions. They will be 
evaluated in order when the function is called and the value of the last 
expression is returned as the value of the function. Or the RETURN-FROM special 
operator can be used to return immediately from anywhere in a function. |#

; Function calls must also be inside parens, as with all statements
(hello-world) ; note that hyphens '-' are valid in function names
; REPL Result:
hello, world ; format prints this to standard output
NIL ; The return value of 'format', nil in this case

; A function with arguments, separated by spaces
(defun make-cd(title artist rating ripped) 
  (list :title title :artist artist :rating rating :ripped ripped))
; Another function 
(defun add-record(cd) (push cd *db*)) 
; A nested function call
(add-record (make-cd "Roses" "Kathy Mattea" 7 t)) 
; Note that the args are not in parents, except when arg is an expression

#| When a parameter list is a simple list of variable names the parameters are 
called "required parameters". When a function is called, it must be supplied 
with one argument for every required parameter. Each parameter is bound to the 
corresponding argument. If a function is called with too few or too many 
arguments, Lisp will signal an error.|#

; = Optional Parameters =
#| To define a function with optional parameters, place the symbol '&optional' 
followed by the names of the optional parameters.  Any parameters appearing 
before '&optional' will be required parameters. |#
(defun foo (a b &optional c d) (list a b c d))
#| Arguments are first bound to the required parameters. After all the required 
parameters have been given values, if there are any arguments left, their values
are assigned to the optional parameters.  |#
(foo 1 2)     #| --> |# (1 2 NIL NIL)
(foo 1 2 3)   #| --> |# (1 2 3 NIL)
(foo 1 2 3 4) #| --> |# (1 2 3 4)
#| Lisp will still check that an appropriate number of arguments are passed to 
the function --in this case between two and four, inclusive-- and will signal 
an error if the function is called with too few or too many.  Optional 
parameters are positional 

You can specify the default value by replacing the parameter name with a list
containing a name and an expression. The expression will be evaluated only if 
the caller doesn't pass enough arguments to provide a value for the optional 
parameter. The default-value expression can refer to parameters that occur 
earlier in the parameter list.|#
(defun foo (a &optional (b 10)) (list a b))
;                       ^- Parameter specifier
(foo 1 2) #| --> |# (1 2)   ; 2nd arg supplied, default expression ignored
(foo 1)   #| --> |# (1 10)  ; No 2nd arg, eval default expr and assign to b

; - Optional Supplied Parameter Flag -
#| If you add another variable name to the parameter specifier after the 
default-value expression, this variable will be bound to true if the caller 
actually supplied an argument for this parameter and NIL otherwise. By 
convention, these variables are usually named the same as the actual parameter 
with a "-supplied-p" on the end. For example:|#
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
; function calls
(foo 1 2)   #| --> |# (1 2 3 NIL)
(foo 1 2 3) #| --> |# (1 2 3 T)
(foo 1 2 4) #| --> |# (1 2 4 T)

; = Rest Parameters =
#| If a function includes a '&rest' parameter, any arguments remaining after 
values have been doled out to all the required and optional parameters are 
gathered up into a list that becomes the value of the &rest parameter. |#
(defun format (stream string &rest values) ...)
(defun + (&rest numbers) ...) 

; = Keyword Parameters =
#| '&key' specifies keyword parameters and offers a mechanism to selectively 
pass values to arguments that may not necessarily be contiguous or sequential in
the function definition. |#
(defun foo(&key a b c) (list a b c)) ; Generates a list containing the args
; A selection of legal calls to the above, and their results:
(foo :a 1 :b 2 :c 3) #| --> |# (1 2 3)
(foo :c 3 :b 2 :a 1) #| --> |# (1 2 3)
(foo :a 1 :c 3)      #| --> |# (1 nil 3)
(foo)                #| --> |# (nil nil nil)
#| Normally if a function is called with no argument for a particular keyword 
parameter, the parameter will have the value NIL. As with optional parameters, 
keyword parameters can provide a default value form and the name of a supplied-p
variable. Here's a version of foo that uses this feature: |#
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
; The same calls as above with the following results:
(foo :a 1 :b 2 :c 3)  #| --> |# (1 2 3 T)
(foo :c 3 :b 2 :a 1)  #| --> |# (1 2 3 T)
(foo :a 1 :c 3)       #| --> |# (1 20 3 T)
(foo)                 #| --> |# (NIL 20 30 NIL)
; The default value form can refer to parameters that appear earlier in the 
;parameter list.

; = Mixing Parameter Types =
#| It's possible to use all four flavors of parameters in a single function. 
They must be declared in the following order: first the required parameters, 
then the optional parameters, then the rest parameter, and finally the keyword 
parameters. 

Combining '&optional' and '&key' parameters yields surprising enough results 
that you should probably avoid it altogether. The problem is that if a caller 
doesn't supply values for all the optional parameters, then those parameters 
will eat up the keywords and values intended for the keyword parameters.|#
(defun foo (x &optional y &key z) (list x y z))
(foo 1 2 :z 3) #| --> |# (1 2 3)     ; okay
(foo 1)        #| --> |# (1 nil nil) ; also okay
(foo 1 :z 3)   #| -->    ERROR:        This is because the keyword :z is taken 
as a value to fill the optional y parameter, leaving only the argument 3 to be 
processed. At that point, Lisp will be expecting either a keyword/value pair or 
nothing and will complain. Perhaps even worse, if the function had had two 
&optional parameters, this last call would have resulted in the values :z and 3 
being bound to the two &optional parameters and the &key parameter z getting the
default value NIL with no indication that anything was amiss. 

In general, if you find yourself writing a function that uses both &optional and
&key parameters, you should probably just change it to use all &key parameters--
they're more flexible, and you can always add new keyword parameters without 
disturbing existing callers of the function. You can also remove keyword 
parameters, as long as no one is using them.

If both '&rest' and '&key' appear in a parameter list, all the remaining values,
which include the keywords themselves, are gathered into a list that's bound to 
the &rest parameter, and the appropriate values are also bound to the &key 
parameters. So, given this function:|#
(defun foo (&rest rest &key a b c) (list rest a b c))
(foo :a 1 :b 2 :c 3) #| --> |# ((:A 1 :B 2 :C 3) 1 2 3)

; = Function Return Values =
#| The most common way to return the value of a function is rely on the default
function behavior of returning the value of the last expression evaluated.
However, Lisp provides a mechanism to return from the middle of a function and
break out of nested control structures: 'return-from' |#
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
#| You must specify the block of code you are returning from, in this case the
function.  (defun automatically creates a block with the same name as the 
function.) 'return-from' is used less frequently in Lisp than the equivalents in
other languages due to the fact that every Lisp expression evaluates to a value
|#


; = Block Structure =
#| Fucntions may be composite and thus depend on many smaller functions.  The
roles of these smaller functions are abstracted away from the end user, who does
not care to think of them.  Lexical scoping can be used to define these subparts
within the larger abstraction so that necessary code does not get lost when 
the overall code library is moved, modified, or copied.  The obvious drawback
of this approach is that sub-parts thus lexically contained cannot be shared
by other top-level functions within the library. |#
(defun fib(n)
  "Fibonacci computed by linear recursion rather than tree recursion"
  (defun fib-iter(a b count)
    "Local recursive function"
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; == Higher Order Functions ==
#| While the main way you use functions is to call them by name, a number of 
situations exist where it's useful to be able treat functions as data. For 
instance, you can write a general-purpose sorting function while allowing the 
caller to provide a function that's responsible for comparing any two elements. 
Then the same underlying algorithm can be used with many different comparison 
functions. Similarly, callbacks and hooks depend on being able to store 
references to code in order to run it later. In Lisp, functions are just another
kind of object.

Common Lisp provides two functions for invoking a function through a function 
object: FUNCALL and APPLY. They differ only in how they obtain the arguments to 
pass to the function.|#

; = funcall =
#| The function "funcall" takes a function and individual arguments, 
and calls the function with the specified arguments. It is usually used when 
the particular function to be called is not known in advance, but is passed in|#
(foo 1 2 3)           ; identical
(funcall #'foo 1 2 3) ; identical
#| However, there's little point in using FUNCALL to call a function whose name 
you know when you write the code. 

  The following accepts a function object as an argument and plots a simple 
ASCII histogram of the values returned by the argument function when it's 
invoked on the values from min to max, stepping by step: |#
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
#| The FUNCALL expression computes the value of the function for each value of 
i. The inner LOOP uses that computed value to determine how many times to print 
an asterisk to standard output. Note that you don't use FUNCTION or #' to get 
the function value of fn; you want it to be interpreted as a variable because 
it's the variable's value that will be the function object. You can call plot 
with any function that takes a single numeric argument.  

funcall cannot be used if the arguments are not known at compile-time.  'apply'
can be used to call a function and pass an arbitrary number of arguments. |#

; = apply =
#| The "apply" function is similar to "funcall", but it expects the arguments to
come in a list. It is normally used when you have an indeterminate number of 
arguments. As a further convenience, APPLY can also accept "loose" arguments as 
long as the last argument is a list. The argument list produced by combining any
loose arguments with the final list must be a legal argument list for the 
function with enough arguments for all the required parameters and only 
appropriate keyword parameters. |#

; = lambda =
#| Create an anonymous function. Other than the lack of a name, 
a LAMBDA expression looks a lot like a DEFUN: the word lambda is followed 
by a parameter list, which is followed by the body of the function. Anonymous 
functions can be useful when you need to pass a function as an argument to 
another function and the function you need to pass is simple enough to express 
inline.|#
(remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10)); Result:
(1 3 5 7 9) ; A list of odd numbers in the original list returned
; A lambda function has access to the context in which it is embedded
; LAMBDA expressions can be used anywhere a normal function name can be.

#| The read syntax #' is a short-hand for using function. Generally, 
it is not necessary to use either #' or function; just use an unquoted lambda 
form instead. (Actually, lambda is a macro defined using function.) 
The following forms are all equivalent: |#
#'(lambda (x) (* x x))
(function (lambda (x) (* x x)))
(lambda (x) (* x x))
; URL: http://www.gnu.org/s/emacs/manual/html_node/elisp/Anonymous-Functions.html#Anonymous-Functions
