#|
PCL-05--Macros.lisp
James Watson, 2011 December
General comments on Macros
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

; == Macros ==
#| Each macro defines its own syntax, determining how the s-expressions it's 
passed are turned into Lisp forms: new syntactic abstractions. A macro is a 
function that takes s-expressions as arguments and returns a Lisp form that's 
then evaluated in place of the macro form.  First, the elements of the macro 
form are passed, unevaluated, to the macro function. Second, the form returned 
by the macro function --called its expansion-- is evaluated according to the 
normal evaluation rules. A specific Lisp interpreter may expand all the macros 
in the form being interpreted and then interpret the resulting code, or it could
start right in on interpreting the form and expand macros when it hits them.

When you compile a whole file of source code with the function COMPILE-FILE, all
the macro forms in the file are recursively expanded until the code consists of 
nothing but function call forms and special forms. This macro-less code is then 
compiled into a FASL file that the LOAD function knows how to load. The compiled
code, however, isn't executed until the file is loaded. Because macros generate 
their expansion at compile time, they can do relatively large amounts of work 
without having to pay for it when the file is loaded or the functions defined 
in the file are called.

Macros can provide a mechanism to implement commonly-used patterns suited to 
your needs with new syntax, to abstract away language-level bookkeeping details,
allowing you to express your true intent a bit more clearly.  It is also 
possible to conditionally generate code for use with client code on a 
case-by-case basis.  One application of this is to automatically streamline a 
very general function to one that narrowly applies to each situation, 
eliminating code that performs work or checks on data or parameters the client 
does not care about.  

As with functions, macros can also contain declarations.  |#

; = defmacro =
(defmacro name (parameter*)
  "Optional documentation string."
  body-form*)

(defmacro backwards(expr) (reverse expr)) ; returns the reverse of a sequence
(backwards ("hello, world" t format)) ; -->
hello, world
NIL
; Prior to evaluating 
("hello, world" t format)
#| It was sent to the macro, which reversed it, and sent back a properly formed
forwards call to format |# (format t "hello, world") ; to be evaluated 

; Suppose that your goal is to write a macro that outputs code in this format:
(do ((p (next-prime 0) (next-prime (1+ p)))) ; next-prime is defined elsewhere
    ((> p 19))
  (format t "~d " p))
; The following macro will do this:
(defmacro do-primes-r0 (var-and-range &rest body)
  #| The variables 'var', 'start', and 'end' each hold a value, extracted from 
var-and-range, that's then interpolated into the backquote expression that 
generates do-primes's expansion. |#
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

#| However, you don't need to take apart var-and-range "by hand" because macro 
parameter lists are what are called destructuring parameter lists. 
Destructuring, as the name suggests, involves taking apart a structure--in this 
case the list structure of the forms passed to a macro. You can replace 
 'var-and-range' with a list (var start end), and the three elements of the list 
will automatically be destructured into those three parameters. 

Another special feature of macro parameter lists is that you can use '&body' as 
a synonym for &rest. Semantically &body and &rest are equivalent, but many 
development environments will use the presence of a &body parameter to modify 
how they indent uses of the macro--typically &body parameters are used to hold a
list of forms that make up the body of the macro. |#

; do-primes with a destructuring parameter list
(defmacro do-primes-r1 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

#| In addition to being more concise, destructuring parameter lists also give 
you automatic error checking--with do-primes defined this way, Lisp will be able
to detect a call whose first argument isn't a three-element list and will give 
you a meaningful error message just as if you had called a function with too few
or too many arguments. |#

; = Macro Expansion & Runtime =
#| The key to understanding macros is to be quite clear about the distinction 
between the code that generates code (macros) and the code that eventually makes
up the program (everything else). Macros operate at a different level than 
functions and create a totally different kind of abstraction.  At macro 
expansion time, there's no way to access the data that will exist at runtime; 
only data that's inherent in the source code is available at time. |#

; = Macros, Complete Abstraction =

; Suppose the following code:
(defun foo (x)
  (when (> x 10) (print 'big)))
#| Since the program isn't running yet, there's no call to foo and thus no value
associated with x. Instead, the values the compiler passes to WHEN are the Lisp 
lists representing the source code, namely, (> x 10) and (print 'big). 
Given the following definition of WHEN: |#
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
#| When the code in foo is compiled, the WHEN macro will be run with those two 
forms as arguments. The parameter condition will be bound to the form (> x 10), 
and the form (print 'big) will be collected into a list that will become the 
value of the &rest body parameter. The backquote expression will then generate 
this code: |#
(if (> x 10) (progn (print 'big)))
#| Macros are always passed the unevaluated Lisp objects representing the 
subforms of the macro form, and the job of the macro is still to produce code 
that will do something rather than to do anything directly. 

Since the arguments passed to a macro are Lisp objects representing the source 
code of the macro call, the first step in any macro is to extract whatever parts
of those objects are needed to compute the expansion. For macros that simply 
interpolate their arguments directly into a template, this step is trivial: 
simply defining the right parameters to hold the different arguments is 
sufficient.|#

; The following two functions will be used in further discussion of macros

(defun primep (number)
  "Test whether a given number is prime"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Return the next prime number greater than or equal to the argument"
  (loop for n from number when (primep n) return n))

; Suppose you start with the idea that you want to be able to write this:
(do-primes (p 0 19)
  (format t "~d " p))
#| to express a loop that executes the body once each for each prime number 
greater or equal to 0 and less than or equal to 19, with the variable p holding 
the prime number. This could be done with a DO loop and the two functions 
above: |#
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

; do-primes first try
(defmacro do-primes-r0 (var-and-range &rest body)
  #| The variables 'var', 'start', and 'end' each hold a value, extracted from 
var-and-range, that's then interpolated into the backquote expression that 
generates do-primes's expansion. |#
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

#| However, you don't need to take apart var-and-range "by hand" because macro 
parameter lists are what are called destructuring parameter lists. 
Destructuring, as the name suggests, involves taking apart a structure--in this 
case the list structure of the forms passed to a macro. You can replace 
'var-and-range' with a list (var start end), and the three elements of the list 
will automatically be destructured into those three parameters. 

Another special feature of macro parameter lists is that you can use '&body' as 
a synonym for &rest. Semantically &body and &rest are equivalent, but many 
development environments will use the presence of a &body parameter to modify 
how they indent uses of the macro--typically &body parameters are used to hold a
list of forms that make up the body of the macro. |#

; do-primes with a destructuring parameter list
(defmacro do-primes-r1 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

#| In addition to being more concise, destructuring parameter lists also give 
you automatic error checking--with do-primes defined this way, Lisp will be able
to detect a call whose first argument isn't a three-element list and will give 
you a meaningful error message just as if you had called a function with too few
or too many arguments. 

It's important to note that backquote is just a convenience. But it's a big 
convenience. To appreciate how big, compare the backquoted version of do-primes 
to the following version, which uses explicit list-building code: |#
(defmacro do-primes-longhand ((var start end) &body body)
  (append '(do)
          (list  (list (list var
                             (list 'next-prime start)
                             (list 'next-prime (list '1+ var)))))
          (list (list (list '> var end)))
          body))

; = Leaky Abstractions =
#| As it turns out, a macro can leak details of its inner workings in three 
ways. The above definition suffers from one of the three possible macro leaks: 
namely, it evaluates the 'end' subform too many times. Suppose you were to call 
do-primes-r1 with, instead of a literal number such as 19, an expression such as
|# (random 100) ;in the end position. 

(do-primes (p 0 (random 100))
  (format t "~d " p))

Presumably the intent here is to loop over the primes from zero to whatever 
random number is returned by (random 100). However, this isn't what the current 
implementation does, as MACROEXPAND-1 shows.

 (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
     ((> P (RANDOM 100)))
   (FORMAT T "~d " P)) 

When this expansion code is run, RANDOM will be called each time the end test 
for the loop is evaluated. Thus, instead of looping until p is greater than an 
initially chosen random number, this loop will iterate until it happens to draw 
a random number less than or equal to the current value of p.

This is a leak in the abstraction because, to use the macro correctly, the 
caller needs to be aware that the end form is going to be evaluated more than 
once. Programmers will typically expect the forms they pass to macros to be 
evaluated no more times than absolutely necessary. 

You can fix the multiple evaluation easily enough; you just need to generate 
code that evaluates end once and saves the value in a variable to be used later.
|#

; do-primes evaluating 'end' exactly once and storing in a temp variable 
(defmacro do-primes-r2 ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

#| Unfortunately, this fix introduces two new leaks to the macro abstraction.

One new leak is similar to the multiple-evaluation leak you just fixed. Because 
the initialization forms for variables in a DO loop are evaluated in the order 
the variables are defined, when the macro expansion is evaluated, the expression
passed as end will be evaluated before the expression passed as start, opposite 
to the order they appear in the macro call. 

This leak is trivially plugged by swapping the order of the two variable 
definitions. |#

(defmacro do-primes-r3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

#| The last leak you need to plug was created by using the variable name 
ending-value. The problem is that the name, which ought to be a purely internal 
detail of the macro implementation, can end up interacting with code passed to 
the macro or in the context where the macro is called. The following seemingly 
innocent call to do-primes doesn't work correctly because of this leak:

 (do-primes (ending-value 0 10)
   (print ending-value)) 

Again, MACROEXPAND-1 can show you the problem. The first call expands to this:
 
 (do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
      (ending-value 10))
     ((> ending-value ending-value))
   (print ending-value)) 

Some Lisps may reject this code because ending-value is used twice as a variable
name in the same DO loop. If not rejected outright, the code will loop forever 
since ending-value will never be greater than itself. 

Clearly, what you need to patch this leak is a symbol that will never be used 
outside the code generated by the macro. The function GENSYM returns a unique 
symbol each time it's called. This is a symbol that has never been read by the 
Lisp reader and never will be because it isn't interned in any package. Thus, 
instead of using a literal name like ending-value, you can generate a new symbol
each time do-primes is expanded. |#

(defmacro do-primes-r4 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

#| Note that the code that calls GENSYM isn't part of the expansion; it runs as 
part of the macro expander and thus creates a new symbol each time the macro is 
expanded. This may seem a bit strange at first--ending-value-name is a variable 
whose value is the name of another variable. But really it's no different from 
the parameter var whose value is the name of a variable--the difference is the 
value of var was created by the reader when the macro form was read, and the 
value of ending-value-name is generated programmatically when the macro code 
runs. 

Not all literal names used in a macro expansion will necessarily cause a problem
--as you get more experience with the various binding forms, you'll be able to 
determine whether a given name is being used in a position that could cause a 
leak in a macro abstraction. But there's no real downside to using a gensymed 
name just to be safe. 

- Macro Guidelines -

Unless there's a particular reason to do otherwise, include any subforms in the 
expansion in positions that will be evaluated in the same order as the subforms 
appear in the macro call.

Unless there's a particular reason to do otherwise, make sure subforms are 
evaluated only once by creating a variable in the expansion to hold the value of
evaluating the argument form and then using that variable anywhere else the 
value is needed in the expansion.

Use GENSYM at macro expansion time to create variable names used only in the 
expansion.


= Macros Writing Macros =
'with-gensyms' needs to expand into a LET that binds each named variable, 
'ending-value-name' in this case, to a gensymed symbol. |#

(defmacro with-gensyms ((&rest names) &body body)
  "Expands into a LET that binds each named variable to a gensym symbol"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

; Fifth and final revision of do-primes
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
