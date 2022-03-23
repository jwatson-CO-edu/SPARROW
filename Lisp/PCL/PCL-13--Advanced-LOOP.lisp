#|
PCL-13--Advanced-LOOP.lisp
James Watson, 2012 March
The LOOP macro and its own domain-specific language
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

;; == The Parts of a LOOP ==
#| You can do the following in a LOOP (automatically or conditionally):
* Step variables numerically and over various data structures
* Collect, count, sum, minimize, and maximize values seen while looping
* Execute arbitrary Lisp expressions
* Decide when to terminate the loop

Additionally, LOOP provides syntax for the following:
* Creating local variables for use within the loop
* Specifying arbitrary Lisp expressions to run before and after the loop proper
 
The basic structure of a LOOP is a set of clauses, each of which begins with a 
loop keyword. How each clause is parsed by the LOOP macro depends on the 
keyword. Some of the main keywords, which you saw in Chapter 7, are for, 
collecting, summing, counting, do, and finally. LOOP keywords aren't keywords in
the normal sense of being symbols in the KEYWORD package. |#

; = Iteration Control =
#| Most of the so-called iteration control clauses start with the loop keyword 
for, or its synonym as,2 followed by the name of a variable. What follows after 
the variable name depends on the type of for clause. 

The subclauses of a for clause can iterate over the following:
* Ranges of numbers, up or down, by specified intervals
* The individual items of a list
* The cons cells that make up a list
* The elements of a vector, including subtypes such as strings and bit vectors
* The pairs of a hash table
* The symbols in a package
* The results of repeatedly evaluating a given form

A single loop can have multiple for clauses with each clause naming its own 
variable. When a loop has multiple for clauses, the loop terminates as soon as 
any for clause reaches its end condition. For instance, the following loop will 
iterate at most ten times but may stop sooner if list contains fewer than ten 
items: |#
(loop
  for item in list
  for i from 1 to 10
  do (something))

; == Counting Loops ==
#| These clauses consist of from one to three of the following prepositional 
phrases after the for (or as): 

* the from where phrase, 
  - specifies the initial value of the clause's variable. 
  - It consists of one of the prepositions from, downfrom, or upfrom followed by
    a form, which supplies the initial value (a number).

* the to where phrase, 
  - specifies a stopping point for the loop 
  - consists of one of the prepositions to, upto, below, downto, or above 
    followed by a form, which supplies the stopping point.
    > With upto and downto, the loop body will be terminated (without executing 
      the body again) when the variable passes the stopping point
    > with below and above, it stops one iteration earlier. 

* and the by how much phrase. 
  - consists of the prepositions by and a form, which must evaluate to a 
    positive number. The variable will be stepped (up or down, as determined by 
    the other phrases) by this amount on each iteration or by one if it's 
    omitted.

You must specify at least one of these prepositional phrases. The defaults are 
to start at zero, increment the variable by one at each iteration, and go 
forever or, more likely, until some other clause terminates the loop. 

The only wrinkle is that if you want decremental stepping, there's no default 
from where value, so you must specify one with either from or downfrom. So, the 
following collects the first eleven integers (zero to ten). |#
(loop for i upto 10 collect i)
; but the behavior of this
(loop for i downto -10 collect i) ; wrong
; is undefined. Instead, you need to write this:
(loop for i from 0 downto -10 collect i)

#| Also note that because LOOP is a macro, which runs at compile time, it has to
be able to determine the direction to step the variable based solely on the 
prepositions--not the values of the forms, which may not be known until 
runtime. 

Finally, if you just want a loop that repeats a certain number of times: |#
(loop repeat 4
   do (print "foo")) ; -->
"foo"
"foo"
"foo"
"foo"
; the repeat clause doesn't create an explicit loop variable.

;; == Looping Over Collections and Packages ==
#| The for clauses for iterating over lists are much simpler than the arithmetic
clauses. They support only two prepositional phrases, in and on. |#
for var in list-form
; Trivial example collects all the members of a list, into a list
(loop for i in (list 10 20 30 40) collect i) #|-->|# (10 20 30 40)

#| Occasionally this clause is supplemented with a by phrase, which specifies a 
function to use to move down the list. The default is CDR but can be any 
function that takes a list and returns a sublist. For instance, you could 
collect every other element of a list with a loop like this: |#
(loop for i in (list 10 20 30 40) by #'cddr collect i) #|-->|# (10 30)

#| An on prepositional phrase is used to step var over the cons cells that make 
up a list. |#
(loop for x on (list 10 20 30) collect x) ==> ((10 20 30) (20 30) (30))
; The sublist at each cell was collected as a member of a new list

; This phrase too can take a by preposition:
(loop for x on (list 10 20 30 40) by #'cddr collect x) ; --> 
((10 20 30 40) (30 40))

#| Looping over the elements of a vector (which includes strings and bit 
vectors) is similar to looping over the elements of a list except the 
preposition across is used instead of in. For instance: |#
(loop for x across "abcd" collect x) #|-->|# (#\a #\b #\c #\d)

#| You may wonder why LOOP can't figure out whether it's looping over a list or 
a vector without needing different prepositions. This is another consequence of 
LOOP being a macro: the value of the list or vector won't be known until 
runtime, but LOOP, as a macro, has to generate code at compile time. And LOOP's 
designers wanted it to generate extremely efficient code. To be able to generate
efficient code for looping across, say, a vector, it needs to know at compile 
time that the value will be a vector at runtime--thus, the different 
prepositions are needed. |#

#| Iterating over a hash table or package is slightly more complicated because 
hash tables and packages have different sets of values you might want to iterate
over--the keys or values in a hash table and the different kinds of symbols in a
package. Both kinds of iteration follow the same pattern. The basic pattern 
looks like this: |#
(loop for var being the things in hash-or-package ...)

#| For Hash Tables:
the possible values for things are hash-keys and hash-values, which cause var to
be bound to successive values of either the keys or the values of the hash 
table. The hash-or-package form is evaluated once to produce a value, which must
be a hash table. 
Since you'll often want both the keys and the values when iterating over a hash 
table, the hash table clauses support a using subclause at the end of the hash 
table clause. |#
(loop for k being the hash-keys in h using (hash-value v) ...)
(loop for v being the hash-values in h using (hash-key k) ...)
#| Both of these loops will bind k to each key in the hash table and v to the 
corresponding value. Note that the first element of the using subclause must be 
in the singular form. |#

#| For Packages: 
things can be symbols, present-symbols, and external-symbols, which cause var to
be bound to each of the symbols accessible in a package, each of the symbols 
present in a package (in other words, interned or imported into that package), 
or each of the symbols that have been exported from the package. The 
hash-or-package form is evaluated to produce the name of a package, which is 
looked up as if by FIND-PACKAGE or a package object. Synonyms are also available
for parts of the for clause. In place of the, you can use each; you can use of 
instead of in; and you can write the things in the singular (for example, 
hash-key or symbol). |#

;; == Equals-Then Iteration ==
#| If none of the other for clauses supports exactly the form of variable 
stepping you need, you can take complete control over stepping with an 
equals-then clause. This clause is similar to the binding clauses in a DO loop 
but cast in a more Algolish syntax. The template is as follows: |#
(loop for var = initial-value-form [ then step-form ] ...)
#| As usual, var is the name of the variable to be stepped. Its initial value is
obtained by evaluating initial-value-form once before the first iteration. In 
each subsequent iteration, step-form is evaluated, and its value becomes the new
value of var. With no then part to the clause, the initial-value-form is 
reevaluated on each iteration to provide the new value. Note that this is 
different from a DO binding clause with no step form. 

The step-form can refer to other loop variables, including variables created by 
other for clauses later in the loop. For instance: |#
(loop repeat 5 
      for x = 0 then y
      for y = 1 then (+ x y)
      collect y) ; --> 
(1 2 4 8 16)
#| Note that each for clause is evaluated separately in the order it appears. So
in the previous loop, on the second iteration x is set to the value of y before 
y changes (in other words, 1). But y is then set to the sum of its old value 
 (still 1) and the new value of x. If the order of the for clauses is reversed, 
the results change. |#
(loop repeat 5
      for y = 1 then (+ x y)
      for x = 0 then y
      collect y) ; --> 
(1 1 2 4 8)

#| Often, however, you'll want the step forms for multiple variables to be 
evaluated before any of the variables is given its new value (similar to how DO 
steps its variables). In that case, you can join multiple for clauses by 
replacing all but the first 'for' with 'and'. |#
(loop repeat 5 
      for x = 0 then y
      and y = 1 then (+ x y)
      collect y) ; --> 
(1 1 2 3 5)

; = Local Variables =
#| While the main variables needed within a loop are usually declared implicitly
in for clauses, sometimes you'll need auxiliary variables, which you can declare
with with clauses. |#
with var [ = value-form ]
#| The name var becomes the name of a local variable that will cease to exist 
when the loop finishes. If the with clause contains an = value-form part, the 
variable will be initialized, before the first iteration of the loop, to the 
value of value-form. |#

(loop repeat 5
   with temp = 5
   collect temp) ; -->
(5 5 5 5 5)

#| Multiple with clauses can appear in a loop; each clause is evaluated 
independently in the order it appears and the value is assigned before 
proceeding to the next clause, allowing later variables to depend on the value 
of already declared variables. Mutually independent variables can be declared in
one 'with' clause with an 'and' between each declaration. |#



; = Destructuring Variables =
#| One handy feature of LOOP is the ability to destructure list values assigned 
to loop variables. This lets you take apart the value of lists that would 
otherwise be assigned to a loop variable, similar to the way DESTRUCTURING-BIND 
works but a bit less elaborate. Basically, you can replace any loop variable in 
a for or with clause with a tree of symbols, and the list value that would have 
been assigned to the simple variable will instead be destructured into variables
named by the symbols in the tree. A simple example looks like this: |#
(loop for (a b) in '((1 2) (3 4) (5 6))
            do (format t "a: ~a; b: ~a~%" a b)) #| -->
a: 1; b: 2
a: 3; b: 4
a: 5; b: 6
NIL
|#

#| The tree can also include dotted lists, in which case the name after the dot 
acts like a &rest parameter, being bound to a list containing any remaining 
elements of the list. This is particularly handy with for/on loops since the 
value is always a list. For instance, this LOOP |#
(loop for cons on list
    do (format t "~a" (car cons))
    when (cdr cons) do (format t ", "))
; could also be written like this:
(loop for (item . rest) on list
    do (format t "~a" item)
    when rest do (format t ", "))

#| If you want to ignore a value in the destructured list, you can use NIL in 
place of a variable name. |#
(loop for (a nil) in '((1 2) (3 4) (5 6)) collect a) #|-->|# (1 3 5)

#| If the destructuring list contains more variables than there are values in 
the list, the extra variables are set to NIL, making all the variables 
essentially like &optional parameters. There isn't, however, any equivalent to 
&key parameters. |#

;; == Value Accumulation ==
#| The value accumulation clauses provide a concise notation for a handful of 
common loop idioms having to do with accumulating values while looping. Each 
accumulation clause starts with a verb and follows this pattern: |#
verb form [ into var ]

#| Each time through the loop, an accumulation clause evaluates form and saves 
the value in a manner determined by the verb. With an into subclause, the value 
is saved into the variable named by var. The variable is local to the loop, as 
if it'd been declared in a with clause. With no into subclause, the accumulation
clause instead accumulates a default value for the whole loop expression. 


The possible verbs are collect, append, nconc, count, sum, maximize, and 
minimize. Also available as synonyms are the present participle forms: 
collecting, appending, nconcing, counting, summing, maximizing, 
and minimizing. 

Related to collect are the verbs append and nconc. These verbs also accumulate 
values into a list, but they join the values, which must be lists, into a single
list as if by the functions APPEND or NCONC. 

  = Collecting Values =
The verb count counts the number of times form is true, sum collects a running 
total of the values of form, maximize collects the largest value seen for form, 
and minimize collects the smallest. For instance, suppose you define a variable 
*random* that contains a list of random numbers.|#
(defparameter *random* (loop repeat 100 collect (random 10000)))
#| Then the following loop will return a list containing various summary 
information about the numbers: |#
(loop for i in *random*
   counting (evenp i) into evens
   counting (oddp i) into odds
   summing i into total
   maximizing i into max
   minimizing i into min
   finally (return (list min max total evens odds)))

;; == Unconditional Execution ==
#| The simplest way to execute arbitrary code within a loop body is with a do 
clause. A do clause consists of the word do (or doing) followed by one or more 
Lisp forms that are all evaluated when the do clause is. The do clause ends at 
the closing parenthesis of the loop or the next loop keyword. 

Another, more dramatic, form of immediate execution is a return clause. This 
clause consists of the word return followed by a single Lisp form, which is 
evaluated, with the resulting value immediately returned as the value of the 
loop. 

You can also break out of a loop in a do clause using any of Lisp's normal 
control flow operators, such as RETURN and RETURN-FROM. Note that a return 
clause always returns from the immediately enclosing LOOP expression, while a 
RETURN or RETURN-FROM in a do clause can return from any enclosing expression.|#


;; == Conditional Execution ==
#| Because a do clause can contain arbitrary Lisp forms, you can use any Lisp 
expressions you want, including control constructs such as IF and WHEN. So, the 
following is one way to write a loop that prints only the even numbers between 
one and ten: |#
(loop for i from 1 to 10 do (when (evenp i) (print i)))

#| However, sometimes you'll want conditional control at the level of loop 
clauses. You couldn't write such a loop with a do clause because there'd be no 
way to "call" the sum i in the middle of a regular Lisp form. In cases like 
this, you need to use one of LOOP's own conditional expressions like this: |#
(loop for i from 1 to 10 when (evenp i) sum i) #|-->|# 30

#| LOOP provides three conditional constructs, and they all follow this basic 
pattern: |#
conditional test-form loop-clause
#| The conditional can be if, when, or unless. The test-form is any regular Lisp
form, and loop-clause can be a value accumulation clause (count, collect, and so
on), an unconditional execution clause, or another conditional execution clause.
Multiple loop clauses can be attached to a single conditional by joining them 
with 'and'. 

Within the first loop clause, after the test form, you can use the variable it 
to refer to the value returned by the test form. For instance, the following 
loop collects the non-NIL values found in some-hash when looking up the keys in 
some-list: |#
(loop for key in some-list when (gethash key some-hash) collect it)
#| 'if' and 'unless' are other possible clauses. Unlike their Common Lisp 
namesakes, LOOP's if and when are merely synonyms--there's no difference in 
their behavior. 

All three conditional clauses can also take an else branch, which is followed by
another loop clause or multiple clauses joined by and. When conditional clauses 
are nested, the set of clauses connected to an inner conditional clause can be 
closed with the word end. The end is optional when not needed to disambiguate a 
nested conditional--the end of a conditional clause will be inferred from the 
end of the loop or the start of another clause not joined by and. |#

;; "While" can also serve as a termination check.  Both
;; "do" and "collect" can be combined in one expression.
(loop for x from 1
      for y = (* x 10)
      while (< y 100)
      do (print (* x 5))
      collect y)
; URL: http://www.ai.sri.com/pkarp/loop.html


;; == Setting Up and Tearing Down ==
#| One of the key insights the designers of the LOOP language had about actual 
loops "in the wild" is that the loop proper is often preceded by a bit of code 
to set things up and then followed by some more code that does something with 
the values computed by the loop. Thus, LOOP provides two keywords, 'initially' 
and 'finally', that introduce code to be run outside the loop's main body. 

All the initially forms are combined into a single prologue, which runs once, 
immediately after all the local loop variables are initialized and before the 
body of the loop. The finally forms are similarly combined into a epilogue to be
run after the last iteration of the loop body. Both the prologue and epilogue 
code can refer to local loop variables.

The prologue is always run, even if the loop body iterates zero times. The loop 
can return without running the epilogue if any of the following happens:
* A return clause executes.
* RETURN , RETURN-FROM, or another transfer of control construct is called from 
  within a Lisp form within the body.
* The loop is terminated by an always, never, or thereis clause

Within the epilogue code, RETURN or RETURN-FROM can be used to explicitly 
provide a return value for the loop. Such an explicit return value will take 
precedence over any value that might otherwise be provided by an accumulation or
termination test clause. 

To allow RETURN-FROM to be used to return from a specific loop (useful when 
nesting LOOP expressions), you can name a LOOP with the loop keyword named. If a
named clause appears in a loop, it must be the first clause. For a simple 
example, assume lists is a list of lists and you want to find an item that 
matches some criteria in one of those nested lists. You could find it with a 
pair of nested loops like this: |#
(loop named outer for list in lists do
     (loop for item in list do
          (if (what-i-am-looking-for-p item)
            (return-from outer item))))

;; == Termination Tests ==
#| Just as there are common patterns for accumulating values, there are also 
common patterns for deciding when it's time to bail on a loop. These patterns 
are supported in LOOP by the termination clauses, while, until, always, never, 
and thereis. They all follow the same pattern. |#
loop-keyword test-form
#| All five evaluate test-form each time through the iteration and decide, based
on the resulting value, whether to terminate the loop. 

When while or until decides to terminate the loop, control passes to the 
epilogue, skipping the rest of the loop body. The epilogue can then return a 
value or do whatever it wants to finish the loop. A while clause terminates the 
loop the first time the test form is false; until, conversely, stops it the 
first time the test form is true. 

The LOOP-FINISH macro causes an immediate jump to the loop epilogue. This is a 
regular Lisp form, not a loop clause, so it can be used anywhere within the Lisp
forms of a do clause. 

The other three clauses--always, never, and thereis--terminate the loop with 
extreme prejudice; they immediately return from the loop, skipping not only any 
subsequent loop clauses but also the epilogue. They also provide a default value
for the loop even when they don't cause the loop to terminate. However, if the 
loop is not terminated by one of these termination tests, the epilogue is run 
and can return a value other than the default provided by the termination 
clauses. The always and never clauses return only boolean values. If the test 
form fails (returning NIL in an always clause or non-NIL in a never clause), the
loop is immediately terminated, returning NIL. If the loop runs to completion, 
the default value of T is provided. 

For instance, if you want to test that all the numbers in a list, numbers, are 
even, you can write this:|#
(if (loop for n in numbers always (evenp n))
    (print "All numbers even."))

#| A 'thereis' clause is used to test whether the test form is ever true. As 
soon as the test form returns a non-NIL value, the loop is terminated, returning
that value. If the loop runs to completion, the thereis clause provides a 
default return value of NIL. |#
(loop for char across "abc123" thereis (digit-char-p char)) #|-->|# 1
(loop for char across "abcdef" thereis (digit-char-p char)) #|-->|# NIL

;; == Other LOOP Examples ==
(loop for i in (list 1 2 3) do (print i) collect (+ i 2)) ;-->
1
2
3
(3 4 5)

#| For integers 1 to 100 (inclusive) print the number, then print a string 
that contains that number of 'x's. |#
(loop for i from 1 to 100 do 
     (progn (print i) 
	    (let ((str "")) 
	      (print (loop for j from 1 to i do
			  (setq str (concatenate 'string str "x")) 
			finally (return str)))))) 
                      ; The 'return' keyword was added because the LOOP would
                      ; have otherwise returned NIL. 'finally' executes after
                      ; the loop has completed 

;; == LOOP Rules ==
#| 
* The named clause, if any, must be the first clause.
* After the named clause come all the initially, with, for, and repeat clauses.
* Then comes the body clauses: conditional and unconditional execution, 
  accumulation, and termination test.
  - There MUST be at least one action word to define the body, 
    such as DO or COLLECT
* End with any finally clauses. 

The LOOP macro will expand into code that performs the following actions:
* Initializes all local loop variables as declared with with or for clauses as 
  well as those implicitly created by accumulation clauses. The initial value 
  forms are evaluated in the order the clauses appear in the loop.
* Execute the forms provided by any initially clauses--the prologue--in the 
  order they appear in the loop.
* Iterate, executing the body of the loop as described in the next paragraph.
  Execute the forms provided by any finally clauses--the epilogue--in the order 
  they appear in the loop. 

While the loop is iterating, the body is executed by first stepping any 
iteration control variables and then executing any conditional or unconditional 
execution, accumulation, or termination test clauses in the order they appear in
the loop code. If any of the clauses in the loop body terminate the loop, the 
rest of the body is skipped and the loop returns, possibly after running the 
epilogue. |#