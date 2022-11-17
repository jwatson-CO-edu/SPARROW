#|
SICP-01--Elements-of-Programming.lisp
James Watson, 2011 December

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Basics of a program and programming
|#

#| 
== Evaluation Rules ==
* To evaluate a combination, do the following: 
  1. Evaluate the subexpressions of the combination.
  2. Apply the procedure that is the value of the leftmost subexpression (the 
     operator) to the arguments that are the values of the other subexpressions
     (the operands).
  3. To apply a compound procedure to arguments, evaluate the body of the 
     procedure with each formal parameter replaced by the corresponding argument
  4. When a primitive is encountered
     i. the values of numerals are the numbers that they name
    ii. values of built-in operators are the machine instruction sequences that 
        carry out the corresponding operations, and
   iii. values of other names are the objects associated with those names in the
        environment.

Tree structure of the evaluation of nested expressions.  Given the following
expression:|#
(* (+ 2 (* 4 6))
    (+ 3 5 7))
#| The interpreter will break it down into the constituent expressions, which
can be viewed as a tree structure. The terminal nodes (that is, nodes with no 
branches stemming from them) represent either operators or numbers. Evaluation 
percolates up from the terminal nodes, as seen below:

Evaluated Answer: 390 
                  /|\ 
                 / | \_
                /  |   \
               *   26  15_
              ___ //\  |\\\____
             /    / |  | \\__  \
            /    /  |  |  \  \  \
           +    2  24  +   3  5  7
                  /|\
                 / | \
                *  4  6

In order to accomplish the evaluation process for a combination we must first 
perform the evaluation process on each element of the combination. Thus, the 
evaluation rule is recursive in nature; that is, it includes, as one of its 
steps, the need to invoke the rule itself.  The evaluation rule is an example of
a general kind of process known as tree accumulation. |#

; == Functions ==
#|      Scheme: |# (define (square x) (* x x)) 
#| Common Lisp: |# (defun square(x) (* x x))

#| We can also use square as a building block in defining other procedures. For 
example, x^2 + y^2 can be expressed as |#
(+ (square x) (square y))

; Functions may be nested
(defun sum-of-squares(x y) (+ (square x) (square y)))

#|To evaluate a combination whose operator names a compound procedure, the 
interpreter follows much the same process as for combinations whose operators 
name primitive procedures. That is, the interpreter evaluates the elements of 
the combination and applies the procedure (which is the value of the operator of
the combination) to the arguments (which are the values of the operands of the 
combination).  This is not the only way to perform evaluation. An alternative 
evaluation model would not evaluate the operands until their values were needed.
Instead it would first substitute operand expressions for parameters until it 
obtained an expression involving only primitive operators, and would then 
perform the evaluation.

This alternative "fully expand and then reduce" evaluation method is known as 
normal-order evaluation, in contrast to the "evaluate the arguments and then 
apply" method that the interpreter actually uses, which is called 
applicative-order evaluation.

Lisp uses applicative-order evaluation, partly because of the additional 
efficiency obtained from avoiding multiple evaluations of expressions and, more 
significantly, because normal-order evaluation becomes much more complicated to 
deal with when we leave the realm of procedures that can be modeled by 
substitution. On the other hand, normal-order evaluation can be an extremely 
valuable tool. |#

; == Conditional Expressions and Predicates ==
; = Conditionals = 
(cond
   (predicate-1 consequent-expression-1)
 ; |<-------- clause ----------------->|

   (predicate-2 consequent-expression-2)
   ;...
   (predicate-n consequent-expression-n))

#| Conditional expressions are evaluated as follows. The predicate-1 is 
evaluated first. If its value is false, then predicate-2 is evaluated. If \
predicate-2's value is also false, then predicate-3 is evaluated. This process 
continues until a predicate is found whose value is true, in which case the 
interpreter returns the value of the corresponding consequent expression of the 
clause. The word predicate is used for procedures that return true or false, as 
well as for expressions that evaluate to true or false. |#

; == Introduction to Functional Programming ==
(defvar a 3)      
(defvar b (+ a 1))
; An 'if' expression inside an addition expression
(+ 2 (if (> b a) b a)) ; The second operand of the + expression conditionally
                       ;evaluates to a or b
#| --> |# 6
#| demonstrates the functional nature of Lisp.  All expressions evaluate to
something, and any expression can be a member of another expression. |#

; A condtional expression inside a multiplication expression
(* (cond ((> a b) a)
	 ((< a b) b)
	 (t -1))
   (+ a 1)) #| --> |# 16

; Conditionally apply a function to operands
(defun a-plus-abs-b (a b)
    (funcall (if (> b 0) #'+ #'-) a b))

#| Define a procedure that takes three numbers as arguments and returns the sum 
of the squares of the two larger numbers.  |#
(defun square(x) (* x x)) ; return the square of an argument
(defun least(a b) (if (< a b) a b)) ; return the least of two arguments
(defun top-two-squares(a b c)
    (- (+ (square a) (square b) (square c)) ; sum ALL squares
       (square (least c (least a b))))) ; subtract the LEAST square

#| The contrast between mathematical descriptions and software procedures is a 
reflection of the general distinction between describing properties of things 
and describing how to do things, or, as it is sometimes referred to, the 
distinction between declarative knowledge and imperative knowledge. In 
mathematics we are usually concerned with declarative (what is) descriptions, 
whereas in computer science we are usually concerned with imperative (how to) 
descriptions. |#

; An implementation & discussion of computation of square roots can be found at
; SICP_prog_1-1-7_NewtonsMethod.lisp

; == Procedures as Black Box Abstractions ==
#| It is crucial that each procedure accomplishes an identifiable task that can 
be used as a module in defining other procedures. So a procedure definition 
should be able to suppress detail. The users of the procedure may not have 
written the procedure themselves, but may have obtained it from another 
programmer as a black box. A user should not need to know how the procedure is 
implemented in order to use it.

A formal parameter of a procedure has a very special role in the procedure 
definition, in that it doesn't matter what name the formal parameter has. Such a
name is called a bound variable, and we say that the procedure definition binds 
its formal parameters. The meaning of a procedure definition is unchanged if a 
bound variable is consistently renamed throughout the definition. If a variable 
is not bound, we say that it is free. The set of expressions for which a binding
defines a name is called the scope of that name. In a procedure definition, the 
bound variables declared as the formal parameters of the procedure have the body
of the procedure as their scope.|#

; = Block Structure =
#| Fucntions may be composite and thus depend on many smaller functions.  The
roles of these smaller functions are abstracted away from the end user, who does
not care to think of them.  Lexical scoping can be used to define these subparts
within the larger abstraction so that necessary code does not get lost when 
the overall code library is moved, modified, or copied.  The obvious drawback
of this approach is that sub-parts thus lexically contained cannot be shared
by other top-level functions within the library. |#

#| Procedures found in 
SICP_prog_1-1-7_NewtonsMethod.lisp
can be compressed in the following manner: |#
(defun sqrt-sicp3 (x)
  "Initial square root implementation repackaged as a single function"
  (defun good-enough(guess)
    (< (abs (- (square guess) x)) 0.001))
  (defun improve(guess)
    (avg guess (/ x guess)))
  (defun sqrt-iter(guess)
    (if (good-enough guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
; Also using the following function
(defun avg(a b)
  "Returns the average of two arguments"
  (/ (+ a b) 2))

; == Tree Recursion ==
#| We can immediately translate the classic definition Fibonacci numbers into a 
recursive procedure: |#
(defun fib(n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
#| This procedure is instructive as a prototypical tree recursion, but it is a 
terrible way to compute Fibonacci numbers because it does so much redundant 
computation. Notice in the figure below that the entire computation of (fib 3) 
-- almost half the work -- is duplicated. In fact, it is not hard to show that 
the number of times the procedure will compute (fib 1) or (fib 0) (the number of
leaves in the above tree, in general) is precisely Fib(n + 1). To get an idea of
how bad this is, one can show that the value of Fib(n) grows exponentially with 
n. 

                            fib 5
                        ___/     \__________
                       /                    \
                  fib 4                      fib 3
              ___/     \                    /     \
             /          \                  /       \
        fib 3           fib 2           fib 2      fib 1
       /     \         /     \          /   \       |
   fib 2     fib 1   fib 1  fib 0   fib 1  fib 0    1
  /     \      |      |       |        |     |
fib 1  fib 0   1      1       0        1     0
 |       |
 1       0

Thus, the process uses a number of steps that grows exponentially with the 
input. On the other hand, the space required grows only linearly with the input,
because we need keep track only of which nodes are above us in the tree at any 
point in the computation. In general, the number of steps required by a 
tree-recursive process will be proportional to the number of nodes in the tree, 
while the space required will be proportional to the maximum depth of the tree.

We can also formulate an iterative process for computing the Fibonacci numbers. 
The idea is to use a pair of integers a and b, initialized to 
Fib(1) = 1 and Fib(0) = 0, and to repeatedly apply the simultaneous 
transformations
a + b --> a
b     --> b |#
(defun fib(n)
  "Fibonacci computed by linear recursion rather than tree recursion"
  (defun fib-iter(a b count)
    "Local recursive function"
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; - Fast Exponentiation -
#| We can also take advantage of successive squaring in computing exponentials 
in general if we use the rule 
b^n = (b^(n/2))^2 : If n is even
b^n = b * b^(n-1) : If n is odd |#
(defun fast-expt(b n)
  "Fast exponential function based on properties of exponents"
  (defun square(x) (* x x))
   (cond ((= n 0) 1)
         ((evenp n) (square (fast-expt b (/ n 2))))
         (t (* b (fast-expt b (- n 1))))))

; - Greatest Common Denominator -
#| Euclid's Algorithm: The idea of the algorithm is based on the observation 
that, if r is the remainder when a is divided by b, then the common divisors of 
a and b are precisely the same as the common divisors of b and r. Thus, we can 
use the equation:
GCD(a,b) = GCD(b,r) |#
(defun gcd-sicp(a b) ; gcd is already part of Common Lisp
   (if (= b 0)
       a
       (gcd b (rem a b))))

; - Testing for Primality -
(defun prime?(n)
  "Test whether a number is prime by searching for divisors"
  (defun divides?(a b)
    "Return true if b is divisible by a"
    (= (rem b a) 0))
  (defun find-divisor(n test-divisor)
    "Search for first divisor of n until found or it is verfied that n is prime"
    (cond ((> (expt test-divisor 2) n) n) ; search exceeded (sqrt n), is prime
	  ((divides? test-divisor n) test-divisor) ; divisor found, not prime
	  (t (find-divisor n (+ test-divisor 1))))) ; next iteration
  (defun smallest-divisor(n)
    "Begin searching for divisors at 2"
    (find-divisor n 2))
  (= n (smallest-divisor n)))

; == Formulating Abstractions with Higher-Order Functions ==
#| We have seen that functions are, in effect, abstractions that describe 
compound operations on numbers independent of the particular numbers. For 
example, when we |#
(defun cube(x) (* x x x))
#| we are not talking about the cube of a particular number, but rather about a 
method for obtaining the cube of any number. Of course we could get along 
without ever defining this function, by always writing expressions such as |#
(* 3 3 3)
(* y y y)        
#| and never mentioning 'cube' explicitly. This would force us to work always at
the level of the particular operations that happen to be primitives in the 
language. Our programs would be able to compute cubes, but our language would 
lack the ability to express the concept of cubing. One of the things we should 
demand from a powerful programming language is the ability to build abstractions
by assigning names to common patterns and then to work in terms of the 
abstractions directly.

Yet even in numerical processing we will be severely limited in our ability to 
create abstractions if we are restricted to functions whose parameters must be 
numbers. Often the same programming pattern will be used with a number of 
different procedures. To express such patterns as concepts, we will need to 
construct procedures that can accept procedures as arguments or return 
procedures as values. Procedures that manipulate procedures are called 
higher-order procedures, and they vastly increase the expressive power of a 
language. 

Consider the following three functions: |# 

; The first computes the sum of the integers from a through b:
(defun sum-integers(a b)
   (if (> a b)
       0
       (+ a (sum-integers (+ a 1) b))))

; The second computes the sum of the cubes of the integers in the given range:
(defun sum-cubes(a b)
   (if (> a b)
       0
       (+ (expt a 3) (sum-cubes (+ a 1) b))))

; The third computes the sum of a sequence of terms in a series which converges 
;to PI/8 (very slowly)
(defun pi-sum(a b)
   (if (> a b)
       0
       (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

#| These three procedures clearly share a common underlying pattern. They are 
for the most part identical, differing only in the name of the procedure, the 
function of a used to compute the term to be added, and the function that 
provides the next value of a. The presence of such a common pattern is strong 
evidence that there is a useful abstraction waiting to be brought to the 
surface. We would like our language to be powerful enough so that we can write a
procedure that expresses the concept of summation itself rather than only 
procedures that compute particular sums. We can do so readily: |#
(defun sum (term a next b)
  "Generalized summation function"
  ; term - a function that produces a term based on the argument
  ; a    - the first number in the sequence whose term will be summed 
  ; next - function to produce the next number in the sequence after the arg
  ; b    - last sequence member whose term will be summed
  (if (> a b)
    0 ; bound exceeded, add nothing to summation
    (+  (funcall term a) ; compute the first term and add to...
        (sum term (funcall next a) next b)))) ; successively generated terms
#| Note that when a function that a var points to is meant to be executed,
'funcall' must be used to invoke the function |#

; Then our pi-sum procedure can be expressed with the following anonyous
;functions
(defun pi-sum(a b)
   (sum (lambda(x) (/ 1.0 (* x (+ x 2))))
        a
        (lambda(x) (+ x 4))
        b))

#| In general, 'lambda' is used to create functionss in the same way as defun, 
except that no name is specified for the function. |#

; = Using 'let' to Create Local Variables =
#| When compositing complex functions wherin a series of operations is repeated
and may be thought of as a modular unit or term, 'let' can be used to compute
this term once and bind it to a temporary name.  Suppose a function f of two
variables: 
f(x,y) = x * (1 + x*y)^2 + y * (1 - y) + (1 + x*y) * (1 - y) 
f(x,y) = x *  a^2        + y *  b      +  a        *  b      |#
(defun f(x y)
   (let ((a (+ 1 (* x y)))
         (b (- 1 y)))
     (+ (* x (expt a 2))
        (* y b)
        (* a b))))

#| Some important aspects of the let expression:
* The scope of a variable specified by a let expression is the body of the let. 
* Let allows one to bind variables as locally as possible to where they are to 
  be used 
* The variables' values are computed outside the let. This matters when the 
  expressions that provide the values for the local variables depend upon 
  outside variables having the same names as the local variables themselves. |#

; Sometimes we can use internal definitions to get the same effect as with let.
(defun f(x y)
  (defvar a (+ 1 (* x y))) ; These will create global vars
  (defvar b (- 1 y))       ; Available outside of 'f'
     (+ (* x (expt a 2))
        (* y b)
        (* a b)))

#| In general, programming languages impose restrictions on the ways in which 
computational elements can be manipulated. Elements with the fewest restrictions
are said to have first-class status. Some of the "rights and privileges" of 
first-class elements are: 
  (1) They may be named by variables
  (2) They may be passed as arguments to procedures
  (3) They may be returned as the results of procedures
  (4) They may be included in data structures
Lisp, unlike other common programming languages, awards procedures full 
first-class status. This poses challenges for efficient implementation, but the 
resulting gain in expressive power is enormous. |#

#| Average damping is a useful general technique in itself. Namely, given a 
function f, we consider the function whose value at x is equal to the average of
x and f(x). |#
(defun average-damp(f)
   (lambda (x) (average x (f x))))
#| 'average-damp' is a procedure that takes as its argument a procedure f and 
returns as its value a procedure (produced by the lambda) that, when applied to 
a number x, produces the average of x and (f x). |#

; Using the same methods, we can define a function that returns the derivative
;function of the function it was passed
(defun deriv(g)
   (lambda (x)
     (/ (- (g (+ x dx)) (g x))
        dx)))
(defvar dx 0.00001)

