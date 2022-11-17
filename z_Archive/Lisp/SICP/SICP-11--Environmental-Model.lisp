#|
SICP-11--Environmental-Model.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

The Environmental Model of Evaluation
|#

#| A variable must somehow designate a "place" in which values can be stored. In
our new model of evaluation, these places will be maintained in structures 
called environments. 

An environment is a sequence of frames. Each frame is a table (possibly empty) 
of bindings, which associate variable names with their corresponding values. (A 
single frame may contain at most one binding for any variable.) Each frame also 
has a pointer to its enclosing environment, unless, for the purposes of 
discussion, the frame is considered to be global. The value of a variable with 
respect to an environment is the value given by the binding of the variable in 
the first frame in the environment that contains a binding for that variable. If
no frame in the sequence specifies a binding for the variable, then the variable
is said to be unbound in the environment. 

        +----------+ 
        |Frame I   |
        | x: 3     |
        | y: 5     |
        +----------+
          ^      ^
        C |      | D
+----------+    +----------+
|Frame II  |    |Frame III |
| z: 6     |    | n: 1     |
| x: 7     |    | y: 2     |
+----------+    +----------+
          ^      ^
          |      |
          A      B

The figure above shows a simple environment structure consisting of three 
frames, labeled I, II, and III. In the diagram, A, B, C, and D are pointers to 
environments. C and D point to the same environment. The variables z and x are 
bound in frame II, while y and x are bound in frame I. The value of x in 
environment D is 3. The value of x with respect to environment B is also 3. This
is determined as follows: We examine the first frame in the sequence (frame III)
and do not find a binding for x, so we proceed to the enclosing environment D 
and find the binding in frame I. On the other hand, the value of x in 
environment A is 7, because the first frame in the sequence (frame II) contains 
a binding of x to 7. With respect to environment A, the binding of x to 7 in 
frame II is said to shadow the binding of x to 3 in frame I.

The environment is crucial to the evaluation process, because it determines the 
context in which an expression should be evaluated. Indeed, one could say that 
expressions in a programming language do not, in themselves, have any meaning. 
Rather, an expression acquires a meaning only with respect to some environment 
in which it is evaluated. Even the interpretation of an expression as 
straightforward as (+ 1 1) depends on an understanding that one is operating in 
a context in which + is the symbol bound to the procedure for addition. |#


; = The Rules for Evaluation =

#| The overall specification of how the interpreter evaluates a combination 
remains the same as when we first introduced it: 

To evaluate a combination:
 1. Evaluate the subexpressions of the combination.
 2. Apply the value of the operator subexpression to the values of the operand 
    subexpressions.

The environment model of evaluation replaces the substitution model in 
specifying what it means to apply a compound procedure to arguments.

In the environment model of evaluation, a procedure is always a pair consisting 
of some code and a pointer to an environment. Procedures are created in one way 
only: by evaluating a LAMBDA expression. This produces a procedure whose code is
obtained from the text of the lambda expression and whose environment is the 
environment in which the lambda expression was evaluated to produce the 
procedure. For example, consider the procedure definition: |#
(defun square (x)
  (* x x))
#| evaluated in the global environment. The procedure definition syntax is just 
syntactic sugar for an underlying implicit lambda expression. This is similar 
to |#
(defvar square #'(lambda (x) (* x x))) 
#| but it is not exactly the same. Here is an area where the Scheme used in SICP
is different from Common Lisp. In Scheme a procedure definition is equivalent 
to |#
(define square (lambda (x) (* x x)))
#| because Scheme stores names that point to values and names that point to 
procedures in the same environment, or namespace.  Common Lisp has separate
environments for variables and functions. If you treat a variable name like a
function name in Common Lisp, the compiler will give you an error. You can, 
however, store a Common Lisp function in a variable if you give the compiler
special notification. When you plan to pass around the result of LAMBDA form,
you must prefix it with a #' (which is a shortcut for the FUNCTION form). Then,
when the result of that LAMBDA form will be called as a function, it must be 
done with FUNCALL. The multiple-namespace design choice CL has made means that
passing functions is a bit more cumbersome for CL than it is for Scheme. 

The figure below shows the result of evaluating this define expression. The 
procedure object is a pair whose code specifies that the procedure has one 
parameter, x, and a procedure body (* x x). The environment part of the 
procedure is a pointer to the global function environment, since that is the 
environment in which the lambda expression was evaluated to produce the 
procedure. A new binding, which associates the procedure object with the symbol 
square, has been added to the global function frame. In general, DEFUN creates 
definitions by adding bindings to frames. 

Function Env.   +----------------+
   \_,--------->| Function Frame |
                | ~~~~~~~~~~~~~~ |
                | Other Vars     |
                | square         |
                +-----|----------+ 
                      |  ^
                    __v_/                                       
                    |*|*|
                     |
                     v
                 params: x
                   body: (* x x)

The environment model specifies: To apply a procedure to arguments, create a new
environment containing a frame that binds the parameters to the values of the 
arguments. The enclosing environment of this frame is the environment specified 
by the procedure. Now, within this new environment, evaluate the procedure 
body. 

To show how this rule is followed, the figure below illustrates the environment 
structure created by evaluating the expression (square 5) in the global 
environment, where square is the procedure generated in the figure above. 
Applying the procedure results in the creation of a new environment, labeled E1 
in the figure, that begins with a frame in which x, the formal parameter for the
procedure, is bound to the argument 5. The pointer leading upward from this 
frame shows that the frame's enclosing environment is the global environment. 
The global environment is chosen here, because this is the environment that is 
indicated as part of the square procedure object. Within E1, we evaluate the 
body of the procedure, (* x x). Since the value of x in E1 is 5, the result is 
 (* 5 5), or 25. 

Function Env.   +----------------------------+
   \_,--------->| Function Frame             |
                | ~~~~~~~~~~~~~~             |
                | Other Vars                 |
                | square                     |
                +-----|----------------------+ 
                      |  ^             ^
                    __v_/              |                         
                    |*|*|          +------+
                     |        E1-->| x: 5 |
                     v             +------+
                 params: x
                   body: (* x x)

The environment model of procedure application can be summarized by two rules:

* A procedure object is applied to a set of arguments by constructing a frame, 
  binding the formal parameters of the procedure to the arguments of the call, 
  and then evaluating the body of the procedure in the context of the new 
  environment constructed. The new frame has as its enclosing environment the 
  environment part of the procedure object being applied.

* A procedure is created by evaluating a lambda expression relative to a given 
  environment. The resulting procedure object is a pair consisting of the text 
  of the lambda expression and a pointer to the environment in which the 
  procedure was created.

We also specify that defining a symbol using DEFUN/DEFVAR creates a binding in 
the current environment frame and assigns to the symbol the indicated value. 
Evaluating the expression (setq <variable> <value>) in some environment locates 
the binding of the variable in the environment and changes that binding to 
indicate the new value. That is, one finds the first frame in the environment 
that contains a binding for the variable and modifies that frame. If the 
variable is unbound in the environment, then setq signals an error. |#

; This file only covers material through section 3.2.1 to this point
