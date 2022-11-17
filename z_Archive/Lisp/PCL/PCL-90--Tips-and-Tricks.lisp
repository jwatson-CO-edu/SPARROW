#|
PCL-90--Tips-and-Tricks.lisp
James Watson, 2011 December
General comments on programmin Lisp, resolution of common issues
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#


; = Print-Pretty for Readability =
#| If the macro expansion is shown all on one line, it's probably because the 
variable *PRINT-PRETTY* is NIL. If it is, evaluating |# 
(setf *print-pretty* t) 
#| should make the macro expansion easier to read. |#

; == Debugging and Analysis ==
; = trace =
#| Invoking trace with one or more function-names causes the denoted functions 
to be traced. Whenever a traced function is invoked, information about the call,
about the arguments passed, and about any eventually returned values is printed 
to trace output. If trace is used with no function-names, no tracing action is 
performed; instead, a list of the functions being traced is returned. |#
(trace function-name)

; = untrace =
#| Invoking untrace with one or more function names causes those functions to be
untraced (i.e., no longer traced). If untrace is used with no function-names, 
all functions currently being traced are untraced. |#

; = Checking for Matched Parens =
M-x check-parens ; Emacs checks that parentheses and quotes all match
#| This command is especially useful for "READ failure" type errors |#

;; == Clozure Quirks ==
; = Tracing Recursive Functions =
#| Clozure CL has a quirk in that a certain compiler optimization prevents 
recursive functions from being traced level-by-level.  Self-referential function
calls within a function definition are shortcuts, rather than explicit function 
calls on the symbol that you traced.  You can disable this optimization with the
following declaration within the function definition.|#
(defun fact (n) 
  "Returns the value for the factorial of n"
  (declare (notinline fact)) ; signals to compiler to disable discussed shortcut
  (if (zerop n) 1 (* n (fact (- n 1)))))

;; == Steel Bank Quirks ==
; = Calling Functions within Other Functions =
#| If one function calls another, the compiler will give a style warning if the
called function is not defined before the function that calls it |#

;; == Functions ==
; = Compiling Nested Functions =
#| It may be the case that you want a certain nested function to be compiled
before the REPL attempts to run the code block that contains it.  You may ask 
for it to be compiled specifically and thus allow it to leverage any compiler
optimizations you may have been depending on. |#
(defun full-fermat-test (n)
  (defun aux-test (a)
    (cond ((= a 1) t)
          ((/= (expmod a n n) a) nil)
          (t (aux-test (1- a)))))
  (compile 'aux-test) ; aux-test will be compiled
  (aux-test (1- n)))