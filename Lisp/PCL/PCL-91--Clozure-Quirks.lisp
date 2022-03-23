#|
PCL-91--Clozure-Quirks.lisp
James Watson, 2011 December
LispBox for Windows uses Clozure CL.  This is a guide to implementation quirks
I have encounted so far
|#

; = trace =
#| Clozure CL has a quirk in that a certain compiler optimization prevents 
recursive functions from being traced level-by-level.  Self-referential function
calls within a function definition are shortcuts, rather than explicit function 
calls on the symbol that you traced.  You can disable this optimization with the
following declaration within the function definition.|#
(defun fact (n) 
  "Returns the value for the factorial of n"
  (declare (notinline fact)) ; signals to compiler to disable discussed shortcut
  (if (zerop n) 1 (* n (fact (- n 1)))))
; URL: http://trac.clozure.com/ccl/wiki/DebugWithOpenmcl