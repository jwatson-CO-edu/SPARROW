#|
SICP_prog_1-1-7_NewtonsMethod.lisp
James Watson, 2011 December
Newton's Method of finding square roots, lifted from SICP 
|#

; = Square Roots by Newton's Method =
#| How does one compute square roots? The most common way is to use Newton's 
method of successive approximations, which says that whenever we have a guess y 
for the value of the square root of a number x, we can perform a simple 
manipulation to get a better guess (one closer to the actual square root) by 
averaging y with (/ x y). The idea is to improve the answer until it is close 
enough so that its square differs from the radicand by less than a predetermined
tolerance (here 0.001).|#

(defun square(x) 
  "Return the square of the argument"
  (* x x))

(defun avg(a b)
  "Returns the average of two arguments"
  (/ (+ a b) 2))

(defun improve(guess x)
  "Guess improved by averaging it with the quotient of radicand and old guess"
  (avg guess 
       (/ x guess)))

(defun good-enough(guess x)
  "Determine if the guess lies within 0.001 of the true square root"
  (< (abs (- (square guess)
	     x))
     0.001))

(defun sqrt-iter(guess x)
  "Use Newton's iterative method to determine the square root"
  (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) 
		 x)))

; Common Lisp already has defined function 'sqrt'
(defun sqrt-sicp(x)
  "Newton's Method of Square Roots wrapped with initial guess 1.0"
  (sqrt-iter 1.0 x))

#| There indeed is a problem with very large and very small numbers in the 
current implementation. 

The problem with very small numbers is easy to understand. Since we have an 
explicit epsilon value in guess, 0.001, the function will give incorrect results
for inputs that approach the epsilon. Consider the 0.005 I used in my example – 
the square of 0.022 is 0.00484, which is within 0.001 of 0.005, while 0.022 is 
50% smaller than the correct answer 0.036

The problem with large numbers is a little more tricky. The square root 
computations must be performed in floating point arithmetic, which has limited 
precision in CL systems (on the other hand, integer arithmetic is arbitrarily 
large). Floating point numbers are usually represented with an exponent and a 
mantissa, and when the exponent grows large, the “quantas” the number can 
advance in become large. In our example, after a few iterations the number 
3.039737E8 was reached and from there the process entered infinite recursion 
because (improve 3.039737E8) returns 3.039737E8 itself

To solve the problem, we’ll do as suggested by the authors, and implement 
good-enough as a test of how guess changes from one iteration to the next and 
stop when the change is a very small fraction of the guess. Here is the improved
code: |#

(defun  sqrt-iter2 (guess x)
  "Improvement solution given by Eli Bendersky"
    (let ((improved-guess (improve guess x)))
      (if (close-enough guess improved-guess)
        improved-guess
        (sqrt-iter improved-guess x))))

 (defun close-enough (a b)
   "Tolerance defined as a ratio rather than a fixed number"
    (let ((ratio (/ a b)))
      (and (< ratio 1.001) (> ratio 0.999))))

(defun sqrt-sicp2 (x)
    (sqrt-iter2 1.0 x))

; = 1.1.8: Black box Abstraction =
; The first example above may be compressed by taking advantage of the ability
;to define functions within functions.  A first attempt at such might look like
;the following
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
#| The above works, but it doesn't quite achieve the intended result of 
abstracting the SQRT concept to a black box, because DEFUN defines functions at 
the top level, and can be called outside of SQRT. This may lead to unintended
behavior as X has been closed over. A better approach would be to use LABELS:|#
(defun sqrt-sicp4 (x) 
  "Square root implementation repackaged using LABELS to define local functions"
  (labels ((good-enough? (guess)
	     (< (abs (- (square guess) x)) .001))
	   (improve (guess)
	     (average guess (/ x guess)))
	   (sqrt-iter (guess)
	     (if (good-enough? guess x)
		 guess
		 (sqrt-iter (improve guess)))))
    (sqrt-iter 1)))


#| Notice that in contrast to the first implementation in this file , x has 
scope across the entire function and does not have to be specified as an 
argument for each sub-function. |#