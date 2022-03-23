#|
SICP-02--Building-Abstractions.lisp
James Watson, 2011 December

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Methods of data representation as means for abstraction
|#

#| Simple data are not sufficient for many of the problems we wish to address 
using computation. Programs are typically designed to model complex phenomena, 
and more often than not one must construct computational objects that have 
several parts in order to model real-world phenomena that have several aspects. 
Thus, we turn to another key aspect of any programming language: the means it 
provides for building abstractions by combining data objects to form compound 
data. 

Consider the task of designing a system to perform arithmetic with rational 
numbers. We could design a program in which each rational number would be 
represented by two integers (a numerator and a denominator). But this would be 
awkward, because we would then need to explicitly keep track of which numerators
corresponded to which denominators. In a system intended to perform many 
operations on many rational numbers, such bookkeeping details would clutter the 
programs substantially, to say nothing of what they would do to our minds. It 
would be much better if we could "glue together" a numerator and denominator to 
form a pair -- a compound data object -- that our programs could manipulate in a
way that would be consistent with regarding a rational number as a single 
conceptual unit.

The use of compound data also enables us to increase the modularity of our 
programs. If we can manipulate rational numbers directly as objects in their own
right, then we can separate the part of our program that deals with rational 
numbers per se from the details of how rational numbers may be represented as 
pairs of integers. The general technique of isolating the parts of a program 
that deal with how data objects are represented from the parts of a program that
deal with how data objects are used is a powerful design methodology called data
abstraction. Data abstraction makes programs much easier to design, maintain, 
and modify. 

Data abstraction is a methodology that enables us to isolate how a 
compound data object is used from the details of how it is constructed from more
primitive data objects. The basic idea of data abstraction is to structure the 
programs that are to use compound data objects so that they operate on 
"abstract data." That is, our programs should use data in such a way as to make 
no assumptions about the data that are not strictly necessary for performing the
task at hand. At the same time, a "concrete" data representation is defined 
independent of the programs that use the data. The interface between these two 
parts of our system will be a set of procedures, called selectors and 
constructors, that implement the abstract data in terms of the concrete 
representation. 

"Wishful Thinking": a design strategy wherein you imagine that an implementation
of a function, data-structure, etc. exists without stopping to figure out the
details of it |#


; = Pairs and 'cons' =

; = cons =
(cons object-1 object-2)
#| Takes two arguments and returns a compound data object (a cons) that contains
the two arguments as parts: the car of which is object-1 and the cdr of which is
object-2. |#

; A cons can be bound to a var name just as any other type.
(defvar my-cons (cons 1 2)) ; creates a cons and binds to 'my-cons'
; Accessing the two parts of a cons
(car my-cons) #| --> |# 1
(cdr my-cons) #| --> |# 2
#| 'car' and 'cdr' refer to hardware implementations that are no longer relevant
to Lisp on modern machines. 
* car - |C|ontents of the |A|ddress part of |R|egister number
* cdr - |C|ontents of the |D|ecrement part of |R|egister number
URL: http://en.wikipedia.org/wiki/CAR_and_CDR
|#

#| cons' may be nested. This ability to nest pairs means that cons' can be used 
as general-purpose building blocks to create all sorts of complex data 
structures. Data objects constructed from pairs are called list-structured data.
|#

; = A Definition of Data =
#| But exactly what is meant by data? In general, we can think of data as 
defined by some collection of selectors and constructors, together with 
specified conditions that these procedures must fulfill in order to be a valid 
representation. Consider the notion of a pair. We never actually said what a 
pair was, only that the language supplied procedures cons, car, and cdr for 
operating on pairs. But the only thing we need to know about these three 
operations is that if we glue two objects together using cons we can retrieve 
the objects using car and cdr. However, any triple of procedures that satisfies 
the above condition can be used as the basis for implementing pairs. This point 
is illustrated strikingly by the fact that we could implement cons, car, and cdr
without using any data structures at all but only using procedures: |#
(defun my-cons(x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (t (error "Argument not 0 or 1 -- CONS ~S~%" m)))))

(defun my-car(z)
  (funcall z 0))

(defun my-cdr(z)
  (funcall z 1))
#| Functionally, they're undistinguishable from the original cons, car, cdr. But
where exactly is the data structure here? Where is the data stored? It is 
stored in a closure - the anonymous procedure returned by my-cons. This 
procedure is a closure because it captures some information from the external 
environment (in this case the values of x and y) at the moment of its 
definition. |#


; = Representing Rational Numbers =
#| Pairs offer a natural way to complete the rational-number system. Simply 
represent a rational number as a pair of two integers: a numerator and a 
denominator. |#
(defun make-rat(num dem)
  "Construct a ratonal number consisting of a numerator and denominator"
  (cons num dem))
(defun numer(rat-num) "Access numerator of arg" (car rat-num))
(defun denom(rat-num) "Access denominator of arg" (cdr rat-num))

(defun print-rat(rat-num)
  "Print a rational number"
  (format t "~%~S / ~S" (numer rat-num) (denom rat-num)))
; URL: http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial-8.html

(defun add-rat(x y)
  "Return the sum of two rational numbers"
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun sub-rat(x y)
  "Return the difference of two rational numbers"
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun mul-rat(x y)
  "Return the product of two rational numbers"
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat(x y)
  "Return the quotient"
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat?(x y)
  "Return true if two rational numbers are equal, otherwise return NIL"
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; 'make-rat' can be re-defined so that the above return reduced fractions
(defun make-rat(n d)
  "Version of make-rat returns a reduced fraction"
  (let ((g (gcd n d))) ; gcd is a built-in CL function
    (cons (/ n g) (/ d g))))

; MAKE-RAT re-written to handle negative rational numbers
(defun make-rat (n d)
  (labels (
    (make-rat-reduce (n d)
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g)))))
    (cond ((and (< n 0) (< d 0))
            (make-rat-reduce (- n) (- d)))
          ((and (< d 0) (> n 0))
            (make-rat-reduce (- n) (- d)))
          (t
            (make-rat-reduce n d)))))

