#|
SICP-05--Symbolic-Data.lisp
James Watson, 2012 March

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Working with arbitrary symbols as data
|#

;; == Quotation ==
; If we can form compound data using symbols, we can have lists such as
(a b c d)
(23 45 17)
((Norah 12) (Molly 9) (Anna 7) (Lauren 6) (Charlotte 4))
; Lists containing symbols can look just like the expressions of our language:
(* (+ 23 45) (+ x 9))
(defun fact(n) (if (= n 1) 1 (* n (fact (- n 1)))))

#| In order to manipulate symbols we need a new element in our language: the 
ability to quote a data object. Suppose we want to construct the list (a b). We 
can't accomplish this with (list a b), because this expression constructs a list
of the values of a and b rather than the symbols themselves. This issue is well 
known in the context of natural languages, where words and sentences may be 
regarded either as semantic entities or as character strings (syntactic 
entities).

We can follow this same practice to identify lists and symbols that are to be 
treated as data objects rather than as expressions to be evaluated. Thus, the 
meaning of the single quote character is to quote the next object. 

Now we can distinguish between symbols and their values: |#
(defvar a 1)
(defvar b 2)

(list a b)   #|-->|# (1 2)
(list 'a 'b) #|-->|# (a b)
(list 'a b)  #|-->|# (a 2)

; Quotation also allows us to type in compound objects, using the conventional 
;printed representation for lists:
(car '(a b c)) #|-->|# a
(cdr '(a b c)) #|-->|# (b c) ; Able to perform list operations on quoted list


; = Set Theory =

#| Using EQL, we can implement a useful procedure called MEMQ. This takes two 
arguments, a symbol and a list. If the symbol is not contained in the list 
 (i.e., is not eq? to any item in the list), then memq returns false. Otherwise,
it returns the sublist of the list beginning with the first occurrence of the 
symbol: |#
(defun memq (item x)
  (cond ((null x) nil)
        ((eql item (car x)) x)
        (t (memq item (cdr x)))))

(memq 'apple '(pear banana prune))            #|-->|# NIL
(memq 'apple '(x (apple sauce) y apple pear)) #|-->|# (APPLE PEAR)
#| Note that the returned list does not begin with (APPLE SAUCE) as that is a 
list, not a symbol.  This implementation of MEMQ is not recursive. |#

(list 'a 'b 'c) #|-->|# (A B C)
(list (list 'george)) #|-->|# ((GEORGE))
(cdr '((x1 x2) (y1 y2))) #|-->|# ((Y1 Y2))
(cadr '((x1 x2) (y1 y2))) #|-->|# (Y1 Y2)
(consp (car '(a short list))) #|-->|# NIL
(memq 'red '((red shoes) (blue socks))) #|-->|# NIL
(memq 'red '(red shoes blue socks)) #|-->|# (RED SHOES BLUE SOCKS)

#| EQUAL? returns T if two lists have the same symbols, in the same order
 (recursive): |# ; luk@ SYMBOLP
(defun equal? (la lb)
  (cond
    ((and (symbolp la) (symbolp lb))
      (eql la lb))
    ((symbolp la) (symbolp lb))
    ((symbolp lb) (symbolp la))
    ((null la) (null lb))
    ((null lb) (null la))
    (t (and
        (equal? (car la) (car lb))
        (equal? (cdr la) (cdr lb))))))

; Dremonstration that '(foo) is the same as (quote (foo))
(car ''abracadabra) #|-->|# QUOTE


; = Symbolic Differentiation =
#| Consider the design of a procedure that performs symbolic differentiation of 
algebraic expressions. We would like the procedure to take as arguments an 
algebraic expression and a variable and to return the derivative of the 
expression with respect to the variable. For example, if the arguments to the 
procedure are ax**2 + bx + c and x, the procedure should return 2ax + b.

In order to keep things simple, we will consider a very simple 
symbolic-differentiation program that handles expressions that are built up 
using only the operations of addition and multiplication with two arguments. 
Differentiation of any such expression can be carried out by applying the 
following reduction rules: |#
; dc/dx = 0, for c = a constant or a variable different from x
; dx/dx = 1
; d(u + v)/dx = du/dx + dv/dx
; d(uv)/dx = u(dv/dx) + v(du/dx)
#| Observe that the latter two rules are recursive in nature. That is, to obtain
the derivative of a sum we first find the derivatives of the terms and add them.
Each of the terms may in turn be an expression that needs to be decomposed. |#

(defun variable? (x)
  "Is X a variable?"
  (symbolp x))

(defun same-variable? (v1 v2)
  "Are v1 and v2 the same variable?"
  (and
    (variable? v1)
    (variable? v2)
    (eql v1 v2)))

(defun make-sum (a1 a2)
  "Construct form to compute the sum of a1 and a2."
  (list '+ a1 a2))

(defun make-product (m1 m2)
  "Construct form to compute the product of m1 and m2."
  (list '* m1 m2))

(defun sum? (x)
  "Is X a sum?"
  (and (consp x) (eql (car x) '+)))

(defun addend (s)
  "Addend of the sum S"
  (cadr s))

(defun augend (s)
  "Augend of the sum S"
  (caddr s))

(defun product? (x)
  "Is X a product?"
  (and (consp x) (eql (car x) '*)))

(defun multiplier (s)
  "Multiplier of the product S"
  (cadr s))

(defun multiplicand (s)
  "Multiplicand of the product S"
  (caddr s))

#| If we had a means for representing algebraic expressions, we should be able 
to tell whether an expression is a sum, a product, a constant, or a variable. We
should be able to extract the parts of an expression. For a sum, for example we 
want to be able to extract the addend (first term) and the augend (second term).
We should also be able to construct expressions from parts. 

Using these, and the primitive predicate numberp, which identifies numbers, we 
can express the differentiation rules as the following procedure:|#

(defun deriv (expr var)
  "Return the derivative of EXPR with respect to VAR"
  (cond ((numberp expr) 0)
        ((variable? expr)
          (if (same-variable? expr var) 1 0))
        ((sum? expr)
          (make-sum (deriv (addend expr) var)
                    (deriv (augend expr) var)))
        ((product? expr)
          (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr))))
        (t
          (error "unknown expression type -- DERIV ~A" expr))))

#| This deriv procedure incorporates the complete differentiation algorithm. 
Since it is expressed in terms of abstract data, it will work no matter how we 
choose to represent algebraic expressions, as long as we design a proper set of 
selectors and constructors. 

Our difficulty is much like the one we encountered with the rational-number 
implementation: we haven't reduced answers to simplest form. To accomplish the 
rational-number reduction, we needed to change only the constructors and the 
selectors of the implementation. We can adopt a similar strategy here. We won't 
change deriv at all. Instead, we will change make-sum so that if both summands 
are numbers, make-sum will add them and return their sum. Also, if one of the 
summands is 0, then make-sum will return the other summand. |#

(defun =number? (expr num)
  "Checks whether an expression EXPR is equal to a given number NUM"
  (and (numberp expr) (= expr num)))

(defun make-sum (a1 a2)
  "Construct a form to sum A1 and A2 while performing simplification"
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))

(defun make-product (m1 m2)
  "Construct a form to multiply M1 and M2, applying properties and simplifying"
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list '* m1 m2))))

#| This system can be further extended to cover exponents. 
The differentiation rule for exponents is as follows: 
d(u**n)/dx = nu**(n-1)(du/dx) |#

(defun make-exponentiation (base exp)
  "Return BASE raised to the EXP, explicitly or symbolically"
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (numberp base) (numberp exp))
          (expt base exp))
        (t (list '** base exp))))

(defun exponentiation? (x)
  "Is X an exponentiation?"
  (and (consp x) (eql (car x) '**)))

(defun base (s)
  "Return the base of an exponentiation S"
  (cadr s))

(defun exponent (s)
  "Return the exponent of an exponentiaion S"
  (caddr s))

(defun deriv (expr var)
  "Implementation of symbolic derivation now covers exponents"
  (cond ((numberp expr) 0)
        ((variable? expr)
          (if (same-variable? expr var) 1 0))
        ((exponentiation? expr)
          (make-product
            (make-product
              (exponent expr)
              (make-exponentiation
                      (base expr)
                      (1- (exponent expr))))
            (deriv (base expr) var)))
        ((sum? expr)
          (make-sum (deriv (addend expr) var)
                    (deriv (augend expr) var)))
        ((product? expr)
          (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr))))
        (t
          (error "unknown expression type -- DERIV ~A" expr))))

#| Extending the differentiation program to handle sums and products of 
arbitrary numbers of (two or more) terms -  |#

; - Extending the addition facility -

(defun make-sum (&rest nums)
  "Construct form to compute the sum of an arbitrary number of operands"
  (append (list '+) nums))
  #| CL's &rest argument passes all the arguments it receives as a list into the
  function. |#

; Therefore, it is possible to create sums as follows:
(make-sum 1 'x 4) #|-->|# '(+ 1 x 4)

; Necessary to redefine AUGEND
(defun augend (s)
  "Returns the sum of the rest of the terms"
  (let ((aug (cddr s)))
    (if (= (length aug) 1)
      (car aug)
      (append (list '+) aug))))

; - Extending multiplication facility -

(defun make-product (&rest nums)
  "Construct form to compute the product of an arbitrary number of operands"
  (append (list '*) nums))

; Necessary to redefine MULTIPLICAND
(defun multiplicand (s)
  "Returns the product of the rest of the terms"
  (let ((m (cddr s)))
    (if (= (length m) 1)
      (car m)
      (append (list '*) m))))

#| For a parser to regular algebra infix notation, see:
http://eli.thegreenplace.net/2007/08/30/sicp-sections-231-232/#fn2 |#

#| Symbolic differentiation is of special historical significance in Lisp. It 
was one of the motivating examples behind the development of a computer language
for symbol manipulation. Furthermore, it marked the beginning of the line of 
research that led to the development of powerful systems for symbolic 
mathematical work, which are currently being used by a growing number of applied
mathematicians and physicists. |#


; = Representing Sets =
#| Informally, a set is simply a collection of distinct objects. To give a more 
precise definition we can employ the method of data abstraction. That is, we 
define "set" by specifying the operations that are to be used on sets. These are
union-set, intersection-set, element-of-set?, and adjoin-set. Element-of-set? is
a predicate that determines whether a given element is a member of a set. 
Adjoin-set takes an object and a set as arguments and returns a set that 
contains the elements of the original set and also the adjoined element. 
Union-set computes the union of two sets, which is the set containing each 
element that appears in either argument. Intersection-set computes the 
intersection of two sets, which is the set containing only elements that appear 
in both arguments. |#  

;  - Sets as Unordered Lists -
#| One way to represent a set is as a list of its elements in which no element 
appears more than once. The empty set is represented by the empty list. |#

(defun element-of-set? (x set)
  "Determine if X is an element of SET"
  (cond ((null set) nil)
        ((equalp x (car set)) t)
        (t (element-of-set? x (cdr set)))))

#| Using this, we can write adjoin-set. If the object to be adjoined is already 
in the set, we just return the set. Otherwise, we use cons to add the object to 
the list that represents the set: |#
(defun adjoin-set (x set)
  "Join X to SET if not already present, otherwise do nothing"
  (if (element-of-set? x set)
      set
      (cons x set)))

#| For intersection-set we can use a recursive strategy. If we know how to form 
the intersection of set2 and the cdr of set1, we only need to decide whether to 
include the car of set1 in this. But this depends on whether (car set1) is also 
in set2. Here is the resulting procedure: |#
(defun intersection-set (set1 set2)
  "Return a set of items common to both SET1 and SET2"
  (cond ((or (null set1) (null set2)) nil)
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))

#| In designing a representation, one of the issues we should be concerned with 
is efficiency. Since the above all use element-of-set?, the speed of this 
operation has a major impact on the efficiency of the set implementation as a 
whole. Now, in order to check whether an object is a member of a set, 
element-of-set? may have to scan the entire set. (In the worst case, the object 
turns out not to be in the set.) Hence, if the set has n elements, 
element-of-set? might take up to n steps. Thus, the number of steps required 
grows as O(n). The number of steps required by adjoin-set, which uses this 
operation, also grows as O(n). For intersection-set, which does an 
element-of-set? check for each element of set1, the number of steps required 
grows as the product of the sizes of the sets involved, or O(n**2) for two sets 
of size n. The same will be true of union-set. 

The union of two sets is the combination of elements from both sets, without 
duplicates. For sets set1 and set2, we will walk over the elements of set2, and 
keep only those that aren’t already members of set1. The remaining elements will
be appended to set1. |#
(defun union-set (set1 set2)
  "Return a set containing all the items in SET1 and SET2 with no repeats"
  (append
    set1
    (remove-if
      (lambda (x)
        (element-of-set? x set1))
      set2)))

; - Sets as Ordered Lists -
#| One way to speed up our set operations is to change the representation so 
that the set elements are listed in increasing order. To do this, we need some 
way to compare two objects so that we can say which is bigger. For example, we 
could compare symbols lexicographically, or we could agree on some method for 
assigning a unique number to an object and then compare the elements by 
comparing the corresponding numbers. To keep our discussion simple, we will 
consider only the case where the set elements are numbers, so that we can 
compare elements using > and <. We will represent a set of numbers by listing 
its elements in increasing order.  

One advantage of ordering shows up in element-of-set?: In checking for the 
presence of an item, we no longer have to scan the entire set. If we reach a set
element that is larger than the item we are looking for, then we know that the 
item is not in the set: |#
(defun element-of-set? (x set)
  "Determine if number X is a member of ordered SET"
  (cond ((null set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-set? x (cdr set)))))

#| On the average we should expect to have to examine about half of the items in
the set. Thus, the average number of steps required will be about n/2. This is 
still O(n) growth, but it does save us, on the average, a factor of 2 in number 
of steps over the previous implementation. 

We obtain a more impressive speedup with intersection-set. Begin by comparing 
the initial elements, x1 and x2, of the two sets. If x1 equals x2, then that 
gives an element of the intersection, and the rest of the intersection is the 
intersection of the cdrs of the two sets. Suppose, however, that x1 is less than
x2. Since x2 is the smallest element in set2, we can immediately conclude that 
x1 cannot appear anywhere in set2 and hence is not in the intersection. Hence, 
the intersection is equal to the intersection of set2 with the cdr of set1. 
Similarly, if x2 is less than x1, then the intersection is given by the 
intersection of set1 with the cdr of set2. Here is the procedure: |#
(defun intersection-set (set1 set2)
  (if (or (null set1) (null set2))
      nil    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

#| To estimate the number of steps required by this process, observe that at 
each step we reduce the intersection problem to computing intersections of 
smaller sets -- removing the first element from set1 or set2 or both. Thus, the 
number of steps required is at most the sum of the sizes of set1 and set2, 
rather than the product of the sizes as with the unordered representation. This 
is O(n) growth rather than O(n**2) -- a considerable speedup, even for sets of 
moderate size. |#

; Implementation of adjoin-set using the ordered representation:
(defun adjoin-set (x set)
  "Adds X to a SET of ordered elements"
  (cond ((null set) (cons x '()))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (t (cons
            (car set)
            (adjoin-set x (cdr set))))))
#| Take advantage of the ordering to produce a procedure that requires on the 
average about half as many steps as with the unordered representation. |#

; Implementation of union-set for sets represented as ordered lists
(defun union-set (set1 set2)
  "Return ordered set that is the union of SET1 and SET2"
  (let ((x1 (car set1)) (x2 (car set2)))
    (cond ((null x1) set2)
          ((null x2) set1)
          ((= x1 x2)
            (cons x1 (union-set (cdr set1) (cdr set2))))
          ((< x1 x2)
            (cons x1 (union-set (cdr set1) set2)))
          (t
            (cons x2 (union-set set1 (cdr set2)))))))