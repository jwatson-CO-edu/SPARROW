#|
little-schemer-04.scm
James Watson, 2013 September 
Chapters 8 and 9 of "The Little Schemer" by Friedman and Felleisen
|#
(load "little-schemer-03.scm") ; This will also load 02 and 01

; == Chapter 8 : Lambda the Ultimate ==
(displayln "Chapter 8")

(define rember-f ; Implement rember using an equality test test? passed as an argument
  (lambda (test? a l)
    (cond ((null? l) (quote ())) ; list end or list null, return null
          ((test? (car l) a) (cdr l)) ; else skip item, return sublist
          (else (cons (car l) (rember-f test? a (cdr l))))))); else cons item onto recur sublist

(displayln ; What is the result?
 (rember-f = 5 '(6 2 5 3) ) ; (6 2 3)
 )

(displayln ; What is the result?
 (rember-f eq? 'jelly '(jelly beans are good) ) ; (beans are good)
 )

(displayln ; What is the result?
 (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)) ) ; (lemonade and (cake))
 )

(define eq?-c ; return a function that tests if 'x' is equal to the argument
  (lambda (a) 
    (lambda (x)
      (eq? x a))))

(displayln ; What is the result?
 (eq?-c 'salad) ; #<procedure:...e-schemer-04.scm:31:4>
 )

(define eq?-salad (eq?-c 'salad)) ; define a function that tests if 'x' is equal to 'salad

(displayln ; What is the result?
 (eq?-salad 'salad) ; #t
 )

(displayln ; What is the result?
 (eq?-salad 'tuna) ; #f
 )

(define rember-f ; return a function that implements rember using the test provided
  (lambda (test?) ; this form is the function that defines a function
    (lambda (a l) ; this form is the function returned
      (cond ((null? l) (quote ())) ; list end or list null, return null
            ((test? (car l) a) (cdr l)) ; test? match, skip item and return sublist
            (else (cons (car l) ; else, cons item onto recur on test? / a / sublist
                        ((rember-f test?) a (cdr l))))))))

(displayln ; What is the result?
 ; In order to obtain function to apply, evaluate (rember-f eq?)
 ((rember-f eq?) 'tuna '(shrimp salad and tuna salad) ) ; (shrimp salad and salad)
 )

(displayln ; What is the result?
 ;          v-- passed symbol 'eq? to rember-f as test function name
 ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?) )
 ;                ^-- passed symbol 'eq? to generated function as member to remove
 ) ; (equal? eqan? eqlist? eqpair?)

(define insertL-f ; Returns a function that implements insertL using equality test?
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) (quote ())) ; list end or list null, return null
            ;test match, cons new onto cons old onto sublist
            ((test? (car l) old) (cons new (cons old (cdr l))))
            (else (cons (car l) ; else, cons item onto recur on test?/new/old/sublist
                        ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) (quote ())) ; list end or list null, return null
            ; test match, cons old onto cons new onto sublist
            ((test? (car l) old) (cons old (cons new (cdr l))))
            (else (cons (car l) ; else, cons item onto recur on test?/new/old/sublist
                        ((insertR-f test?) new old (cdr l))))))))

; abstract out the parts that make insertL/R unique
(define seqL (lambda (new old l) (cons new (cons old l))))
(define seqR (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) (quote ())) ; list end or list null, return null
            ((eq? (car l) old) (seq new old (cdr l))) ; match, return correct sequence
            (else (cons (car l) ; else, cons item onto recur on seq/new/old/sublist
                        ((insert-g seq) new old (cdr l))))))))

; And we can see that insertL and insertR are just variations on insert-g
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
; but we didn't have to create a new name, we can pass the definition
(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
;                         ^-- definitions of sequence generator

(define seqS (lambda (new old l) (cons new l))) ; sequence generator for substitution
(define subst (insert-g seqS))

(define seqrem (lambda (new old l) l)) ; requence generator for removing 'old'
(define rember (lambda (a l) ((insert-g seqrem) #f a l)))

(displayln ; What is the result?
 (rember 'sausage '(pizza with sausage and bacon) ) ; (pizza with and bacon)
 )

#| ** The Ninth Commandment **
Abstract common patterns with a new function. |#

; = Re-implement the arithmetic DSL with similar abstraction? =
(define atom-to-function ; Return a function based on x
  (lambda (x)
    (cond ((eq? x (quote +)) o+)
          ((eq? x (quote *)) o*)
          (else o^))))

(displayln ; What is the result?
 (atom-to-function (operator '(+ 5 3))) ; #<procedure:o+>, that we defined before
 )

(displayln ; What is the result?
 ((atom-to-function (operator '(+ 5 3))) 5 3) ; 8, returned function applied to args
 )

(define value ; Evaluate a prefix arithmetic expression
  (lambda (nexp)
    (cond ((atom? nexp) nexp) ; item is atom, return item
          ; else, determine function to apply from operator and apply to sub-expressions
          (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
                                                    (value (2nd-sub-exp nexp)))))))

(displayln ; What is the value of this expression?
 (value '(+ (* 5 (^ 2 3)) 2) ) ; 42
 )
; = End Re-Implement Prefix =

(define multirember-f ; Return an implementation of multirember based on test?
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) (quote ())) ; list end or list null, return null
            ((test? a (car lat)) ((multirember-f test?) a (cdr lat))); match,skip,recur
            ; else, cons item to recur on a / sublist lat
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(displayln ; What is the result?
 ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna) )
 ) ; (shrimp salad salad and)

(define multirember-eq? (multirember-f eq?))
(define eq?-tuna (eq?-c (quote tuna)))

(define multiremberT ; Return a version of lat with all items that pass test? removed
  (lambda (test? lat)
    (cond ((null? lat) (quote ())) ; list end or list null, return null
          ((test? (car lat)) (multiremberT test? (cdr lat))) ; test match, skip, recur sublist
          (else (cons (car lat) (multiremberT test? (cdr lat))))))) ;else cons item to recur sub

(define multirember&co ; SOMETHING
  (lambda (a lat col)
          ; if null? lat, call collector on '() '()
    (cond ((null? lat) (col (quote ()) (quote ()))) 
          ((eq? (car lat) a) ; match, ...
           (multirember&co a (cdr lat) ; recur on a / sublist lat / new collector
                           (lambda (newlat seen) ; create new collector that ...
                             ; applies previous collector to
                             (col newlat ; first collector arg and ...
                                  (cons (car lat) seen))))) ; cons item onto 2nd arg
          ; else, not a match, recur on a / sublist lat / new collector
          (else (multirember&co a (cdr lat)
                                (lambda (newlat seen) ; create new collector that
                                  ; calls previous collector on 
                                  (col (cons (car lat) newlat) ; cons item onto 1st arg
                                       seen))))))) ; and 2nd arg
#| In other words, examine every atom of lat to determine if eq? to a. Non-matching atoms are
added to l1, while matching atoms are added to l2. Now apply col, (col l1 l2) |#

(define a-friend (lambda (x y) (null? y))) ; return true if y is null, otherwise false, ignore x

(displayln ; What is the result?
 (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend) ; #f
 )

(displayln ; What is the result?
 (multirember&co 'tuna '() a-friend) ; #t
 )

(displayln ; What is the result?
 (multirember&co 'tuna '(tuna) a-friend) ; #f
 )

(displayln ; What is the result?
 (multirember&co 'tuna '(and tuna) a-friend) ; #f
 )

(define last-friend (lambda (x y) (olength x)))

(displayln ; What is the result?
 (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend) ; 3
 )

#| ** The Tenth Commandment ** 
Build functions to collect more than one value at a time. |#

(define multiinsertLR ; return lat with new inserted to the left of oldL and right of oldR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) (quote ())) ; list end or list null, return null
          ((eq? (car lat) oldL) ; oldL match
           (cons new (cons oldL ; cons new onto cons oldL onto recur sublist
                           (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR) ; oldR match
           (cons oldR (cons new ; cons oldR onto cons new onto recur sublist
                            (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat) ; else, cons item onto recur sublist
                      (multiinsertLR new oldL oldR (cdr lat)))))))

#| - multiinsertLR&co -
apply (col newlat L R), where
newlat : lat with new inserted to the left of oldL and right of oldR
L      : count of occurrences of oldL
R      : count of occurrences of oldR |#
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col (quote ()) 0 0)) ;  list null, apply col to null / 0 / 0
          ((eq? (car lat) oldL) ; match oldL
           (multiinsertLR&co new oldL oldR (cdr lat) ; recur on sublist and new collector that
                             (lambda (newlat L R)
                               (col (cons new (cons oldL newlat)) ; inserts new to the left 
                                    (add1 L) ; adds 1 to the oldL count
                                    R)))) ; does not change the oldR count
          ((eq? (car lat) oldR) ; match oldR
           (multiinsertLR&co new oldL oldR (cdr lat) ; recur on sublist and new collector that
                             (lambda (newlat L R)
                               (col (cons oldR (cons new newlat)) ; inserts new to the right
                                    L ; does not change oldL count
                                    (add1 R))))) ; adds 1 to the oldR count
          (else (multiinsertLR&co new oldL oldR (cdr lat) ; else no match, recur on sublist
                                  (lambda (newlat L R) ; and a new collector that
                                    (col (cons (car lat) newlat) ; cons item onto list arg
                                         L ; does not change oldL count
                                         R))))))) ; does not change oldR count

(define list-only-col (lambda (lat L R) lat)) ; collector, return list and discard other args

(displayln ; What is the result?
 (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list-only-col)
 ) ; (chips salty and salty fish or salty fish and chips salty)

(define oeven? ; return true if integer division of n by 2 times 2 is equal to n (even)
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define evens-only* ; Remove all odd integers from l, recursively, and return the result
  (lambda (l)
    (cond ((null? l) (quote ())) ; List end or list null, return null
          ((atom? (car l)) ; item is an atom
           (cond ((oeven? (car l)) (cons (car l) (evens-only* (cdr l))));even,cons item to recur
                                 (else (evens-only* (cdr l))))) ; else recur on sublist
          ; item is not an atom
          (else (cons (evens-only* (car l)) ; cons recur on item (a list) onto ...
                      (evens-only* (cdr l))))))) ; recur on sublist

#| - evens-only*&co - 
apply (col newl p s) with
newl : l with all odd numbers removed, recursively
p    : product of the even numbers
s    : sum of the odd numbers |#
(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col (quote ()) 1 0)) ; null, apply col to null / mult ident / add ident
          ((atom? (car l)) ; item is an atom
           (cond ((oeven? (car l)) ; atom is even
                  (evens-only*&co (cdr l) ; recur on sublist / new collector that ...
                                  (lambda (newl p s)
                                    (col (cons (car l) newl) ; cons item onto list arg
                                         (o* (car l) p) ; multiplies item by product arg
                                         s)))) ; leaves sum arg unchanged
                 (else ; else, atom is not even
                  (evens-only*&co (cdr l) ; recur on sublist / new collector that ...
                                  (lambda (newl p s)
                                    (col newl ; leaves list arg unchanged (skip item)
                                         p ; leaves product arg unchanged
                                         (o+ (car l) s))))))) ; adds item to sum arg
          ; else, item is not atom
          (else (evens-only*&co (car l) ; recur on item (list) / new collector that
                                (lambda (al ap as)
                                  (evens-only*&co (cdr l) ; recur on sublist / new collector ...
                                                  (lambda (dl dp ds)
                                                    (col (cons al dl) ; cons list args
                                                         (o* ap dp) ; multiply product args
                                                         (o+ as ds)))))))))) ; add sum args

(define the-last-friend ; collector, makes a list of the (sum product (list-of-evens))
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(displayln ; Whats is the result?
 (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
 ) ; (38 1920 (2 8) 10 (() 6) 2)

; == Chapter 9 : ... and Again, and Again, and Again, ... ==
(displayln "Chapter 9")

(define looking ; call keep-looking on goal / lat item at index 1 / lat
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking ; keep-looking if sorn is number, otherwise return true if sorn is goal
  (lambda (a sorn lat)
    ;     sorn is number, recur on goal / lat item at index sorn / lat 
    (cond ((number? sorn) (keep-looking a (pick sorn lat) lat)) 
          (else (eq? sorn a))))) ; else, sorn not number, compare sorn to goal

(displayln ; What is the result?
 (looking 'caviar '(6 2 4 caviar 5 7 3)) ; #t
 )

(displayln ; What is the result?
 (looking 'caviar '(6 2 grits caviar 5 7 3)) ; #f
 )

(displayln ; What is the result?
 ;(looking 'caviar '(7 1 2 caviar 5 6 3))
 "looking at (7 1 2 caviar 5 6 3), keep-looking forever!"
 )

; Total functions must terminate, partial functions may never terminate

(define eternity (lambda (x) (eternity x))) ; let's never call this function, it will not stop!

#| - shift -
Takes a pair whose first item is a pair and builds a pair by shifting the second part of the 
first item into the second item. |#
(define shift
  (lambda (pair)
    ; build a new pair for which
    (build (first (first pair)) ; the first item is the first part of the first part of pair
           ; the second item is a new pair for which
           (build (second (first pair));first item is the second part of the first item of pair 
                  (second pair))))) ; the second item is the second part of pair

(define align
  (lambda (pora) ; pora, pair or atom
    (cond ((atom? pora) pora) ; pora is atom, return pora
          ((a-pair? (first pora)) (align (shift pora))) ;1st item is par, recur on (shift pora)
          ;else, 1st item not pair
          (else (build (first pora) ; build pair where first item is first item of pora , and
                       (align (second pora))))))) ; second item is recur on (second pora)
; argument is changed for recursive calls, but the change is not guaranteed to be closer to goal


(displayln ; What is the result?
 (shift '((a b) c) ) ; (a (b c))
 )

(displayln ; What is the result?
 (shift '((a b) (c d)) ) ; (a (b (c d)))
 )

(define length* ; Return the total number of atoms in nested LS pairs
  (lambda (pora)
    (cond ((atom? pora) 1) ; pora is atom, return 1 to be added to count
          ; else, assume pora is pair, add
          (else (o+ (length* (first pora)) ; recur on first item to
                    (length* (second pora))))))) ; recur on second item

(define weight* ; Return a measure of how 'front-heavy' a structure made of nested LS pairs is
  (lambda (pora) ; pora, (LS) pair or atom
    (cond ((atom? pora) 1) ; arg is atom, has weight 1
          (else (o+ (o* (weight* (first pora)) 2) ; else, sum twice recur on (first pora) and
                    (weight* (second pora))))))) ; recur on (second pora)

(define shuffle ; swaps the components of an LS pair when the first component is an LS pair
 (lambda (pora)
   (cond ((atom? pora) pora) ; pora is atom, return pora
         ; assume pora is LS pair, recur on a reversed pora
         ((a-pair? (first pora)) (shuffle (revpair pora))) ; if both components pairs, won't end
         (else (build (first pora) ; else first component atom, build a pair out of first pora
                      (shuffle (second pora))))))) ; and recur on second pora
; therefore shuffle is not a total function

(displayln ; What is the result?
 ;(shuffle '((a b) (c d)) )
 "(shuffle '((a b) (c d)) ), swaps the components indefinitely, both components are LS pairs"
 )

(define C ; test the Collatz conjecture for an integer n, n > 0
  (lambda (n)
    (cond ((one? n) 1) ; n is 1, sequence over, return 1
          (else (cond ((oeven? n) (C (o/ n 2))) ; n is even, recur on n/2
                      (else (C (add1 (o* 3 n))))))))) ; n is odd, recur on 3*n+1
; It is not known if this functions is total, whether every integer > 0 reaches 1

(define A ; Evaluate the Ackermann-Peter function for n and m
  (lambda (n m)
    (cond ((zero? n) (add1 m)) ; 
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))
; function is total, but may take a very, very long time to compute

; = Applicative-Order Y Combinator Rationale and Development =
; URL: http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html

(define fact ; return the factorial of n
  (lambda (n)
    (cond ((zero? n) 1)
          (else (* n (fact (- n 1)))))))

(displayln (fact 4))

; Is there a way to write an recursive, anonymous procedure? Factorial function without a name?

(define fact-maker
  (lambda (procedure) ; first lambda passes the name of the operation
    (lambda (n)
      (cond ((zero? n) 1)
            (else (* n (procedure (- n 1))))))))

#| The idea will be to pass "fact-maker" in through "procedure". Thus, what we would like to do 
is invoke (fact-maker fact-maker) to produce our nameless (well, almost nameless) factorial 
procedure. This would allow us to write, for example: |#

(displayln "((fact-maker fact-maker) 4), results in error")

#| But, this doesn't work because "fact-maker" is a procedure which takes as input one argument 
that is a procedure but "procedure", which is supposed to be identical to "fact", requires a 
numeric argument. The solution is the following: |#

(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (cond ((zero? n) 1)
            (else (* n ((procedure procedure) (- n 1))))))))

(displayln ((fact-maker fact-maker) 4) )

#| Well, we got the name out of the body of the procedure but we still have to pass the 
procedure in and so far we have been using a name to do that. So let's try to get the whole 
dependence on a name out. |#

(define fact
  ((lambda (procedure) ; return a function that ...
     (lambda (n) ; takes an argument 'n'
       (cond ((zero? n) 1) ; if 'n' is zero, return 1
             ; else, multiply 'n' by the application of the result of applying 'procedure' to
             (else (* n ((procedure procedure) (- n 1)))))));'procedure' to n-1
   (lambda (procedure);return a function that will be passed to the returned function above, that
     (lambda (n) ; takes an argument 'n'
       (cond ((zero? n) 1) ; if 'n' is zero, return 1
             ; else, multiply 'n' by the application of the result of applying 'procedure' to
             (else (* n ((procedure procedure) (- n 1)))))))));'procedure' to n-1

(displayln (fact 4) ) ; 24

#| The following produces the factorial of 4 because the procedure which is invoked 
(the huge mess) is exactly the definition of "fact." But, lo and behold, there is no name for 
this procedure anywhere! |#

(displayln
 (((lambda (procedure) ; return a function that ...
     (lambda (n) ; takes an argument 'n'
       (cond ((zero? n) 1) ; if 'n' is zero, return 1
             ; else, multiply 'n' by the application of the result of applying 'procedure' to
             (else (* n ((procedure procedure) (- n 1)))))));'procedure' to n-1
   (lambda (procedure);return a function that will be passed to the returned function above, that
     (lambda (n) ; takes an argument 'n'
       (cond ((zero? n) 1) ; if 'n' is zero, return 1
             (else (* n ((procedure procedure) (- n 1))))))));'procedure' to n-1
  4)
) ; 24
#| There are two outer 'lambda' expressions above, each enclosing an inner lambda expression.
The second outer lambda is passed as an argument to the first outer lambda. The first outer
lambda creates a closure that stores the second outer lambda in the variable procedure. The first
outer lambda returns a function that takes 'n' as an argument. In the above, the returned inner
lambda is passed the variable 4. 4 is not 0, therefore the 'else' conditional branch is 
executed.  This branch multiplies 4 by the result of applying the function returned by 
applying the previously stored procedure to procedure.  The function thus returned is exactly
like the function that called it: a closure that performs one iteration, then recurses using
the stored 'procedure'.  Each recursion is a closure that stores the definition of the next
recursion, that passes that definition to the next recursion.  This continues until the base
case is met.|#

#| In what follows, we try to generalize this to all procedures and wind up with the dreaded 
applicative-order Y-combinator. 

First, we need to separate out the part that pertains to computing the factorial. The goal is to
write this part in one place and when code for other problems is substituted for the factorial
code, the result will be a new recursive procedure. |#

(define F
  (lambda (n)
    (cond ((zero? n) 1)
          (else (* n ((procedure procedure) (- n 1))))))); but where does 'procedure' come from?

#| The above isn't quite what we want because it contains '(prcoedure procedure)' where we 
would like to see a plain old procedure. In order to fix this, a curious substitution must be 
made, which is based on the idea that - |#

; (func arg)
; is identical to
; ((lambda (x) (func x)) arg)

#| Obviously, the lambda expression does nothing except call 'func' on its own arg, but the fact
that these are equivalent bears on our goal. Based on the above - |#

; ((procedure procedure) (- n 1))
; is identical to
; ((lambda (x) ((procedure procedure) x)) (- n 1))

; Therefore
(define F
  (lambda (n)
    (cond ((zero? n) 1)
          (else (* n ((lambda (x) ((procedure procedure) x)) (- n 1)))))))

#| Remembering that procedures can be passed as arguments, consider the following: |#
(define F
  ((lambda (func-arg) ; creating a closure with a function stored in func-arg
     (lambda (n) ; a function that takes arg 'n'
       (cond ((zero? n) 1)
             (else (* n (func-arg (- n 1)))))))
   (lambda (x) ((procedure procedure) x))))
    
; Combining what we have done so far - 
(define fact
  ((lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (cond ((zero? n) 1)
                (else (* n (func-arg (- n 1)))))))
      (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     ((lambda (func-arg)
        (lambda (n)
          (cond ((zero? n) 1)
                (else (* n (func-arg (- n 1)))))))
      (lambda (arg) ((procedure procedure) arg))))))
#|  There are two lambda expressions above that each contain two additional lambda expressions. 
The first of these two 2nd level lambdas itself contains a lambda at a third level. For the sake
of explanation the outermost will be called lambda-procedure, the middle will be called 
lambda-func-arg and lambda-arg, and the innermost lambda-n.
    The second lambda-procedure is passed as an argument to the first lambda-procedure. The first
lambda-procedure stores the second lambda-procedure in a variable named 'procedure', which 
creates a closure. The first lambda-arg is passed to the first lambda-func-arg as an argument.
The first lambda-func-arg store the first lambda-arg in a variable named 'func-arg', which 
creates a closure. The first lambda-n is the function returned and stored in 'fact'. It is inside
two closures. 
    When the recursive case of lambda-n is encountered, func-arg (the first lambda-arg) is called
on (- n 1). lambda-arg applies the result of calling 'procedure' on 'procedure' to the argument.
The result of calling 'procedure' on 'procedure' is the another lambda-n that is contained in two
closures identical to the above. The closures provide enough information for this construction to
happen for the next recursive step. That construction will contain the information needed for the
next. |#

#| The two expressions beginning with '(lambda (func-arg) ...' are exactly the pieces of code
that correspond to the factorial code, and they are in exactly the right form. We can get them
out of the definition in the following way: |#

(define F*
  (lambda (func-arg)
    (lambda (n)
      (cond ((zero? n) 1)
            (else (* n (func-arg (- n 1))))))))

(define fact
  ((lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     (F* (lambda (arg) ((procedure procedure) arg))))))
#|  F* is function that returns a closure with a function stored in 'func-arg'. The closure 
contains a function that computes the factorial of 'n'. 'func-arg' will be called on the next 
iteration argument, and it accomplishes the recursive step.
    'fact' is defined to be a lambda expression that takes another lambda expression as an 
argument. The first outer lambda expression is a closure that stores the second in a variable
named 'procedure'. The first outer lambda expression calls 'F*', passing it an inner lambda 
expression that takes an argument, and applies the result of calling 'procedure' on 'procedure'
to the argument. This result creates a new closure that holds all the information required to
construct the next nested closure, as was seen above. |#

(displayln (fact 4) ) ; 24

(define Y*
  (lambda (X)
    ((lambda (procedure) (X (lambda (arg) ((procedure procedure) arg))))
     (lambda (procedure) (X (lambda (arg) ((procedure procedure) arg)))))))
#| Notice that the procedure which does our computation is X and that is passed in as an 
argument. 

We can write "fact" in terms of the Y-combinator as follows: |#
(define fact (Y* F*))

(displayln (fact 4) ) ; 24
    
; = End Y Combinator Rationale =

(define Y ; Applicative-order Y combinator
  (lambda (le)
    ((lambda (f) (f f)) ; eliminated a layer of 'le', but the same effect is accomplished
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(define fact (Y F*)) ; This works as well as the above.

(displayln (fact 4) ) ; 24

#| In order the further demonstrate the Y combinator, we define an anonymous, recursive function
that returns the maximum item in a list of numbers. |#
(displayln
 ((Y (lambda (func-arg) ; the function passed to Y must create a closure that applies the 
       (lambda (l) ;      enclosed function to the recursive argument
         (cond ((null? l) 'no-list)
               ((null? (cdr l)) (car l))
               (else (let ((rest (func-arg (cdr l))))
                       (cond ((> (car l) rest) (car l))
                             (else rest))))))))
  '(4 5 6 3 4 8 6 2))
) ; 8