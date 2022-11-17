#|
seasoned-schemer-08.scm
James Watson, 2014 March 
Chapter 17 of "The Seasoned Schemer" by Friedman and Felleisen
|#

#| == TODO == 
* Understand what a continuation really is
* Understand what Call with Current Continuation means.
* What the heck is Chapter 18 about? Examples don't seem to work
* Find out why the last version of (supercounter deepM) does not get same result as book. 
  - Does a new definition containing 'consC' somehow change the closure? 
* Sort out the confusing nested 'letcc's near the end of Chapter 19.|#

(load "seasoned-schemer-07.scm") ; This will also load all previous

; == Chapter 17 : We Change, Therefore We Are! ==
(displayln "Chapter 17")

; redefine using 'if'
(define deep
  (lambda (m) ; arg desired depth 'm'
    (if (zero? m) ; if 'm' is 0
        'pizza ; then return symbol 'pizza
        (cons (deep (sub1 m)) ; else cons recur on m-1 to
              '())))) ; null


(define deepM ; redefined without 'deep' as a helper
  (let ((Rs '()) ; local 'Rs' stores previous results
        (Ns '())) ; local 'Ns' stores previous args passed to 'deepM'
    (letrec ((D (lambda (m) ; local func 'D' is the recursive part of 'deepM'
                  (if (zero? m) ; if arg zero
                      'pizza ; then, return pizza symbol
                      (cons (deepM (sub1 m)) ; else, cons recur on m-1 onto
                            '()))))) ; list terminator
      (lambda (n) ; top arg 'n' is the list depth we desire
        (let ((exists (find n Ns Rs))) ; local 'exists' holds lookup result or error symbol
          (if (atom? exists) ; if we stored the error symbol in 'exists'
              (let ((result (D n))) ; then, local 'result' holds call 'D' on top arg 'n'
                (set! Rs (cons result Rs)) ; add result to stored results
                (set! Ns (cons n Ns)) ; add arg to stored args
                result) ; return the result
              exists)))))) ; else, previous located (non-atom), return it

(displayln (deepM 7) ) ; (((((((pizza)))))))

#| In the above, we could have used 'let' in place of 'letrec' because the name 'D' does not
appear in its own definition. We could then put all the assingnments in one 'let' expression
because the definition of 'D' includes neither 'Rs' nor 'Ns'. |#

(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m))
                       '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(displayln (deepM 7) ) ; (((((((pizza)))))))

; Now replacing the use of 'D' with the expression that it names
(define deepM
  (let ((Rs '()) ; local, previous results
        (Ns '())) ; local, previous args
    (lambda (n) ; the function assigned to 'deepM', arg is desired depth
      (let ((exists (find n Ns Rs))) ; local 'exists' is result of search of previous results
        (if (atom? exists) ; if the failure atom was assigned to 'exists'
            (let ((result ((lambda (m) ; then, local 'result' is assigned call function that 
                            (if (zero? m) ; takes arg 'm', if 'm' is zero
                                'pizza ; return pizza symbols
                                (cons (deepM (sub1 m)) ; else, cons recur on m-1 onto
                                      '()))) ; the list terminus
                           n))) ; the above function is called on 'n', assigned to 'result'
              (set! Rs (cons result Rs)) ; cons new result onto list of results
              (set! Ns (cons n Ns)) ; cons new 'n' onto list of args
              result) ; return the result
            exists))))) ; else was not error atom, return successful lookup

(displayln (deepM 7) ) ; (((((((pizza)))))))

; 'consC' is a function that returns same value of cons, and counts how many times it sees args
(define consC
  (let ((N 0)) ; local 'N' holds number of conses, but what good does this do us outside func?
    (lambda (x y)
      (set! N (add1 N)) ; add 1 to 'N'
      (cons x y)))) ; cons 'x' onto 'y'

; see if we can use it in 'deep'
(define deep
  (lambda (m)
    (if (zero? m) ; if arg is zero
        'pizza ; return pizza symbol
        (consC (deep (sub1 m)) ; else cons recur on m-1 onto
               '())))) ; the list terminus

(define counter 0) ; R5RS will not let us leave 'counter' undefined, as it is in the book

(define consC
  (let ((N 0))
    (set! counter (lambda () N)) ; set counter to a function that takes no arg and returns 'N'
    (lambda (x y) ; Note: the above creates a closure
      (set! N (add1 N)) ; set 'N' to N+1
      (cons x y)))) ; cons 'x' onto 'y'

(displayln counter) ; #<procedure:counter>
(displayln (counter) ) ; 0
(displayln (deep 5) ) ; (((((pizza)))))
(displayln (counter) ) ; 5
(displayln (deep 7) ) ; (((((((pizza)))))))
(displayln (counter) ) ; 12

(define supercounter ; a function that tallies 'consC' for 'deep' with args 1000 to 0
  (lambda (f) ; top arg 'f' is a function to re
    (letrec ((S (lambda (n) ; local func 'S' takes arg 'n', the number of times to run 'f'
                  (if (zero? n) ; if arg is zero
                      (f n) ; call 'f' on 'n', this is the base case
                      (begin ; book had '(let ()', which achieves the same result
                        (f n) ; call 'f' on 'n'
                        (S (sub1 n))))))) ; recur on n-1
      (S 1000) ; run 'f' 1001 times
      (counter)))) ; return the result of calling counter

(displayln (supercounter deep) ) ; 500512, the 12 is left over from our previous investigation

; redefine 'consC' with some capability to reset counter and avoid leftovers
(define counter 0) ; will be redefined momentarily
(define set-counter 0) ; will be redefined momentarily
(define consC
  (let ((N 0)) ; set local counter 'N' to 0, funcs below need to be closures to reach it 
    (set! counter (lambda () N)) ; assign 'counter' a closure that returns the 'N'
    (set! set-counter (lambda (x) (set! N x))) ; 'set-counter' is a closure that SETS 'N'!
    (lambda (x y) 
      (set! N (add1 N)) ; set 'N' to N+1
      (cons x y)))) ; cons x onto y and return 

(displayln (set-counter 0) ) ; #<void>, but obviously there was a side effect, see below
(displayln (supercounter deep) ) ; 500500, this is now the result that we expected

; now we will modify 'deepM' to take advantage of 'consC' accounting
(define deepM
  (let ((Rs '()) ; local list of results found so far
        (Ns '())) ; local list of args so far
    (lambda (n)
      (let ((exists (find n Ns Rs))) ; attempt to find prev result and store in 'exists'
        (if (atom? exists) ; if 'exists' holds the error atom
            (let ((result (if (zero? n) ; then, 'result' is, if 'n' is zero
                              'pizza ; then, pizza symbol
                              (consC (deepM (sub1 n)) ; else, it is cons recur on n-1 onto 
                                     '())))) ; list terminus
              (set! Rs (cons result Rs)) ; cons result onto list of resukts
              (set! Ns (cons n Ns)) ; cons arg onto list of args
              result) ; return result
            exists))))) ; else was not error atom, assume correct structure found and return

(displayln (deepM 5) ) ; (((((pizza)))))
(displayln (counter) ) ; 500505, we appear to have leftovers again b/c we forgot 'set-counter'

(set-counter 0)

(displayln (deepM 5) ) ; (((((pizza)))))
(displayln (counter) ) ; 0, why?

(set-counter 0)
(displayln "(supercounter deepM), 995, takes a long time" ) ; 995, does not match book, WHY?

; a "safe" version of the last definition of 'rember1*'
(define rember1*C
  (lambda (a l) ; args: search item 'a' and list 'l'
    (letrec ((R (lambda (l oh) ; local func 'R' takes a list 'l' ans shortcut word 'oh'
                  (cond ((null? l) (oh 'no)) ; if 'l' null, shortcut with symbol 'no
                        ; else assume list, if item is atom ...
                        ((atom? (car l)) (if (eq? (car l) a) ; if item and 'a' equal
                                             (cdr l) ; then, skip item and return sublist
                                             (consC (car l) ; else, cons item onto
                                                    (R (cdr l) oh)))); recur on sublist/shortcut
                        (else (let ((new-car ; else item not atom, local new-car is
                                     (letcc oh ; declare shortcut word 'oh'
                                       (R (car l) oh)))) ; recur on item and shortcut word 'oh'
                                (if (atom? new-car) ; if 'new-car' is atom, must be 'no'
                                    (consC (car l); then search failed, return item conCsed onto
                                           (R (cdr l) oh)) ; recur on sublist/shortcut 'oh'
                                    (consC new-car; else returned list with 'a' removed, consC
                                           (cdr l))))))))) ; onto sublist and return
      (let ((new-l (letcc oh (R l oh)))) ; local 'new-l' the result setting up 'oh' shortcut,
        ; then calling 'R' on 'l' and the 'oh' shortcut
        (if (atom? new-l) ; if an atom was stored in 'new-l' assume the search failed
            l ; then return original list
            new-L))))) ; else, assume 'new-l' holds the modified list and return it

(set-counter 0) ; set counter to zero for a new investigation

(displayln (rember1*C 'noodles '((food) more (food))) ) ; ((food) more (food))
(displayln (counter) ) ; 0, never used 'conC', shortcuts cleared all pending 'consC'es

; a version without any shortcuts
(define rember1*C2
  (lambda (a l)
    (letrec ((R (lambda (l) ; local recursive func takes list arg 'l'
                  (cond ((null? l) '()) ; if 'l' is null, return null
                        ; else if item is atom
                        ((atom? (car l)) (if (eq? (car l) a) ; if item matches search
                                             (cdr l) ; then, skip item and return sublist
                                             (consC (car l) ; else, 'consC' item onto
                                                    (R (cdr l))))) ; recur on sublist
                        (else ; else, item was not atom
                         (let ((av (R (car l)))) ; local 'av' is result of calling 'R' on item
                           (if (eqlist? (car l) av) ; if this result is an unchanged list
                               (consC (car l) ; then 'consC' item onto
                                      (R (cdr l))) ; recur on sublist
                               (consC av ; else assume an 'a' was removed, 'consC' changed item
                                      (cdr l))))))))) ; onto sublist and return
      (R l)))) ; return the result of calling 'R' on top arg 'l'

(set-counter 0)

(displayln (consC (consC 'food '()) ; build the expected result manually for comparison's sake
                  (consC 'more
                         (consC (consC 'food '())
                                '()))) ) ; ((food) more (food))
(displayln (counter) ) ; 5. we used 5 'consC' operations

(set-counter 0) ; set counter 0 to discover what the 'consC' usage of 'rember1*C2' is
(displayln (rember1*C2 'noodles '((food) more (food))) ) ; ((food) more (food))
(displayln (counter) ) ; 5, 'rember1*C2' used 5 'consC'es to rebuild the list structure
  
; == Chapter 18 : We Change, Therefore We Are the Same! ==
(displayln "Chapter 18")

; 'lots' returns a list that comprises the symbol 'egg repeated 'm' times
(define lots ; this function makes use of alternate cons structure functions
  (lambda (m)
    (cond ((zero? m) '())
          (else (kons 'egg
                      (lots (sub1 m)))))))

; a legnth function with alternate cons
(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l)) (konsC (kar l)
                                  (kons 'egg
                                        '())))
          (else (konsC (kar l)
                       (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (ls)
                  (cond ((null? (kdr ls)) (set-kdr ls
                                                   (kons 'egg
                                                         '())))
                        (else (A (kdr ls)))))))
      (A l)
      l)))

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector (lambda (x) (set! kdr x))
                  kar
                  kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

; I have no idea what's going on in this chapter. Stopped reading at pg 146.
 
; == Chapter 19 : Absconding with the Jewels ==
(displayln "Chapter 19")

(displayln (deep 6) ) ; ((((((pizza))))))

(define toppings '()) ; create a var to use later

(define deepB
  (lambda (m)
    (cond ((zero? m) (letcc jump
                       (set! toppings jump)
                       'pizza))
          (else (cons (deepB (sub1 m))
                      '())))))
#| 'jump' would have been like a function, but when used, it would have also forgotten what to
do afterward. |#

(displayln toppings) ; ()
(displayln (deepB 4) ) ; ((((pizza))))
(displayln (toppings 'cake) ) ; ((((cake))))
(displayln (deepB 2) ) ; ((pizza))
(displayln (toppings 'cake) ) ; ((cake))
(displayln (cons (toppings 'cake) '()) ) ; ((cake)) ; could not cons onto toppings!
(displayln toppings) ; #<continuation>

#| *** The Twentieth Commandment ***
When thinking about a value created with 'letcc', write down the function that is equivalent but
does not forget. Then, when you use it, remember to forget. |#

(define deep&co
  (lambda (m k) ; args: depth of structure 'm', collector function 'k'
    (cond ((zero? m) (k 'pizza)) ; if 'm' zero, call 'k' on symbol 'pizza
          (else (deep&co (sub1 m) ; else, recur on m-1 and
                         (lambda (x) ; a function that takes arg 'x'
                           (k (cons x ; calls 'k' on the result of cons 'x' onto
                                    '())))))))) ; the null list

#| a function is just data and it doesn't need a name. It is evaluated where it is encountered,
just like a string, number, or symbol. |#

(displayln 
 (deep&co 0
          (lambda (x) x)) ; a function that returns 'pizza when given pizza
) ; pizza

(displayln (deep&co 6 (lambda (x) x)) ) ; ((((((pizza))))))
#| In the above calls to 'deep&co', we pass a collector that simply returns its argument. In the
base case of 'deep&co', it returns the 'pizza symbol passed to it. In the recursive case of
'deep&co', the function is passed a new collector that calls the collector on the result of 
consing the collector's argument onto the empty list. Thus, in the execution of each recursion,
a composite collector is constructed that does not evaluate until the base case. 
When we reach the base case of '(deep&co 2 (lambda (x) x))', the situation looks like this:
(deep&co 0
         (k (lambda (x)
              (cons x
                    '()))))
; where k is
(lambda (x)
  (k2 (cons x
            '())))
; where k2 is
(lambda (x) x) |#

(define deep&coB; this version remembers the collector in 'toppings'
  (lambda (m k) ; args: depth of structure 'm', collector function 'k'
    (cond ((zero? m) (begin ; if 'm' is zero, do the following
                       (set! toppings k) #| set 'toppings' equal to collector 'k', which is, 
                       now that we are at the base case, the composite application of each
                       iteration of the collector, as seen above. |#
                       (k 'pizza))) ; call 'k' on symbol 'pizza
          (else (deep&coB (sub1 m) ; else 'm' not zero, recur on m-1 and 
                          (lambda (x) ; a new collector which takes arg 'x'
                            (k (cons x ; applied 'k' to the result of cons 'x' onto
                                     '())))))))) ; the null list

(displayln (deep&coB 4 (lambda (x) x)) ) ; ((((pizza))))
#| after the base case of 'deep&coB' has assigned the composited collector to 'toppings',
calling 'toppings' on an argument will perform the work of that composite collector on the 
argument.|#

(displayln (cons (toppings 'cake)
                 (toppings 'cake)) ) ; (((((cake)))) (((cake))))

(displayln (cons (toppings 'cake)
                 (cons (toppings 'mozzarella)
                       (cons (toppings 'pizza)
                             '()))) ) ; (((((cake)))) ((((mozzarella)))) ((((pizza)))))

(displayln (two-in-a-row? '(mozzarella cake mozarella)) ) ; #f
(displayln (two-in-a-row? '(mozzarella mozzarella pizza)) ) ; #t

(define leave '())

#| 'walk' searches a list of S-expressions from left to right for the first atom and then gives
this atom to a value created by 'letcc'.|#
(define walk
  (lambda (l) ; args: list structure 'l'
    (cond ((null? l) '()) ; list was null, return null
          ((atom? (car l)) (leave (car l))) ; item was atom, shortcut with the item
          (else (begin ; else item was not atom, recur on item, recur on sublist
                  (walk (car l)) ; the branch that finds an atom will shortcut with it
                  (walk (cdr l)))))))
; This is just the minor function from the pervious definition of 'leftmost'

#| The function 'start-it' sets up a North Pole in 'here', remembers it in 'leave', and then
determines the value of (walk l). The function 'walk' crawls over l from left to right until it
finds an atom and then uses 'leave' to return that atom as the value of (start-it l) |#
(define start-it
  (lambda (l) ; arg: list structure 'l'
    (letcc here ; set up a shortcut word 'here
      (set! leave here) ; assign the shortcut word 'here to 'leave'
      (walk l)))) ; call 'walk' on 'l'

(define fill '()) ; Assign null to 'fill'

(define waddle
  (lambda (l) ; function takes a list 'l' as an arg
    (cond ((null? l) '()) ; if 'l' is null, return null
          ((atom? (car l)) (begin ; if item is atom, do the following
                             (letcc rest ; set up 'rest' as a shortcut keyword
                               (set! fill rest) ; set 'fill' to 'rest'
                               (leave (car l))) ; send item to 'leave'
                             (waddle (cdr l)))) ; recur on sublist
          (else (begin ; else item was not atom, do the following
                  (waddle (car l)) ; recur on item
                  (waddle (cdr l))))))) ; recur on sublist

#| If 'rest' is to 'waddle' what 'jump' is to 'deepB', the function ignores its argument and 
then it acts like 'waddle' for the rest of the list until it encounters the next atom.

'rest' ignores its argument because the new shortcut creates a function that remembers the rest
of what 'waddle' has to do after 'letcc' produces a value. Since the value of the first 
expression in the body of 'begin' is ignored, the function throws away the value of the 
argument. 

Afterward, it looks for first atom in the rest of the list and then uses 'leave' on it. It also
remembers what is left to do.|#

(define start-it2
  (lambda (l)
    (letcc here ; set up 'here' as a shortcut keyword
      (set! leave here) ; set 'leave' to 'here'
      (waddle l)))) ; call 'waddle' on 'l'

(displayln (start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts)) ) ; donuts

(define get-next
  (lambda (x)
    (letcc here-again
      (set! leave here-again)
      (fill 'go))))
#| 'get-next' deserves its name because it sets up a new shortcut for 'fill' to return the next
atom to. 

Just before 'fill' determines the next atom in the list of s-expressions that was passed to 
'start-it2', it changes itself so that it can resume the search for the next atom when used
again. |#

(displayln (get-next 'go) ) ; cheerios
#| The value is 'cheerios' because 'fill' is like 'rest1', except that it forgets what to do. 
Since (rest1 'go) would eventually determine the value of (leave 'cheerios), and since 'leave'
is just the shortcut 'here-again', the result of (get-next 'go) would just be cheerios. 

Now 'fill' would remember a new value, corresponding to a function like 'rest1', except that 
the rest of the list would have been smaller. |#

(define rest2
  (lambda (x)
    (waddle '(((cheerios (spaghettios))) donuts) )))

(define get-first
  (lambda (l)
    (letcc here
      (set! leave here)
      (waddle l)
      (leave '()))))

(displayln (get-first '(donut)) ) ; donut
(displayln (get-next 'go) ) ; ()
(displayln (get-first '(fish (chips))) ) ; fish

#| The function two-in-a-row*? processes a list of S-expressions and checks whether any atom
occurs twice in a row, regardless of parentheses. |#

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go))) ; 'get-next' knows how to get the rest of the atoms of 'l'
      ; returning the non-atom () is 'get-first's way of saying there is no atom in 'l'
      (if (atom? n) 
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

(displayln (two-in-a-row*? '(((food) ()) (((food))))) ) ; #t

(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a)
                       (T? n))
                   #f))))
       (get-next (lambda (x)
                   (letcc here-again
                     (set! leave here-again)
                     (fill 'go))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
                 (cond ((null? l) '())
                       ((atom? (car l)) (begin
                                          (letcc rest
                                            (set! fill rest)
                                            (leave (car l)))
                                          (waddle (cdr l))))
                       (else (begin
                               (waddle (car l))
                               (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (letcc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst) (T? fst) #f)))))

(displayln (two-in-a-row*? '(((food) ()) (((food))))) ) ; #t
    
