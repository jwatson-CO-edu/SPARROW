#|
seasoned-schemer-07.scm
James Watson, 2014 March 
Chapter 14 (Continued) of "The Seasoned Schemer" by Friedman and Felleisen
|#

#|
 == TODO ==
* Explore the difference between 'let' and 'letrec'
|#

(load "seasoned-schemer-06.scm") ; This will also load all previous

#| 'if' asks only one question and provides two answers: if the question is true, it selects the
first answer; otherwise, it selects the second answer. but you already knew this |#

(define omax ; return the larger of two non-negative numbers 'n', 'm'
  (lambda (n m) ; assumes not negative due to reliance on 'o>'
    (if (o> n m) n m))) ; if 'n' is greater, return 'n', otherwise return 'm'

; omax is used to simplify 'depth*' further, we don't even need 'let'
(define depth*
  (lambda (l)
    (cond ((null? l) 1) ; base case depth 1
          ((atom? (car l)) (depth* (cdr l))) ; item is atom, no add, recur on sublist
          ; else item is not atom, return the greater of
          (else (omax (add1 (depth* (car l))) ; add 1 to recur on item
                      (depth* (cdr l))))))) ; recur on sublist

(displayln (depth* '((pickled) peppers (peppers pickled))) ) ; 2
(displayln (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ) ; 4
(displayln (depth* '(c (b (a b) a) a)) ) ; 3, as expected

; define 'scramble' again with 'let'
(define scramble
  (lambda (tup)
    (letrec ((P (lambda (tup rp)
                  (cond ((null? tup) (quote ()))
                        (else (let ((rp (cons (car tup) rp)))
                                (cons (pick (car tup) rp)
                                      (P (cdr tup) rp))))))))
      (P tup (quote ())))))

; define 'leftmost' with a 'letcc' shortcut
(define leftmost ; return the left-most atom in a depth-first search
  (lambda (l)
    (letcc skip ; let 'skip' be the shortcut keyword
      (lm l skip)))) ; call helper 'lm' on top arg 'l' and 'skip'

(define lm ; helper function for 'leftmost', with a very clever application of 'letcc'
  (lambda (l out) ; takes a list 'l' and a shortcut keyword 'out' as arguments
    (cond ((null? l) (quote ())) ; list end or arg null, return null
          ((atom? (car l)) (out (car l))) ; item is atom, jump out with item as value
          ; else item is not an atom
          (else (let () ; 'let' with no assignments is equivalent to 'begin'
                  ; below, 'out' is letting us choose branching path without another form!
                  (lm (car l) out) ; recur on item, jumping out if an atom is encountered!
                  (lm (cdr l) out)))))) ; recur on sublist, jumping out when atom found!
#| When 'let' (or 'begin', for that matter) has two exprressions in its value part, we must
first determine the value of the first expression. If it has one, we ignore it and determine the
value of the second expression. |#

(displayln (leftmost '(((() a) ())) ) ) ; a
(displayln (leftmost '(((a)) b (c)) ) ) ; a

; define 'leftmost' with all the new efficiencies we have learned
(define leftmost
  (lambda (l)
    (letcc skip; establish 'skip' as shortcut
      (letrec ((lm (lambda (l) ; local func 'lm' is recursive portion, remembering 'skip'
                     (cond ((null? l) (quote ())) ; list end or null arg, return null
                           ((atom? (car l)) (skip (car l))) ; item is atom, skip out with item
                           (else (begin ; else item not atom, 'begin' list case
                                   (lm (car l)) ; recur on item, remembering to skip with atom
                                   (lm (cdr l)))))))) ; recur on sublist, skip on atom
        (lm l))))) ; call 'lm' on top arg 'l', which will skip out with atom as above

(displayln (leftmost '(((() a) ())) ) ) ; a
(displayln (leftmost '(((a)) b (c)) ) ) ; a

#| in order to use the below, the call must be contained in a 'letcc' so that it can make use of
the shortcut keyword that you pass to it.|#
(define rm ; remove the first occurrence of atom 'a' from list structure 'l'
  (lambda (a l oh) ; takes search term 'a', list structure 'l', and shortcut keyword 'oh'
    (cond ((null? l) (oh (quote no))) ; list end, assume no removal, shortcut with 'no
          ; if item is atom
          ((atom? (car l)) (if (eq? (car l) a) ; if match
                               (cdr l) ; then skip and return sublist
                               (cons (car l) ; else, cons item onto
                                     (rm a (cdr l) oh)))) ; recur sublist
          ; else item is not atom
          (else (if (atom? (letcc oh (rm a (car l) oh))) ; if recur on item returned 'no
                    (cons (car l) ; then, cons item onto
                          (rm a (cdr l) oh)) ; recur on sublist
                    (cons (rm a (car l) 0) ; else cons recur on item, assuming succes, onto
                          (cdr l))))))) ; sublist

(displayln (letcc Say (rm 'noodles '((food) more (food)) Say)) )

(define rember1*
  (lambda (a l)
    (if (atom? (letcc oh (rm a l oh)))
        l
        (rm a l (quote ())))))

; define 'rm' again so that it does not fail on empty lists
(define rm
  (lambda (a l oh) ; args: search term 'a', list structure 'l', shortcut keyword 'oh'
    (cond ((null? l) (oh (quote no))) ; list end or arg null, shortcut with 'no
          ((atom? (car l)) ; if item is atom
           (if (eq? (car l) a) ; if item is equal to 'a'
               (cdr l) ; then return sublist
               (cons (car l) ; else cons item onto
                     (rm a (cdr l) oh)))) ; recur sublist
          (else ; else item is not atom
           (let ((new-car (letcc oh (rm a (car l) oh)))) ; set 'new-car' to attemp recur sublist
             (if (atom? new-car) ; if the result of recur was the error atom
                 (cons (car l) ; then cons item onto
                       (rm a (cdr l) oh)) ; recur on sublist
                 (cons new-car ; else, assume removal success, and cons recur result onto
                       (cdr l)))))))) ; sublist

(define rember1*
  (lambda (a l)
    (let ((new-l (letcc oh (rm a l oh))))
      (if (atom? new-l)
          l
          new-l))))

#| define 'rm' again using a 'try' form. |#
(define rm
  (lambda (a l oh) ; args: search atom 'a', list structure 'l', shortcut keyword 'oh'
    (cond ((null? l) (oh (quote no))) ; list end or arg null, shortcut with 'no
          ; item is atom
          ((atom? (car l)) (if (eq? (car l) a) ; if item is match
                               (cdr l) ; then, skip item and return sublist
                               (cons (car l) ; else, cons item onto
                                     (rm a (cdr l) oh)))) ; recur on sublist
          ; else item is not atom
          (else (try oh2 ; try the following with shortcut 'oh2
                     (cons (rm a (car l) oh2) ; cons recur on item onto
                           (cdr l)) ; sublist
                     (cons (car l) ; cons item onto
                           (rm a (cdr l) oh))))))) ; recur on sublist

(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

(displayln (letcc Say (rm 'noodles '((food) more (food)) Say)) )

; == Chapter 15 : The Difference Between Men and Boys ... ==
(displayln "Chapter 15")

(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))

(displayln (set! x (quote gone)) ) ; #<void>, no return value

(displayln x ) ; gone

(set! x (quote skins))

(define gourmet
  (lambda (food)
    (cons food
          (cons x
                (quote ())))))

(displayln (gourmet 'onion) ) ; (onion skins)

(set! x (quote rings))
(displayln (gourmet 'onion) ) ; (onion rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x
                (quote ())))))

(displayln (gourmand 'potato) ) ; (potato potato)
(displayln (gourmand 'rice) )   ; (rice rice)

(define dinerR ; write a function that remembers the last value of x before it is changed
  (lambda (food)
    (set! x food) ; whoops, we forgot it already!
    (cons (quote milkshake)
          (cons food
                (quote ())))))

(displayln (dinerR 'onion) ) ; (milkshake onion)
(displayln (dinerR 'pecanpie) ) ; (milkshake pecanpie)

(define omnivore 
  (let ((x (quote minestrone))) ; local 'x' is 'minestrone
    (lambda (food) ; arg is symbol 'food'
      (set! x food) ; we have forgotten minestrone!
      (cons food ; cons arg onto
            (cons x ; 'x', same as arg onto
                  (quote ())))))) ; list terminus

(displayln (omnivore 'onion) ) ; (onion onion)
(displayln (omnivore 'bouillabaisse) ) ; (bouillabaisse bouillabaisse)

#| Both 'dinerR' and 'omnivore' access the global 'x'. 'x' will retain the value of whatever was
passed to either of these functions until the global 'x' is 'set!' again or the program exits.
|#

#| *** The Sixteenth Commandment ***
Use 'set!' only with names defined in 'let' |#

#| You can see that 'gobbler' is exactly identical to 'omnivore'. It took me a bit to realize
that this was an attempt by the authors to be cute, and a way to illustrate that each function
has a local var 'x' that shadows the global 'x' that was defined earlier.  Neither function
manages to access or change the global 'x'. Neither function has access to the local function 
of the other. |#
(define gobbler ; we have seen this before, yes?
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  (quote ())))))) ; yes, we have

(displayln (gobbler 'bouillabaisse) ) ; (bouillabaisse bouillabaisse)
(displayln (gobbler 'gumbo) ) ; (gumbo gumbo)

(define food 'none)
(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x
                            '()))))))

(displayln (glutton 'garlic) ) ; (more garlic more garlic)

(define chez-nous ; is a swap function that does no such thing!
  (lambda ()
    (set! food x) ; set global 'food' to value of global 'x'
    (set! x food))) ; set global 'x' to the value we just assigned to 'food', now they are same!

#| *** The Eighteenth Commandment *** 
Use (set! x <value>) only when the value that x refers to is no longer needed. |#

(define chez-nous ; is a function that swaps the values of globals 'x' and 'food'
  (lambda ()
    (let ((a food)) ; safely store the value of global 'food' in local 'a'
      (set! food x) ; assign value of global 'x' to global 'food'
      (set! x a)))) ; assign value of local 'a' to global 'x'

; == Chapter 16 : Ready, Set, Bang! ==
(displayln "Chapter 16")

(define sweet-tooth ; appends 'cake to the arg and returns as a list
  (lambda (food)
    (cons food
          (cons 'cake
                '()))))

(define last 'angelfood) ; a var to hold the last food arg

(displayln (sweet-tooth 'chocolate) ) ; (chocolate cake)
(displayln (sweet-tooth 'fruit) ) ; (fruit cake)

; 'last' was not modified by the calls to 'sweet-tooth'

(define sweet-toothL ; a version of 'sweet-tooth' that saves the value of the arg to last
  (lambda (food)
    (set! last food) ; save the arg to 'last'
    (cons food ; same as before
          (cons 'cake
                '()))))

(displayln (sweet-toothL 'chocolate) ) ; (chocolate cake)
(displayln last) ; chocolate
(displayln (sweet-toothL 'fruit) ) ; (fruit cake)
(displayln last) ; fruit ; previous state was successfully saved!

(define ingredients '())

(define sweet-toothR ; version of 'sweet-tooth' that conses each arg passed to 'ingredients'
  (lambda (food)
    (set! ingredients (cons food ingredients)); 'ingredients' to cons 'food' onto 'ingredients'
    (cons food
          (cons 'cake
                '()))))

(displayln (sweet-toothR 'chocolate) ) ; (chocolate cake)
(displayln (sweet-toothR 'fruit) ) ; (fruit cake)
(displayln (sweet-toothR 'cheese) ) ; (cheese cake)
(displayln (sweet-toothR 'carrot) )
(displayln ingredients) ; (carrot cheese fruit chocolate)

(define deep ; returns a nested list that is the symbol 'pizza, 'm' layers deep
  (lambda (m)
    (cond ((zero? m) 'pizza) ; if 'm' is zero, return 'pizza
          (else (cons (deep (sub1 m)) ; else, cons recur on m-1 onto
                      '()))))) ; list terminator

(displayln (deep 3) ) ; (((pizza)))
(displayln (deep 7) ) ; (((((((pizza)))))))
(displayln (deep 0) ) ; pizza

(define Ns '()) ; a list of pizza depths (calls to 'deepR')
(define Rs '()) ; a list of deep pizzas (results of calls to 'deepR')

(define deepR ; fucntion that stores the arg and result of a call to 'deep' and returns result
  (lambda (n)
    (let ((result (deep n))) ; local 'result' holds the result of '(deep n)'
      (set! Rs (cons result Rs)) ; cons the result onto the list of results
      (set! Ns (cons n Ns)) ; cons the arg onto the list of args
      result))) ; return the result

(displayln (deepR 3) ) ; (((pizza)))
(displayln (deepR 7) ) ; (((((((pizza)))))))
(displayln (deepR 0) ) ; pizza
(displayln Ns) ; (0 7 3)
(displayln Rs) ; (pizza (((((((pizza))))))) (((pizza))))

#| *** The Nineteenth Commandment *** 
Use 'set!' to remember valuable things between two distinct uses (calls) of a function.
In other words we are preserving state. |#

(define find ; perform a linear search of 'Ns' for 'n', if found, return the item at the 
  (lambda (n Ns Rs) ;corresponding position within 'Rs'. This function assumes 'n' within 'Ns'
    (letrec ((A (lambda (ns rs) ; local 'A' is recursive portion
                  (cond ((o= (car ns) n) (car rs)) ; if item matches, return respective result
                        (else (A (cdr ns) (cdr rs))))))) ; else recur on sublists
      (A Ns Rs)))) ; note that there is no base condition for a null list!

(displayln (find 3 Ns Rs) ) ; (((pizza)))
(displayln (find 7 Ns Rs) ) ; (((((((pizza)))))))
; (displayln (find 5 Ns Rs) ) ; never passed 5 to 'deepR', ERROR at end of list!

(define deepM ; version of deepR that does not create new entries for previously-used args
  (lambda (n)
    (if (member? n Ns) ; if this 'n' used (this takes care of 'find's assumption of membership) 
        (find n Ns Rs) ; then, return that previously-computed result (how efficient of us!)
        (deepR n)))) ; else, use 'deepR' to obtain the new result

(displayln (deepM 3) ) ; (((pizza)))
(displayln (deepM 7) ) ; (((((((pizza)))))))
(displayln Ns) ; (0 7 3)
(displayln Rs) ; (pizza (((((((pizza))))))) (((pizza))))
(displayln (deepM 5) ) ; (((((pizza)))))
(displayln Ns) ; (5 0 7 3) ; new arg was consed
(displayln Rs) ; ((((((pizza))))) pizza (((((((pizza))))))) (((pizza)))) ; new result was consed

(define deepM ; redefine without using 'deepR' as a helper function
  (lambda (n) ; arg: integer 'n' the list depth within which the symbol 'pizza is placed
    (if (member? n Ns) ; if arg has been used before
        (find n Ns Rs) ; then, find current arg in list of previous and return corresponding
        (let ((result (deep n))) ; else, store the new result in local 'result'
          (set! Rs (cons result Rs)) ; cons new result onto previous results list
          (set! Ns (cons n Ns)) ; cons new arg onto previous args list
          result)))) ; return the current result

#| Now that 'deepM' has been freed of 'deepR', we are now able to re-write 'deep' to take 
advantage of 'deepM's previous arg lookup. This affords us the advantage of using previous 
results if a recursion ever coincides with a previously computed result. |#

(define deep ; version of 'deep' relies on 'deepM' to shortcut recursive case if computed before
  (lambda (m) ; arg: integer 'm' depth of 'pizza within list structure
    (cond ((zero? m) 'pizza) ; base case, return symbol 'pizza
          (else (cons (deepM (sub1 m)) ; else cons recur on m-1 onto
                      '()))))) ; the list terminator

(displayln (deepM 9) ) ; (((((((((pizza)))))))))
(displayln Ns) ; (9 8 5 0 7 3)
#|                  ^\     ^-- previous result 7 used to build (deep 8)
                      \_r-- (deep 8) was computed to build (deep 9)      |#

; define 'deepM' in a way in which it carries its own bookkeeping with it
(define deepM
  (let ((Rs '()) ; local 'Rs' stores results
        (Ns '())) ; local 'Ns' stores args
    (lambda (n) ; arg: integer 'n' pizza depth
      (if (member? n Ns) ; if current arg found in previous args
          (find n Ns Rs) ; then, return the previous result associated with the previous arg
          (let ((result (deep n))) ; else, let local 'result' be a newly computed result
            (set! Rs (cons result Rs)) ; cons new result onto list of previous results
            (set! Ns (cons n Ns)) ; cons new arg onto list of previous args
            result))))) ; return the new result
#| Note, however, that we can never take advantage our records of previous calls, because these
local  lists disappear when execution of the function exits. |#

; now we must prepare 'find' for the case when it is attempting to search an empty list
(define find
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs) ; local 'A' is the recursive portion, remembering 'n'
                  (cond ((null? ns) #f) ; if there are no 'ns' to search, return false
                        ((o= (car ns) n) (car rs)) ; if item arg a match, return item result
                        (else (A (cdr ns) (cdr rs))))))) ; else, recur on sublists
      (A Ns Rs)))) ; call A on top 'Ns' and 'Rs'

#| We can now re-write 'deepM' so that it takes advantage of 'find's new ability to report when
its second argument is an empty list. This feature obviates the need to use 'member?'. |#
(define deepM
  (let ((Rs '()) ; I'm still upset about losing my long-term storage!  can you tell me again how
        (Ns '())) ; we are supposed to gain from lookups after we throw away the lookup table?
    (lambda (n) ; arg: integer 'n' the depth of our pizza
      (let ((exists (find n Ns Rs))) ; attempt arg lookup and store lookup result in 'exists'
        (if (atom? exists) ; if result was atom (#f, lookup failure code)
            (let ((result (deep n))) ; then, store newly computed (deep n) in 'result'
              (set! Rs (cons result Rs)) ; cons new result onto list of previous results
              (set! Ns (cons n Ns)) ; cons new arg onto list of previous args
              result) ; return the new result
            exists))))) ; else, lookup succeeded

(displayln (deepM 15) ) ; (((((((((((((((pizza)))))))))))))))

(define olength ; version of 'olength' that pointlessly defines itself twice
  (let ((h (lambda (l) 0))) ; local func 'h' always returns 0
    (set! h (lambda (l) ; 'set!' 'h' to an actual definition of a length function
              (cond ((null? l) 0) ; list null, adds no length
                    (else (add1 (h (cdr l))))))) ; else add 1 to recur on sublist
    h)) ; return the function so constructed, will be assigned to 'olength'

(displayln (olength '(1 2 3)) ) ; 3

#| *** The Seventeenth Commandment ***
Use (set! x ...) for (let ((x ...)) ...) only if there is at least one 'lambda' between them,
of if the new value for x is a function that refers to x. |#

(define L ; a function that returns a length function
  (lambda (length) ; take arg 'length', return a function that
    (lambda (l) ; takes arg 'l'
      (cond ((null? l) 0) ; list null, adds none to length, return 0
            (else (add1 (length (cdr l)))))))) ; else add 1 to call 'length' on sublist

(define olength ; define a length function as we approach the Y-combinator
  (let ((h (lambda (l) 0))) ; local func 'h' takes one arg and always returns 0
    (set! h ; now set 'h' to the result of calling 'L' on a function that returns (h arg)
          (L (lambda (arg) (h arg)))) ; the function passed to 'L' always returns 0
    h)) ; return the function so constructed

(displayln (olength '(1 2 3)) ) ; 3

(define Y! ; The Applicative Order, Imperative Y Combinator
  (lambda (L) ; a function that takes arg 'l' and returns a function that ...
    (let ((h (lambda (l) '()))) ; local func 'h' takes arg 'l' and always returns null
      (set! h ; now setting 'h' to the result of calling 'L' on a function that ...
            (L (lambda (arg) (h arg)))) ; take an 'arg' and calls old 'h' on arg
      h))) ; return the function so constructed

(define Y-bang ; The Applicative Order, Imperative Y Combinator (In a shorter form)
  (lambda (f) ; return a function that takes arg function 'f', which
    (letrec ((h (f (lambda (arg) (h arg)))));applies f to a function that applies 'h' to its arg
      h)))

(define olength (Y! L))
(displayln (olength '(1 2 3)) ) ; 3

#| In order the further demonstrate the Y combinator, we define an anonymous, recursive function
that returns the maximum item in a list of numbers. |#
(displayln
 ((Y! (lambda (func-arg) ; the function passed to Y must create a closure that applies the 
       (lambda (l) ;      enclosed function to the recursive argument
         (cond ((null? l) 'no-list)
               ((null? (cdr l)) (car l))
               (else (let ((rest (func-arg (cdr l))))
                       (cond ((> (car l) rest) (car l))
                             (else rest))))))))
  '(4 5 6 3 4 8 6 2))
) ; 8

(displayln
 ((Y-bang (lambda (func-arg) ; the function passed to Y must create a closure that applies the 
       (lambda (l) ;      enclosed function to the recursive argument
         (cond ((null? l) 'no-list)
               ((null? (cdr l)) (car l))
               (else (let ((rest (func-arg (cdr l))))
                       (cond ((> (car l) rest) (car l))
                             (else rest))))))))
  '(4 5 6 3 4 8 6 2))
) ; 8

#| How do we go from a recursive function definition to a function 'f' such that (Y! f) builds
the corresponding recursive function without 'define'? 

'f' is like a recursive function, except that the name of the recursive function is replaced by
a name we'll call 'recfun', and the whole expression is wrapped in (lambda recfun ...)|#

(define Y! ; The Applicative Order, Imperative Y Combinator
  (lambda (L) ; a function that takes arg 'l' and returns a function that ...
    (let ((h 'foo)) ; local var 'h' value doesn't matter yet
      (set! h ; now setting 'h' to the result of calling 'L' on a function that ...
            (L (lambda (arg) (h arg)))) ; take an 'arg' and calls old 'h' on arg
      h))) ; return the function so constructed 

(define Y-bang ; The Applicative Order, Imperative Y Combinator (In a shorter form)
  (lambda (f) ; return a function that takes arg function 'f', which
    (letrec ((h (f (lambda (arg) (h arg)))));applies f to a function that applies 'h' to its arg
      h)))

(define D
  (lambda (func-arg) ; returns a function that takes 'func-arg' as an arg
    (lambda (s) ; that function returns a function that takes 's' as an arg
      (cond ((null? s) 1) ; list end, contribute 1 to depth
            ((atom? (car s)) (func-arg (cdr s))) ; item is atom, call top arg on sublist
            ; else item is not atom, return the maximum value of the two following:
            (else (omax (add1 (func-arg (car s))) ; add 1 to call top arg on item
                        (func-arg (cdr s)))))))) ; call top arg on sublist

(define depth* (Y! D))

#| Call Y-bang with D as the argument:
Store D in the argument name f. Define a local function h, which is the application of f
on a function that takes one 'arg' and calls h on that arg. We'll call f the 
work function closure, because it returns a function we designed to do the real work.
When we pass the work function closure to Y-bang, Y-bang calls it. The work function closure
takes a function argument, and returns the work function. Whenever the work function reaches
the recursive case, it calls the function argument. The function argument that was passed to it,
as defined in Y-bang, is the result of applying the work function closure to a function that is
defined by a function that will call the work function closure again. Y-bang is able to use
the closure stored in f to construct the next recursion. Each call to the work function closure
will remember to call that closure again to construct the next recursion.
|#

(displayln (depth* '(tomato ((cheese) ((pizza)) pie) fries) ) ) ; 4
(define depth* (Y-bang D)) ; Y-bang provides the same functionality
(displayln (depth* '(tomato ((cheese) ((pizza)) pie) fries) ) ) ; 4