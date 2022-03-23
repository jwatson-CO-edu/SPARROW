#|
seasoned-schemer-06.scm
James Watson, 2013 October 
Chapter 11 of "The Seasoned Schemer" by Friedman and Felleisen
|#

#| == TODO ==
* Understand the difference between 'letrec' and 'let' |#

(load "little-schemer-05.scm") ; This will also load all previous
; == Chapter 11 : Welcome Back to the Show ==
(displayln "Chapter 11")

(displayln
 (member? 'sardines '(Italian sardines spaghetti parsley)) ) ; #t

(define is-first? ; Return true if the first item in lat is equal to a, otherwise false
  (lambda (a lat)
    (cond ((null? lat) #f) ; lat is null and cannot have a first item
          (else (eq? (car lat) a)))))

(define two-in-a-row? ; Return true if an item appears in lat twice consecutively
  (lambda (lat)
    (cond ((null? lat) #f) ; a null list cannot have two of any item
          (else (or (is-first? (car lat) (cdr lat)) ; item and first of sublist same?
                    (two-in-a-row? (cdr lat))))))) ; recur on sublist

(displayln 
 (two-in-a-row? '(Italian sardines spaghetti parsley)) ) ; #f

(displayln
 (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) ) ; #t

(define two-in-a-row? ; second version, decision to recur left to is-first-b?
  (lambda (lat)
    (cond ((null? lat) #f) ; no consecutive items in an empty list
          (else (is-first-b? (car lat) (cdr lat)))))) ; is-first-b? first two items

(define is-first-b? ; true if a is the first of lat, otherwise two-in-a-row? list
  (lambda (a lat)
    (cond ((null? lat) #f) ; no first item of an empty list, #f
          (else (or (eq? (car lat) a) ; match, return true
                    (two-in-a-row? lat)))))) ; otherwise two-in-a-row? lat
; We have moved the recursive case to a helper function and I think it's messy!

(define two-in-a-row-b? ; return true if preceding the same as item, or recur -->
  (lambda (preceding lat) ; which checks if item is the same as first of sublist
    (cond ((null? lat) #f) ; lat is null, cannot compare, return #f
          (else (or (eq? (car lat) preceding) ; match found, or
                    (two-in-a-row-b? (car lat) (cdr lat))))))) ; recur on item / sublist
; The above does not really accomplish two-in-a-row?, but contains recursive guts

(define two-in-a-row? ; third version, wrapper for two-in-a-row-b?
  (lambda (lat)
    (cond ((null? lat) #f) ; list null, cannot have two items
          (else (two-in-a-row-b? (car lat) (cdr lat)))))) ; check non-null list

(displayln 
 (two-in-a-row? '(Italian sardines spaghetti parsley)) ) ; #f

(displayln
 (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) ) ; #t

(define sum-of-prefixes-b
  (lambda (sonssf tup) ; sum-of-numbers-seen-so-far, tup
    (cond ((null? tup) '()) ; list end or list null, return null
          ; else cons the sum of sum-of-numbers-seen-so-far and first tup item onto
          (else (cons (o+ sonssf (car tup))
                      (sum-of-prefixes-b (o+ sonssf (car tup)) (cdr tup)))))));recur sum/sublist

(define sum-of-prefixes ; return a list in which each item is the sum of the corresponding tup
  (lambda (tup) ; item and all tup items that precede it
    (sum-of-prefixes-b 0 tup))) ; call sum-of-prefixes-b with zero initial sum

#| ** The Eleventh Commandment ** 
Use additional arguments when a function needs to know what other arguments to the function have
been like so far. |#

(define scramble-b
  (lambda (tup rev-pre) ; takes tuple and reversed-prefix as arguments
    (cond ((null? tup) (quote ())) ; list empty or list null, return null
          (else (cons (pick (car tup) (cons (car tup) rev-pre))
                      (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

#| The function 'scramble-b' receives a tup and the reverse of its proper prefix. If the tup is
empty, it returns the empty list. Otherwise, it constructs the reverse of the complete prefix
and uses the first element of a tup as a backward index into this list. It then processes the
rest of the tup and conses the two results together. |#

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))

(displayln (scramble '(1 1 1 3 4 2 1 1 9 2)) ) ;    (1 1 1 1 1 4 1 1 1 9)
(displayln (scramble '(1 2 3 4 5 6 7 8 9)) ) ;      (1 1 1 1 1 1 1 1 1)
(displayln (scramble '(1 2 3 1 2 3 4 1 8 2 10)) ) ; (1 1 1 1 1 1 1 1 2 8 2)

; == Chapter 12 : Take Cover ==
(displayln "Chapter 12")

(define olength ; definition of 'length' using the Y combinator
  (Y (lambda (len) ; pass a function to the Y combinator that takes 'len' as arg
       (lambda (l) ; retiuns a function that takes 'l' as an arg
         (cond ((null? l) 0) ; if list null, no addition to total, return 0
               (else (add1 (len (cdr l))))))))) ; else add1 to recur with 'len' on sublist
; We do not use define to make this a recursive function, Y creates a recursive function

(displayln (olength '(1 2 3 4)) ) ; 4

; We can use 'letrec' to create a local namespace
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond ((null? lat) (quote ()))
                      ((eq? a (car lat)) (mr (cdr lat)))
                      (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

#| In general, names defined inside 'lambda' only make sense inside 'lambda'. The difference 
between the above and the way the function was defined perviously is that the recursive portion,
defined in 'letrec' is able to reach the 'a' symbol, and is thus able to carry out the search
without having been passed the search term as an argument. |#

(displayln (multirember 'pie '(apple custard pie linzer pie torte)) )
; (apple custard linzer torte)

#| ** The Twelfth Commandment **
Use 'letrec' to remove arguments that do not change for recursive applications.|#

(define multirember-f ; returns a 'multirember'-style function that compares with 'test?'
  (lambda (test?) ; function that takes 'test?' as an argument
    (lambda (a lat) ; returns a function that takes 'a', 'lat' as arguments
      (cond ((null? lat) (quote ())) ; arg null or list empty, return null
            ((test? (car lat) a) ((multirember-f test?) a (cdr lat))) ; test? true, skip, recur
            (else (cons (car lat) ; else cons item onto recur on 'a', sublist 'lat'
                        ((multirember-f test?) a (cdr lat))))))))

; for any single application of multirember-f, the value (multirember-f test?) will be the same
(define multirember-f
  (lambda (test?)
    (letrec ; therefore we assign the multirember that encloses the test function to a name
        ((m-f (lambda (a lat)
                (cond ((null? lat) (quote ()))
                      ((test? (car lat) a) (m-f a (cdr lat)))
                      (else (cons (car lat)
                                  (m-f a (cdr lat))))))))
      m-f))) ; return the function that was composed above

; redefine 'member' so that we do not have to pass around the unchanging 'a' multiple times
(define member? ; return #t if 'a' is found in 'lat', otherwise #f
  (lambda (a lat)
    (letrec ((yes? (lambda (l) ; local function 'yes?' tests each member of 'l' against 'a'
                     (cond ((null? l) #f) ; list empty or arg null, return false
                           ((eq? (car l) a) #t) ; item is match, return true 
                           (else (yes? (cdr l))))))) ; else recur on sublist
      (yes? lat)))) ; having constructed 'yes?', call it with 'lat' as an arg

(displayln (member? 'frog '(bee bird frog dog)) ) ; #t
(displayln (member? 'weasel '(bee bird frog dog)) ) ; #f

; redefine 'union' such that the unchanging second set does not have to be passed around
(define union ; return the union of 'set1' and 'set2'
  (lambda (set1 set2)
    (letrec ((U (lambda (set) ; define a recursive function that compares 'set' to outer 'set2'
                  (cond ((null? set) set2) ; 'set' end or null arg, 'set2' has rest of union
                        ((member? (car set) set2) (U (cdr set)));skip repeat in 'set2' has,recur
                        (else (cons (car set) ; else not repeated in 'set2', cons item onto
                                    (U (cdr set)))))))) ; recur on 'set' sublist
      (U set1)))) ; call function so constructed on the first outer arg 'set1'

(displayln (union '(bee bird) '(frog dog)) ) ; (bee bird frog dog)
(displayln (union '(bee bird) '(bird dog)) ) ; (bee bird dog)

#| ** The Thirteenth Commandment **
Use 'letrec' to hide and to protect functions. |#

#| taking this pattern further, encapsulating the 'member' function inside of 'union'. 
This implementation is correct, but violates DRY, and only protects us if we wanted the freedom
to arbitrarily change 'member?' (such as arg order) at any time without breaking 'union'.|#
(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set) ; local function 'U' holds the recursive part of 'union'
                  (cond ((null? set) set2) ; 'set' empty or null, 'set2' constitutes union
                        ((M? (car set) set2) (U (cdr set))) ; if item member 'set2', skip, recur
                        (else (cons (car set) ; else, cons item onto recur on sublist
                                    (U (cdr set)))))))
             (M? (lambda (a lat) ; local function 'M?' holds an implementation of 'member?'
                   (letrec ((N? (lambda (lat) ; local function 'N?' is recursive portion of 'M?'
                                  (cond ((null? lat) #f) ; 'lat' is null set and has no members 
                                        ((eq? (car lat) a) #t); search term matches item, rtn #t
                                        (else (N? (cdr lat))))))) ; else recur on sublist
                     (N? lat))))) ; determine if 'a' is a member of inner 'lat'
      (U set1)))) ; call 'U' on 'set1', remembering 'set2' at each step
             
(displayln (union '(bee bird) '(frog dog)) ) ; (bee bird frog dog)
(displayln (union '(bee bird) '(bird dog)) ) ; (bee bird dog)

#| Helper functions are often used on specific values that make sense only inside the execution
of another function. To make sure that these minor functions always receive the correct values,
or are not otherwise modified in a breaking way, we hide such functions inside a local space
where they belong, using 'letrec'. |#

(define two-in-a-row? ; given 'lat', determine if there is a sequence of two identical atoms
  (letrec ((W (lambda (a lat);local function 'W' determines if two consecutive atom in 'lat'
                (cond ((null? lat) #f) ; 'lat' empty or arg null, cannot contain sequence
                      (else (or (eq? (car lat) a) ; either 'a' equal to item, or
                                (W (car lat) (cdr lat)))))))); recur on item/sublist
    (lambda (lat) ; the main function
      (cond ((null? lat) #f) ; if 'lat' empty or arg null, cannot contain sequence, false
            (else (W (car lat) (cdr lat)))))));else begin 'W' search with item and sublist

(displayln (two-in-a-row? '(a b c d)) ) ; #f
(displayln (two-in-a-row? '(a b b d)) ) ; #t

(define sum-of-prefixes ; return list that is a running total of each item up to that position
  (lambda (tup) ; assign a function that takes 'tup' as an arg
    (letrec ((S (lambda (sss tup) ; local var 'S' is a function with args 'sss' and 'tup'
                  (cond ((null? tup) (quote ())) ; if list empty or arg null, return null
                        (else (cons (o+ sss (car tup)) ; else cons sum of pre so far with item
                                    (S (o+ sss (car tup)) ; onto recur on new sum / sublist
                                       (cdr tup))))))))
      (S 0 tup)))) ; call the recursive part on 0 (no init sum) and top level arg 'tup'

(displayln (sum-of-prefixes '(1 2 3 4)) ) ; (1 3 6 10)

; == Chapter 13 : Hop, Skip, and jump ==
(displayln "Chapter 13")

#| URL: http://community.schemewiki.org/?seasoned-schemer
The Seasoned Schemer makes use of 'letcc' and 'try' which aren't in R5RS Scheme. Here are
implementations of such using 'syntax-rules': |#
(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation (lambda (var) body ...)))))

(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (letcc success ; the name 'success' must not occur in 'a' or 'b'
       (letcc var (success a)) . b))))


(define intersect ; return a list that is the intersection of 'set1' and 'set2'
  (lambda (set1 set2)
    (letrec ((I (lambda (set) ; local var 'I' is function, arg is 'set', with access to 'set2'
                  (cond ((null? set) (quote ())) ; set empty or arg null, intersection is null
                        ((member? (car set) set2) (cons (car set);if item member of 'set2', cons
                                                        (I (cdr set))));item onto recur sublist
                        (else (I (cdr set))))))) ; else skip item and recur sublist
      (I set1)))) ; call recursive portion on 'set1'

(displayln (intersect '(tomatoes and macaroni) '(macaroni and cheese)) ) ; (and macaroni)

(define intersectall ; return the intersection of a list of sets
  (lambda (lset) ; this function assumes that 'lset' is not null
    (letrec ((A (lambda (lset) ; assign 'A' a function that takes 'lset' as arg
                  (cond ((null? (cdr lset)) (car lset)) ; sublist null, intersection is item
                        ; else, intersect item with recur on sublist
                        (else (intersect (car lset) (A (cdr lset))))))))
      (cond ((null? lset) (quote ())) ; cond to handle null arg, return null
            (else (A lset)))))) ; The non-null case may safely assume that arg is not null

(displayln (intersectall '((tomatoes and macaroni) 
                           (macaroni and cheese)
                           (ham and cheese))) ) ; (and)

; define 'intersectall' again using the 'letcc' structure, remembering that the intersection of
;the null set and any other set is the null set
(define intersectall ; return the intersection of a list of sets
  (lambda (lset)
    (letcc hop ; when 'hop' is called as a function inside this 'letcc', whatever was passed to
      ;          'hop' will immediately be returned as the value of the 'letcc' expression
      (letrec ((A (lambda (lset) ; local function 'A' takes 'lset' as an arg
                    ;     If item null, intersection must be null set, forget execution and
                    (cond ((null? (car lset)) (hop (quote ()))) ; return null for 'letcc' exprsn
                          ((null? (cdr lset)) (car lset)) ; sublist null, item is intersection
                          ; else, intersect item with recur on sublist
                          (else (intersect (car lset) (A (cdr lset))))))))
        (cond ((null? lset) (quote ())) ; arg null, return null
              (else (A lset))))))) ; assume arg non-null, call 'A' with top 'lset' as arg

#| ** The Fourteenth Commandment ** 
Use 'letcc' to return values abruptly and promptly. |#

#| URL: http://www.ccs.neu.edu/home/matthias/BTSS/errata.html
In the below definition, the answers to the last two clauses of 'J' had been swapped. |#

#| define 'intersectall' again to make the most use of 'letcc'. That is, that execution of
certain forms should drop execution and shortcut to a special value, and that there are multiple
situations that would cause us to arrive at this value. |#
(define intersectall ; return the intersection of a list of sets
  (lambda (lset)
    (letcc hop ; shortcut value of this 'letcc' form will be whatever is passed to a 'hop' form
      (letrec ((A (lambda (lset) ; local func 'A' is recursive portion of 'intersectall'
                    (cond ((null? (car lset)) (hop (quote ()))) ; item null, shortcut to null
                          ((null? (cdr lset)) (car lset)) ; sublist null, intersection of itself
                          (else (I (car lset) (A (cdr lset))))))) ; else intersect item, sublist
               (I (lambda (s1 s2) ; local func 'I' is an implementation of 'intersect'
                    (letrec ((J (lambda (s1) ; local local func 'J' is recursive part of 'I'
                                  (cond ((null? s1) (quote ())) ; first arg null, return null
                                        ; if item member of 's2', cons item onto recur sublist
                                        ((member? (car s1) s2) (cons (car s1) 
                                                                     (J (cdr s1))))
                                        (else (J (cdr s1))))))); else, skip item, recur sublist
                      (cond ((null? s2) (hop (quote ()))) ; 's2' null, shortcut to null
                            (else (J s1))))))) ; else call 'J' on 's1'
        (cond ((null? lset) (quote ())) ; top arg null, return null
              (else (A lset))))))) ; else call 'A' on top arg 'lset'

(displayln
 (intersectall '((3 steaks and)
                 (no food and)
                 (three baked potatoes)
                 (3 diet hamburgers)))  )

; define 'rember' with 'letrec'
(define rember ; remove the first instance of 'a' from 'lat'
  (lambda (a lat)
    (letrec ((R (lambda (lat) ; local function 'R' is recursive portion, remembers 'a'
                  (cond ((null? lat) (quote ())) ; empty list or null arg, return null
                        ((eq? (car lat) a) (cdr lat)) ; match, skip item and return sublist
                        (else (cons (car lat) ; else not match, cons item onto
                                    (R (cdr lat)))))))); recur sublist
      (R lat)))) ; call 'R' on 'lat', assuming top arg 'a'

(displayln (rember 'dog '(fish dog frog)) ) ; (fish frog)

#| The function 'rember-beyond-first' takes an atom 'a' and a 'lat' and, if 'a' occurs in the
'lat', removes all atoms from the lat beyond and including the first occurrence of 'a'. |#
(define rember-beyond-first
  (lambda (a lat)
    (letrec ((R (lambda (lat) ; 'R' is the local recursive function that remembers 'a'
                  (cond ((null? lat) (quote ())) ; list empty or arg null, return null
                        ((eq? (car lat) a) (quote ()));match, skip item, return null to end list
                        (else (cons (car lat) ; else, cons item onto
                                    (R (cdr lat)))))))) ; recur on sublist
      (R lat)))) ; call 'R' on top arg 'lat'

(displayln (rember-beyond-first 'others
                                '(noodles spaghetti spatzle bean-thread roots potatoes yam
                                          others rice)) ) ; -->
; (noodles spaghetti spatzle bean-thread roots potatoes yam)

#| The function 'rember-upto-last' takes an atom 'a' and a 'lat' and removes all the atoms from
the 'lat' up to and including the last occurrence of 'a'. If there are no occurrences of 'a',
'rember-upto-last' returns the 'lat'. |#
(define rember-upto-last
  (lambda (a lat)
    (letcc skip ; 'skip' used as a keyword to shortcut value of this form
      (letrec ((R (lambda (lat) ; 'R' local func is the recursive portion
                    (cond ((null? lat) (quote ())) ; list empty or arg null, return null
                          ((eq? (car lat) a) (skip (R (cdr lat))));match, shortcut recur sublist
                          (else (cons (car lat) ; else cons item onto
                                      (R (cdr lat)))))))) ; recur on sublist
        (R lat))))) ; call 'R' on top arg
#| In the above, each time a match is encountered, it is skipped, and the value of the 'letcc'
form becomes the next recursion. This discards all the recursion that happened before the
shortcut. |#

(displayln (rember-upto-last 'roots
                             '(noodles spaghetti spatzle bean-thread roots potatoes yam
                                       others rice)) ) ; --> (potatoes yam others rice)

; == Chapter 14 : Let There Be Names ==
(displayln "Chapter 14")

; define leftmost so that it doesn't break when the depth-first search hits an empty list
(define leftmost
  (lambda (l)
    (cond ((null? l) (quote ())) ; list empty or arg null, return null
          ((atom? (car l)) (car l)) ; item is atom, return item
          ; else item is not atom,
          ; if recur on item is atom, return recur on item: WE RECURRED FROM THIS POINT TWICE!
          (else (cond ((atom? (leftmost (car l))) (leftmost (car l)))
                      (else (leftmost (cdr l)))))))) ; else recur on sublist
; the cond answer that caused us to recur on the same item as the predicate is wasteful

(displayln (leftmost '(((() a) ())) ) ) ; a

; 'let' gives names to the values of expressions
; TODO: Understand the difference between 'letrec' and 'let'

; define 'leftmost' with a little efficiency using 'let'
(define leftmost
  (lambda (l)
    (cond ((null? l) (quote ())) ;  list empty or arg null, return null
          ((atom? (car l)) (car l)) ; item is atom, return item
          ; else item is not atom
          (else (let ((a (leftmost (car l)))) ; store (recur on item) in local var 'a'
                  (cond ; Schemer's annoying nested 'cond' makes sense since it is inside 'let'
                    ((atom? a) a) ; if (recur on item) evaluated to atom, return the atom
                    (else (leftmost (cdr l))))))))) ; else recur on sublist

(displayln (leftmost '(((() a) ())) ) ) ; a

#| The function 'rember1*' goes through the list of S-expressions. When there is a list in the
car, it attempts to remove 'a' from the car. If the car remains the same, 'a' is not in the car,
and 'rember1*' must continue. When 'rember1*' finds an atom in the list, and the atom is equal
to 'a', it is removed. |#
(define rember1* ; returns nested list structure that is 'l' with first instance of 'a' removed
  (lambda (a l)
    (letrec ((R (lambda (l) ; local func 'R' recursive portion, remembering atom arg 'a'
                  (cond ((null? l) (quote ())) ; list end or arg null, return null
                        ((atom? (car l)) ; if item is atom
                         (cond ((eq? (car l) a) (cdr l)) ; if match, skip and return sublist
                               (else (cons (car l) ; else cons item onto
                                           (R (cdr l)))))); recur sublist
                        ; else item is not atom
                        (else (cond ((eqlist? (R (car l)) (car l)) ; if recur on item eq to item
                                     (cons (car l) ; cons item onto
                                           (R (cdr l)))) ; recur on sublist
                                    (else (cons (R (car l)) ; else cons recur on item onto
                                                (cdr l))))))))) ; sublist
      (R l)))) ; call 'R' on structure arg

(displayln
 (rember1* 'meat
           '((pasta meat)
             pasta
             (noodles meat sauce)
             meat potatoes)) ) ; --> ((pasta) pasta (noodles meat sauce) meat potatoes)

; define 'rember1*' again with the efficiency of 'let'!
(define rember ; return version of list structure 'l' with first instance of 'a' removed
  (lambda (a l)
    (letrec ((R (lambda (l) ; local func 'R' is the recursive portion, remembering 'a'
                  (cond ((null? l) (quote ())) ; list end or arg null, return null
                        ((atom? (car l)) ; if item atom
                         (cond ((eq? (car l) a) (cdr l)) ; if item match, skip, return sublist
                               (else (cons (car l) ; else cons item onto
                                           (R (cdr l)))))) ; recur sublist
                        ; else item is not atom
                        (else (let ((av (R (car l)))) ; recur on item and store result in 'av'
                                (cond ((eqlist? (car l) av) ; if recur result same as original
                                       (cons (car l) ; cons item onto
                                             (R (cdr l)))) ; recur sublist
                                      ; else assume recur result removed an instance of 'a'
                                      (else (cons av (cdr l)))))))))) ; cons result onto sublist
      (R l)))) ; call 'R' on top structure arg

(displayln
 (rember1* 'meat
           '((pasta meat)
             pasta
             (noodles meat sauce)
             meat potatoes)) ) ; --> ((pasta) pasta (noodles meat sauce) meat potatoes)

(define depth* ; return the maximum nested depth of a list structure
  (lambda (l)
    (cond ((null? l) 1) ; base case is depth 1
          ((atom? (car l)) (depth* (cdr l))) ; item is atom, not nested, no add, recur sublist
          ; else item is not an atom
          (else (cond ((o> (depth* (cdr l)) ; if depth of sublist is greater than
                           (add1 (depth* (car l)))) ; depth of item plus 1
                       (depth* (cdr l))) ; return depth of sublist
                      (else (add1 (depth* (car l))))))))) ; else add 1 and recur on item

(displayln (depth* '((pickled) peppers (peppers pickled))) ) ; 2
(displayln (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ) ; 4
(displayln (depth* '(c (b (a b) a) a)) ) ; 3

#| try depth* again with the efficiency of 'let', but it is wrong because ...

A 'let' first determines the values of the named expressions. Then it associates a name with
each value and determines the value of the expression in the value part. Since the value of the
named expression in our example depends on the value of (car l) before we know whether or not
'l' is empty, this version is incorrect

(define depth*
  (lambda (l)
    (let ((a (add1 (depth* (car l))))
          (d (depth* (cdr l))))
      (cond ((null? l) 1)
            ((atom? (car l)) d)
            (else (cond ((o> d a) d)
                        (else a))))))) |#

; try 'depth*' again, this time we won't use 'let' until we know the value expressions are safe
(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else ; ruled out null and atom cases for item, it is now safe to 'let'
           (let ((a (add1 (depth* (car l))))
                 (d (depth* (cdr l))))
             (if (o> d a) d a))))))

(displayln (depth* '((pickled) peppers (peppers pickled))) ) ; 2
(displayln (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ) ; 4
(displayln (depth* '(c (b (a b) a) a)) ) ; 3

#| ** The Fifteenth Commandment (Revised) ** 
Use 'let' to name the values of repeated expressions in a function definition if they may be
evaluated twice for one and the same use of the function. |#