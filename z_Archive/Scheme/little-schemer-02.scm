#|
little-schemer-02.scm
James Watson, 2013 September 
Chapters 4 to 5 of "The Little Schemer" by Friedman and Felleisen
|#
(load "little-schemer-01.scm") ; Note that the file was also executed
(displayln "loaded file!") ; line printed with loaded function, success!

; == Chapter 4 : Numbers Games ==
(displayln "Chapter 4")

(displayln ; Is 14 an atom?
 (atom? 14) ; True, all numbers are atoms
 )

(displayln ; Is -3 a number?
 (number? -3) ; True, but only non-negative numbers considered in this book
 )

(displayln ; Is 3.14159 a number?
 (number? 3.14159) ; True, but only integers considered in this book
 )

(define add1 ; Add 1 to n and return the result
  (lambda (n)
    (+ n 1)))

(displayln ; What is the result?
 (add1 67) ; 68
 )

(define sub1 ; Subtract 1 from n and return the result
  (lambda (n)
    (- n 1)))

(displayln ; What is the result?
 (sub1 5) ; 4
 )

(displayln ; What is the result
 (sub1 0) ; -1,  but only non-negative integers are considered in the book
 )

(displayln ; True or false?
 (zero? 0) ; True
 )

(displayln ; True or false?
 (zero? 1492) ; False
 )

(define o+
  (lambda (n m)
    (cond ((zero? m) n) ; second argument is zero, return the first
          (else (add1 (o+ n (sub1 m))))))) ; else, add 1 to (o+ n m-1)

(displayln ; What is the result?
 (o+ 46 12) ; 58
 )

(define o-
  (lambda (n m)
    (cond ((zero? m) n) ; second argument zero, nothing to subtract, return the first
          (else (sub1 (o- n (sub1 m))))))) ; else, subtract 1 from (o- n m-1)
          ; The last call to sub1 will return n, so in essence sub1 is being
          ;called on n m times

(displayln ; What is the result?
 (o- 14 3) ; 11
 )

(displayln ; What is the result?
 (o- 17 9) ; 8
 )

(displayln ; What is the result?
 (o- 18 25) ; -7, but book avoids negative numbers
 )

#| ** The First Commandment ** 
(first revision)
When recurring on a list of atoms (lat) ask two questions about it: 
(null? lat) and (else ...)
When recurring on a number, n, ask two questions about it:
(zero? n) and (else ...)|#

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0) ; If null, Empty list or list end, return 0
          (else (o+ (car tup) (addtup (cdr tup))))))) ; else add item to (addtup sublist)

(displayln ; What is the result?
 (addtup '(3 5 2 8)) ; 18
 )

(displayln ; What is the result?
 (addtup '(15 6 7 12 3)) ; 43
 )

#| ** The Fourth Commandment ** 
(first revision)
Always change at least one argument while recurring. It must be changed to be
closer to termination. The changing argument must be tested in the termination
condition:
- When using cdr, test termination with null?
- When using sub1, test termination with zero? |#

(define o*
  (lambda (n m)
    (cond ((zero? m) 0) ; second argument 0, return 0
          (else (o+ n (o* n (sub1 m))))))) ; add m-1 n's to n

(displayln ; What is the result?
 (o* 5 3) ; 15
 )

(displayln ; What is the result?
 (o* 13 4) ; 52
 )

(define tup+1 ; First attempt, assumes both tups are the same length
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) (quote ())) ; If both tups null, return '()
          (else (cons (o+ (car tup1) (car tup2)) ; Else cons the sum of the items
                      (tup+1 (cdr tup1) (cdr tup2))))))) ; to (tup+ sublist1 sublist2)

(displayln ; What is the result?
 (tup+1 '(3 6 9 11 4) '(8 5 2 0 7)) ; (11 11 11 11 11)
 )

#| ** The Fifth Commandment ** 
When building a value with o+, always use 0 for the value returned by the terminating 
condition, for adding 0 does not change the value of an addition.

When building a value with o*, always use 1 for the value returned by the terminating 
condition, for multiplying by 1 does not change the value of the multiplication.

When building a list with cons, always consider () for the value of the terminating
condition. |#

(displayln ; What is the result?
 (tup+1 '(3 7) '(4 6)) ; (7 13)
 )

(displayln ; What is the result?
 ; (tup+1 '(3 7) '(4 6 8 1))
 "When given two args of unequal length, tup+1 tries to get car of a non-existent sublist"
 )

(define tup+ ; Final version, accepts lists of unequal length
  (lambda (tup1 tup2)
    ;     When an arg is null, the tup was empty, or we reached the end of the tup and
    ;     other tup is either also null (end) or has remaining items. remaining items 
    ;     are consed unto list because the shorter tup has no more items to offer
    (cond ((null? tup1) tup2)  
          ((null? tup2) tup1) 
          ; else both tuples have items remaining
          (else (cons (o+ (car tup1) (car tup2)) ; cons the sum of the items onto
                      (tup+ (cdr tup1) (cdr tup2))))))) ; (tup+ sublist1 sublist2)

(displayln ; What is the result?
 (tup+ '(3 7) '(4 6 8 1)) ; (7 13 8 1)
 ) ; This is the behavior we were after

(displayln ; What is the result?
 (tup+ '(3 7 8 1) '(4 6)) ; (7 13 8 1)
 ) ; Correct behavior no matter which is longer

(define o>1 ; First attempt, incorrect when arguments are equal
  (lambda (n m)
    (cond ((zero? m) #t) ; m reached 0 first, n was larger
          ((zero? n) #f) ; n reached 0 first, m was larger
          (else (o>1 (sub1 n) (sub1 m)))))) ; recur on n-1 and m-1

(displayln ; What is the result?
 (o>1 3 3) ; #t, this is wrong!
 )

(define o> ; final version, notice that switching zero? statements fixes the problem because
  (lambda (n m) ; n reaches zero in both the case it is equal or smaller, both return #f
    (cond ((zero? n) #f) ; n reached 0 first, m was larger or equal
          ((zero? m) #t) ; m reached 0 first, n was larger
          (else (o> (sub1 n) (sub1 m)))))) ; else subtract one from both and perform zero? check

(displayln ; What is the result?
 (> 3 3) ; #f, this is right!
 )

(define o<
  (lambda (n m)
    (cond ((zero? m) #f) ; m reached 0 first, n was larger or equal
          ((zero? n) #t) ; n reached 0 first, m was larger
          (else (o< (sub1 n) (sub1 m)))))) ; else subtract one from both and perform zero? check

(displayln ; What is the result?
 (o< 4 6) ; #t
 )

(displayln ; What is the result?
 (o< 8 3) ; #f
 )

(displayln ; What is the result?
 (o< 6 6) ; #f
 )

(define o=
  (lambda (n m) ; Return true if neither n nor m is larger, otherwise return false
    (cond ((o> n m) #f)
          ((o< n m) #f)
          (else #t))))

(displayln ; What is the result?
 (o= 3 3) ; #t
 )

(displayln ; What is the result?
 (o= 4 3) ; #f
 )

(displayln ; What is the result?
 (o= 3 4) ; #f
 )

(define o^ ; Return n raised to the m power
  (lambda (n m)
    (cond ((zero? m) 1) ; n raised to the zero power, return 1
          (else (o* n (o^ n (sub1 m))))))) ; Multiply n by (o^ n m-1), (multiply n by m-1 n's)

(displayln ; What is the result?
 (o^ 1 1) ; 1
 )

(displayln ; What is the result?
 (o^ 2 3) ; 8
 )

(displayln ; What is the result?
 (o^ 5 3) ; 125
 )

(define o/ ; Integer division
  (lambda (n m)
    (cond ((o< n m) 0) ; Dividend is smaller than divisor, drop remainder and return 0
          (else (add1 (o/ (o- n m) m))))));add 1 to (o? n-m m), (count times m subtracts from n)

(displayln ; What is the result?
 (o/ 15 4) ; 3
 )
 
(define olength ; return the number of items in a list (assumes lat)
  (lambda (lat)
    (cond ((null? lat) 0) ; end of list or null list, return 0 additional length
          (else (add1 (olength (cdr lat))))))) ; count 1 for every recurse that finds an item

(displayln ; What is the result?
 (olength '(hotdogs with mustard sauerkraut and pickles)) ; 6
 )

(displayln ; What is the result?
 (olength '(ham and cheese on rye)) ; 5
 )

(define pick ; Return the nth item of a list, index n is 1-based
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat)) ; reached n == 1, return item
          (else (pick (sub1 n) (cdr lat)))))) ; recurse with n-1 on sublist

(displayln ; What is the result?
 (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) ; macaroni
 )

(displayln ; What is the result?
 ;(pick 0 '(a))
 "(pick 0 lat), ERROR: count variable begins <0, end clause not met, runs until list end"
 )

(define rempick ; Remove the nth item of a list and return resulting list, first index 1
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat)) ; reached n == 1, skip item, otherwise...
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
          ; cons item to recurse with n-1 on sublist

(displayln ; What is the result?
 (rempick 3 '(hotdogs with hot mustard)) ; (hotdogs with mustard)
 )

(displayln ; True or flase?
 (number? 'tomato) ; #f
 )

(define no-nums ; Return lat with all numbers removed
  (lambda (lat)
    (cond ((null? lat) (quote ())) ; null list or end of list, return '()
          (else (cond ((number? (car lat)) (no-nums (cdr lat))) ; found a number, skip it
                      (else (cons (car lat) (no-nums (cdr lat)))))))))
                      ; else cons item to recurse on sublist

(displayln ; What is the result?
 (no-nums '(5 pears 6 prunes 9 dates)) ; (pears prunes dates)
 )

(define all-nums ; Return lat with only number items
  (lambda (lat)
    (cond ((null? lat) (quote ())) ; null list or end of list, return '()
          (else (cond ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
                      ; number found, cons item to recurse on sublist
                      (else (all-nums (cdr lat)))))))) ; not a number, skip item

(displayln ; What is the result?
 (all-nums '(5 pears 6 prunes 9 dates)) ; (5 6 9)
 )

(define eqan? 
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (o= a1 a2)) ; Both numbers, test with o=
          ((or (number? a1) (number? a2)) #f) ; only one operand a number, return false
          (else (eq? a1 a2))))) ; neither is a number, test with eq?

(displayln ; True or false?
 (eqan? 4 4) ; #t
 )

(displayln ; True or false?
 (eqan? 3 4) ; #f
 )

(displayln ; True or false?
 (eqan? 4 'duck) ; #f
 )

(displayln ; True or false?
 (eqan? 'duck 'duck) ; #t
 )

(displayln ; True or false?
 (eqan? 'duck 'goose) ; #f
 )

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0) ; end of list or null list, add no occurrences
          (else (cond ((eq? (car lat) a) (add1 (occur a (cdr lat))));match found, add 1 to recur
                      (else (occur a (cdr lat)))))))); else recur on sublist

(displayln ; What is the result?
 (occur 'duck '(duck duck goose)) ; 2
 )

(displayln ; What is the result?
 (occur 'banana '(peanut butter sandwich)) ; 0
 )

(define one? (lambda (n) (o= n 1)))

(define rempick-w-one? ; remove nth, using the one? function
  (lambda (n lat) 
    (cond ((one? n) (cdr lat)) ; index is 1, skip the item. Otherwise...
          (else (cons (car lat) (rempick-w-one? (sub1 n) (cdr lat))))))); cons item to recurse

(displayln ; What is the result?
 (rempick-w-one? 3 '(lemon meringue salty pie)) ; (lemon meringue pie)
 )

; == Chapter 5 : *Oh My Gawd*: It's Full of Stars ==
(displayln "Chapter 5")
          
(define rember* ; Remove all instances of a from l, recursively, removing also from sublists
  (lambda (a l)
    (cond ((null? l) (quote ())) ; List null or end of list, return '()
          ; If the item is an atom
          ((atom? (car l)) (cond ((eq? (car l) a) (rember* a (cdr l))) ; match: skip and recur
                                 (else (cons (car l) (rember* a (cdr l))))))
                                 ; else cons item onto (rember* a sublist)
          ; Item is not an atom, build list with the following args to cons
          (else (cons (rember* a (car l)) ;     Recur on the item (recursively removing)
                      (rember* a (cdr l))))))); Recur on sublist

(displayln ; What is the result?
 (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup) ) ; ((coffee) ((tea)) (and (hick)))
 )

(displayln ; What is the result?
 (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)) )
 ) ; (((tomato)) ((bean)) (and ((flying))))

(define insertR* ; Insert new to the right of each occurrence of old, recursively
  (lambda (new old l)
    (cond ((null? l) (quote ())) ; End of list or null list, return '()
          ; Item is an atom
          ((atom? (car l)) (cond ((eq? (car l) old) ; if match, insert new and recur
                                  (cons old (cons new (insertR* new old (cdr l)))))
                                 ; else cons item onto recurse on sublist
                                 (else (cons (car l) (insertR* new old (cdr l))))))
          ; Item is not an atom, build a list with the following args to cons
          (else (cons (insertR* new old (car l)) ;      Recur on the item (a list)
                      (insertR* new old (cdr l))))))) ; Recur on the sublist

#| ** The First Commandment **
(final version)
When recurring on a list of atoms (lat) ask two questions about it: null? and else
When recurring on a number (n) ask two questions about it: zero? and else
When recurring on a list of S-expressions (l) ask three questions about it: null?, 
(atom? car l)) and else|#

#| ** The Fourth Commandment ** 
(final version)
Always change at least one argument while recurring. It must be changed to be closer to 
termination.

When recurring on a list of atoms (lat) use (cdr lat).
When recurring on a number (n) use (sub1 n).
When recurring on a list of S-expressions (l) use (car l) and (cdr l) if neither (null? l) nor
(atom? (car l)) are true.

When using cdr, test termination with null?
When using sub1, test termination with zero?|#

(define occur*
  (lambda (a l)
    (cond ((null? l) 0) ; List end or list null, add zero occurrences
          ((atom? (car l)) ; Item was an atom
           (cond ((eq? (car l) a) (add1 (occur* a (cdr l)))) ;Match, add 1 to recurse on sublist
                                 (else (occur* a (cdr l)))));else, return recurse on sublist
          ; Else, item was not an atom, was a list.  Add the following terms
          (else (o+ (occur* a (car l)) ; recur on item known to be a list
                    (occur* a (cdr l))))))) ; recur on sublist

(displayln ; What is the result?
 (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbert))
                            (banana) (bread) (banana brandy))) ; 5
 )

(define subst* ; Substitute every 'old' with a 'new', recursively, return result
  (lambda (new old l)
    (cond ((null? l) (quote ())) ; end of list or null list, return null
          ((atom? (car l)) ; item is an atom
           ;      match found, cons new onto recurse sublist, skipping old
           (cond ((eq? (car l) old) (cons new (subst* new old (cdr l))))
                 (else (cons (car l) (subst* new old (cdr l))))));else cons item to recurse
          ; else, item is not an atom, cons the following expressions:
          (else (cons (subst* new old (car l)) ; recur on the item (list)
                      (subst* new old (cdr l))))))) ; recur on sublist

(displayln ; What is the result?
 (subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbert))
                            (banana) (bread) (banana brandy)))
 ) #| ((orange) (split ((((orange ice))) (cream (orange)) sherbert)) (orange) (bread) 
(orange brandy)) |#
                           
(define insertL* ; Insert 'new' to the left of 'old', recursively, return result
  (lambda (new old l)
    (cond ((null? l) (quote ())) ; list end or list null, return null
          ((atom? (car l)) ; item is an atom
           ;     match found cons new onto cons old onto recur on sublist
           (cond ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
                 ; else cons item onto recur on sublist
                 (else (cons (car l) (insertL* new old (cdr l))))))
          ; else, item is not an atom, cons the collowing expressions
          (else (cons (insertL* new old (car l)) ; recur on the item (list)
                      (insertL* new old (cdr l))))))) ; recur on sublist

(displayln ; What is the result?
 (insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
                                              (if (a) ((wood chuck))) could chuck wood))
 ) #| ((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) 
((wood pecker chuck))) could pecker chuck wood) |#

(define member* ; Return true if 'a' is found in any sublist of 'l', otherwise return false
  (lambda (a l)
    (cond ((null? l) #f) ; list end or list null, if reached this point none found
          ((atom? (car l)) ; item is an atom
           (or (eq? (car l) a) (member* a (cdr l)))) ; return true if match or if recur sub
          ; else item not an atom, return true if either of the following true
          (else (or (member* a (car l)) ; recur on item (list)
                    (member* a (cdr l))))))) ; recur on sublist

(displayln ; What is the result?
 (member* 'chips '((potato) (chips ((with) fish) (chips))) ) ; #t
 )

(define leftmost ; Return the first atom encountered in a depth-first search
  (lambda (l)
    (cond ((atom? (car l)) (car l)) ; Item is atom, return item
          (else (leftmost (car l)))))) ; Item is list, recur on first item of item

(displayln ; What is the result?
 (leftmost  '((potato) (chips ((with) fish) (chips))) ) ; potato
 )

(displayln ; What is the result?
 (leftmost '(((hot) (tuna (and))) cheese) ) ; hot
 )

(displayln ; What is the result?
 ;(leftmost '(((() four)) 17 (seventeen)) )
 "Error: cannot recur on an emtpy list"
 )

(define eqlist?1 ; return true if two lists are equal, first attempt
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t) ; two null lists are equivalent
          ((and (null? l1) (atom? (car l2))) #f) ; l1 null, l2 has atom item - #f
          ((null? l1) #f) ; l1 null, l2 not null - #f
          ((and (atom? (car l1)) (null? l2)) #f) ; l1 has atom item, l2 null - #f
          ((and (atom? (car l1)) (atom? (car l2))); both have atom item, so test if
           ; atom items are equal AND recur on sublists
           (and (eqan? (car l1) (car l2)) (eqlist?1 (cdr l1) (cdr l2))))
          ((atom? (car l1)) #f) ; l1 has atom item, l2 does not - #f
          ((null? l2) #f) ; l2 is null, l1 is not - #f
          ((atom? (car l2)) #f) ; l2 has atom item, l1 does not - #f
          ; else l1 and l2 have list items so take the and of
          (else (and (eqlist?1 (car l1) (car l2)) ; recur on items
                     (eqlist?1 (cdr l1) (cdr l2))))))) ; recur on sublists

(displayln ; What is the result?
 (eqlist?1 '(strawberry ice cream) '(strawberry ice cream) ) ; #t
 )

(displayln ; What is the result?
 (eqlist?1 '(strawberry ice cream) '(strawberry cream ice) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?1 '(banana ((split))) '((banana) (split)) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?1 '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?1 '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))) ) ; #t
 )

(define eqlist?2 ; reduce equivalent cases
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t) ; two null lists are equivalent
          ((or (null? l1) (null? l2)) #f) ; one is null while the other is not - #f
          ((and (atom? (car l1)) (atom? (car l2))) ; both have atom item, so evaluate if
           ; items are equal AND recur on sublists
           (and (eqan? (car l1) (car l2)) (eqlist?2 (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) #f) ; one has atom item, the other not
          ; else both lists have list items, so evaluate if
          (else (and (eqlist?2 (car l1) (car l2)) ; recur on list items
                     (eqlist?2 (cdr l1) (cdr l2))))))) ; recur on sublists

(displayln ; What is the result?
 (eqlist?2 '(strawberry ice cream) '(strawberry ice cream) ) ; #t
 )

(displayln ; What is the result?
 (eqlist?2 '(strawberry ice cream) '(strawberry cream ice) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?2 '(banana ((split))) '((banana) (split)) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?2 '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))) ) ; #f
 )

(displayln ; What is the result?
 (eqlist?2 '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))) ) ; #t
 )

(define oequal?1 ; return true if arguments are equal
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2)) ; both atoms, test equivalence
          ((atom? s1) #f) ; s1 is atom, s2 not - #f
          ((atom? s2) #f) ; s2 is atom, s1 not - #f
          (else (eqlist? s1 s2))))) ; both items lists, determine equivalence

(define oequal? ; return true if arguments equal
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2)) ; both atoms, test equivalnce
          ((or (atom? s1) (atom? s2)) #f) ; one arg item and other is not - #f
          (else (eqlist? s1 s2))))) ; else both args lists, determine equivalence

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t) ; two null lists are equivalent
          ((or (null? l1) (null? l2)) #f) ; one list null, the other is not - #f
          ; else both args non-null, so evaluate if both the following are true
          (else (and (oequal? (car l1) (car l2)) ; list items equal
                     (eqlist? (cdr l1) (cdr l2))))))) ; recur on sublists

(displayln ; What is the result?
 (eqlist? '(strawberry ice cream) '(strawberry ice cream) ) ; #t
 )

(displayln ; What is the result?
 (eqlist? '(strawberry ice cream) '(strawberry cream ice) ) ; #f
 )

(displayln ; What is the result?
 (eqlist? '(banana ((split))) '((banana) (split)) ) ; #f
 )

(displayln ; What is the result?
 (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))) ) ; #f
 )

(displayln ; What is the result?
 (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))) ) ; #t
 )

#| ** The Sixth Commandment ** 
Simplify only after the function is correct. |#

(define rember
  (lambda (s l)
    (cond ((null? l) (quote ())) ; null list or list end, return null
          ((oequal? (car l) s) (cdr l)) ; found match, skip
          (else (cons (car l) (rember s (cdr l))))))) ; else recur on sublist

(displayln ; What is the result?
 (rember 'zebra '(horse zebra pony)) ; (horse pony)
 )

(displayln ; What is the result?
 (rember 'goat '(horse zebra pony)) ; (horse zebra pony)
 )