#|
little-schemer-03.scm
James Watson, 2013 September 
Chapters 6 to 7 of "The Little Schemer" by Friedman and Felleisen
|#
(load "little-schemer-02.scm") ; This will also load "little-schemer-01.scm" when loaded

; == Chapter 6 : Shadows ==
(displayln "Chapter 6")

(define numbered?1 ; determine if s-expression is an arithmetic expression in our DSL
  (lambda (aexp) ; first attempt, don't assume that expression is well-formed
    (cond ((atom? aexp) (number? aexp)) ; expression is atom, determine if number
          ((eq? (car (cdr aexp)) (quote +));second item +, determine first & third items
           (and (numbered?1 (car aexp)) (numbered?1 (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote *));second item *, determine first & third items
           (and (numbered?1 (car aexp)) (numbered?1 (car (cdr (cdr aexp))))))
          ((eq? (car (cdr aexp)) (quote ^));second item ^, determine first & third items
           (and (numbered?1 (car aexp)) (numbered?1 (car (cdr (cdr aexp)))))))))

(define numbered? ; determine if s-expression is an arithmetic expression in our DSL
  (lambda (aexp) ; final, assume that expression is well-formed
    (cond ((atom? aexp) (number? aexp)); aexp is atom, determine if number
          ; else aexp is a list, determine if the first and third items are numbered?
          (else (and (numbered? (car aexp)) (numbered? (car (cdr (car aexp)))))))))

#| ** The Seventh Commandment ** 
Recur on the subparts that are of the same nature:
* On the subparts of a list
* On the subexpressions of an arithmetic expression |#

(define value-infix ; eval the value for arithmetic expressed in an infix DSL
  (lambda (nexp);Expression (n op m ) where op is +, *, or ^. Nested expressions allowed 
    (cond ((atom? nexp) nexp) ; item was atom, assume it is a number
          ((eq? (car (cdr nexp)) (quote +)) ; 2nd is +, apply o+ to 1st and 3rd
           (o+ (value-infix (car nexp)) (value-infix (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote *)) ; 2nd is *, apply o* to 1st and 3rd
           (o* (value-infix (car nexp)) (value-infix (car (cdr (cdr nexp))))))
          ; else 2nd is ^, apply o^ to 1st and 3rd
          (else (o^ (value-infix (car nexp)) 
                       (value-infix (car (cdr (cdr nexp)))))))))
#| NOTE: In the above DSL, if symbol other than the three specified appears in the 
proper spot in an otherwise well-formed expression, value-infix assumes exponentiation|#

(displayln ; What is the result?
 (value-infix 1) ; 1
 )

(displayln ; What is the result?
 (value-infix '(3 + (4 * 5)) ) ; 23
 )

(displayln ; What is the result?
 (value-infix '(3 + (4 ^ 5)) ) ; 1027
 )

; - Helper functions for prefix arithmetic DSL -
(define 1st-sub-exp (lambda (aexp) (car (cdr aexp)))); Fetch 1st operand of expression
(define 2nd-sub-exp (lambda (aexp) (car (cdr (cdr aexp)))));Get 2nd operand
(define operator (lambda (aexp) (car aexp))) ; Get the operator of the expression
; - End Helper prefix -

(define value ; eval the value for arithmetic expressed in an prefix DSL
  (lambda (nexp); Expression (op n m) where op is +, *, or ^. Nested expressions allowed
    (cond ((atom? nexp) nexp) ; expression atom, assume number, return
          ((eq? (operator nexp) (quote +)) ; match op +, apply o+ to sub-expressions
           (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) (quote *)) ; match op *, apply o* to sub-expressions
           (o* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
          (else ; else assume op ^, apply o^ to sub-expressions
           (o^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

#| ** The Eighth Commandment ** 
Use help functions to abstract from representations. |#

(displayln ; What is the result?
 (value 1) ; 1
 )

(displayln ; What is the result?
 (value '(+ 3 (* 4 5)) ) ; 23
 )

(displayln ; What is the result?
 (value '(+ 3 (^ 4 5)) ) ; 1027
 )

; - Helper functions for infix arithmetic DSL -
(define 1st-sub-exp-in (lambda (aexp) (car aexp))); Fetch 1st operand of expression
(define 2nd-sub-exp-in (lambda (aexp) (car (cdr (cdr aexp)))));Get 2nd operand
(define operator-in (lambda (aexp) (car (cdr aexp)))) ; Get the operator of the expression
; - End Helper prefix -

(define value-infix ; redefine with new helper functions
  (lambda (nexp); Expression (op n m) where op is +, *, or ^. Nested expressions allowed
    (cond ((atom? nexp) nexp) ; expression atom, assume number, return
          ((eq? (operator-in nexp) (quote +)) ; match op +, apply o+ to sub-expressions
           (o+ (value-infix (1st-sub-exp-in nexp)) (value-infix (2nd-sub-exp-in nexp))))
          ((eq? (operator-in nexp) (quote *)) ; match op *, apply o* to sub-expressions
           (o* (value-infix (1st-sub-exp-in nexp)) (value-infix (2nd-sub-exp-in nexp))))
          (else ; else assume op ^, apply o^ to sub-expressions
           (o^ (value-infix (1st-sub-exp-in nexp)) 
               (value-infix (2nd-sub-exp-in nexp)))))))

(displayln ; What is the result?
 (value-infix 1) ; 1
 )

(displayln ; What is the result?
 (value-infix '(3 + (4 * 5)) ) ; 23
 )

(displayln ; What is the result?
 (value-infix '(3 + (4 ^ 5)) ) ; 1027
 )

; - Empty List Number Representation -
; The count of empty lists in a list represents an integer, ( () () ) == 2
(define sero? (lambda (n) (null? n))) ; A simple empty list is zero
(define edd1 (lambda (n) (cons (quote ()) n))) ;cons () onto list, adding 1
(define zub1 (lambda (n) (cdr n))) ; pop () from list head, subtracting 1

(define null+
  (lambda (n m)
    (cond ((sero? m) n) ; second term zero, return first
          (else (edd1 (null+ n (zub1 m))))))) ; else edd1 m-1 times
; - End Empty List Rep -

(displayln ; (+ 2 3)
 (null+ '(()()) '(()()()) ) ; (() () () () ()), 5
 )

(displayln ; Do you remember lat?
 (lat? '(1 2 3)) ; #t
 )

(displayln ; Can we apply the same to our new number representation?
 (lat? '((()) (()()) (()()())) ) ; #f, absolutely not!
 )

; == Chapter 7 : Friends and Relations ==
(displayln "Chapter 7")

(define set? ; Return true if each atom appears only once
 (lambda (lat) ; assume arg is lat
   (cond ((null? lat) #t)
         ((member? (car lat) (cdr lat)) #f) ; if item found in sublist, not a set
         (else (set? (cdr lat)))))) ; else recur on sublist

(displayln ; Is this a set?
 (set? '(apple peach apple plum) ) ; #f, 'apple appears more than once
 )

(displayln ; Is this a set?
 (set? '(apples peaches pears plums) ) ; #t, no atom appears more than once
 )

(displayln ; Is this a set?
 (set? '(apple 3 pear 4 9 apple 3 4) ) ; #f, note this works for numbers too
 )

(displayln ; Is this a set?
 (set? '(apples peaches pears plums 1 2 3) ) ; #t
 )

(define makeset1 ; Make a set from a lat, unique items no repeats, first attempt
  (lambda (lat)
    (cond ((null? lat) (quote ())) ; null list or end of list, return null
          ; if item is a member of sublist, skip item and recur on sublist
          ((member? (car lat) (cdr lat)) (makeset1 (cdr lat)))
          (else ; else item is not member of sublist, cons item onto recur on sublist
           (cons (car lat) (makeset1 (cdr lat)))))))

(displayln ; What is the set of items found in the following list?
 (makeset1 '(apple peach pear peach plum apple lemon peach) )
 ) ; (pear plum apple lemon peach)

(define makeset ; Make a set from a lat, unique items no repeats
  (lambda (lat)
    (cond ((null? lat) (quote ())) ; null list or end of list, return null
          (else ; else cons item to a sublist with instances of items removed
           (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(displayln ; What is the set of items found in the following list?
 (makeset '(apple peach pear peach plum apple lemon peach) )
 ) ; (apple peach pear plum lemon), same items, different order

(displayln ; What is the set of items found in this lat?
 (makeset '(apple 3 pear 4 9 apple 3 4) )
 ) ; (apple 3 pear 4 9), works for numbers too!

(define subset?1 ; Return true if set1 is a subset of set2
  (lambda (set1 set2)
    (cond ((null? set1) #t) ; return #t, the null set is a subset of every set
          ((member (car set1) set2) ; if item of set1 a member of set2
           (subset?1 (cdr set1) set2)) ; recur on sublist of set1 and set2
          (else #f)))) ; did not locate item in set2, return #f

(displayln ; What is the result?
 (subset?1 '(5 chicken wings)
           '(5 hampburgers 2 pieces fried chicken and light duckling wings) )
 ) ; #t

(displayln ; What is the result?
 (subset?1 '(4 pounds of horseradish)
           '(four pounds chicken and 5 ounces horseradish) ) ; #f
 )

(define subset? ; Return true if set1 is a subset of set2
  (lambda (set1 set2)
    (cond ((null? set1) #t) ; the null set is a subset of every set, return #t
          ; else non-null, so evaluate
          (else (and (member? (car set1) set2) ; item is present in set2
                     (subset? (cdr set1) set2)))))) ; AND recur on sublist

(displayln ; What is the result?
 (subset? '(5 chicken wings)
           '(5 hampburgers 2 pieces fried chicken and light duckling wings) )
 ) ; #t

(displayln ; What is the result?
 (subset? '(4 pounds of horseradish)
           '(four pounds chicken and 5 ounces horseradish) ) ; #f
 )

(define eqset? ; Return true if set1 and set2 contain the same elements
  (lambda (set1 set2)
    ; set1 a subset of set2 AND set2 a subset of set1
    (and (subset? set1 set2) (subset? set2 set1))))

(displayln ; Are these sets equal?
 (eqset? '(6 large chickens with wings) '(6 chickens with large wings) ) ; #t
 )

(displayln ; Are these sets equal?
 (eqset? '(6 large chickens with wings) '(5 chickens with large wings) ) ; #f
 )

(define intersect? ; Return true if set1 and set2 intersect (share elements)
  (lambda (set1 set2)
    (cond ((null? set1) #f) ; set1 null, cannot intersect
          ; else, evaluate if either are true
          (else (or (member? (car set1) set2) ; set1 item also in set2, at least one intersects
                    (intersect? (cdr set1) set2)))))) ; recur on set1 sublist

(displayln ; Do these two sets intersect?
 (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese) ) ; #t
 ) ; shares: 'macaroni 'and

(define intersect ; Return the intersection between two sets
  (lambda (set1 set2)
    (cond ((null? set1) (quote ())) ; list null or list end, return null
          ; if item set1 member of set2, cons item set1 onto recur on sublist
          ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2))))) ; else, recur on set1 sublist / set2 
                
(displayln ; Do these two sets intersect?
 (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese) ) ; (and macaroni)
 ) ; shares: 'macaroni 'and

(define union ; Return the union of set1 and set2
  (lambda (set1 set2)
    (cond ((null? set1) set2) ; set1 null, union is all set2 elements, if exist
          ; item set1 in set2, recur on set1 sublist / set2
          ((member? (car set1) set2) (union (cdr set1) set2))
          ; else item set1 not in set2, so cons item set1 onto recur on set1 sublist / set2
          (else (cons (car set1) (union (cdr set1) set2))))))
          #| When the end of set1 is reached, all the set2 items are added to the union, this 
          happens in all cases. So, when we find that an item of set1 is a member of set2, we
          do not add it becaus it will be added later. Else, set1 has an item that set2 does not
          so it is added to the union and then we recur. |#

(displayln ; What is the union of these two sets?
 (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese) )
 ) ; (stewed tomatoes casserole macaroni and cheese)

(define difference ; Return a set of the items in set1 that are not in set2
  (lambda (set1 set2)
    (cond ((null? set1) (quote ())) ; set1 null, no additional difference to evaluate, rtn null
          ; item set1 a member of set2, do not add to difference, recur on set1 sublist / set2
          ((member? (car set1) set2) (difference (cdr set1) set2))
          ; item set1 not member of set2, add to difference, recur on set1 sublist / set2
          (else (cons (car set1) (difference (cdr set1) set2))))))

(displayln ; What is the difference between set1 and set2?
 (difference '(stewed tomatoes and macaroni casserole) '(macaroni and cheese) )
 ) ; (stewed tomatoes casserole)

(displayln ; What is the difference between set1 and set2?
 (difference '(macaroni and cheese) '(stewed tomatoes and macaroni casserole) )
 ) ; (cheese)

(define intersectall
  (lambda (l-set) ; assume that the list of sets is non-empty
    (cond ((null? (cdr l-set)) (car l-set)) ; sublist null?, return item (list)
          (else (intersect (car l-set) (intersectall (cdr l-set)))))))
          #| When item is not last in list, intersect item with all follow-on sets in
          the list. When at last set, return the set. Result of each intersection from
          last to first is intersected with the previous set. |#

(displayln ; What is the intersection of all these lists?
 (intersectall '((a b c) (c a d e) (e f g h a b)) ) ; (a)
 )

; - Representation of LS Pair -
(define a-pair? ; Return true if x is a list of two items
  (lambda (x) ; note that "pair" usually means "cons cell" in Lisp/Scheme terminology
    (cond ((atom? x) #f) ; x is atom, not a list, return false
          ((null? x) #f) ; x is null, return false
          ((null? (cdr x)) #f) ; x is a list with only one item, return false
          ((null? (cdr (cdr x))) #t) ; found list terminator at cddr, is a pair
          (else #f)))) ; anything else, return false


(define first (lambda (p) (car p))) ; return the first item of an LS pair
(define second (lambda (p) (car (cdr p)))) ; return the second item of an LS pair
(define build (lambda (s1 s2) (cons s1 (cons s2 (quote ()))))); build LS pair from s1,s2
; - End LS Pair -

(define third (lambda (l) (car (cdr (cdr l))))) ; return the third item in a list

(displayln ; Is this an LS pair?
 (a-pair? '(pear pear) ) ; #t
 )

(displayln ; Is this an LS pair?
 (a-pair? '(3 7) ) ; #t
 )

(displayln ; Is this an LS pair?
 (a-pair? '((2) (pear)) ) ; #t, b/c it is a list with only two s-expressions
 )

(displayln ; Is this an LS pair?
 (a-pair? '(full (house)) ) ; #t
 )

(displayln ; Is this an LS pair?
 (a-pair? '(a) ) ; #f
 )

(displayln ; Is this an LS pair?
 (a-pair? '(a b c) ) ; #f
 )

(define fun? (lambda (rel) (set? (firsts rel)))); return true if 'rel' a finite function
; Note that fun? assumes 'rel' is a relation, a list composed of LS pairs
#| A finite function is a list of pairs in which no first element of any pair is the
same as any other first element. |#

(define revrel1 ; Reverse each pair of a relation and return the result, first attempt
  (lambda (rel)
    (cond ((null? rel) (quote ())) ; list null or list end, return null
          ; else cons an LS pair built of the reversed items of item onto recur sublist 
          (else (cons (build (second (car rel)) (first (car rel)))
                      (revrel1 (cdr rel)))))))

(displayln ; What is the result?
 (revrel1 '((8 a) (pumpkin pie) (got sick)) ) ; ((a 8) (pie pumpkin) (sick got))
 )

(define revpair ; Reverse the items of an LS pair and return the result
  (lambda (pair)
    (build (second pair) (first pair))))

; revrel becomes even easier to write with the above helper function
(define revrel ; Reverse each LS pair of a relation and return the result, final version
  (lambda (rel)
    (cond ((null? rel) (quote ())) ; list null or list end, return null
          ; else cons an LS pair built of the reversed items of pair onto recur sublist
          (else (cons (revpair (car rel))
                      (revrel (cdr rel)))))))

(displayln ; What is the result?
 (revrel '((8 a) (pumpkin pie) (got sick)) ) ; ((a 8) (pie pumpkin) (sick got))
 )

(define seconds ; Compose a list of the second item of each of a list of lists, return
  (lambda (l)
    (cond ((null? l) (quote ())) ; list null or list end, return null
          ; else cons the second item of the item onto recur sublist
          (else (cons (second (car l)) (seconds (cdr l)))))))

(displayln ; What is the result?
 (seconds '((grape raisin) (plum prune) (stewed grape)) ) ; (raisin prune grape)
 )

(define fullfun? ; Return true if 'fun' is a full finite function
  (lambda (fun)
    (set? (seconds fun)))) ; is the list of seconds a set?
;In a full finite function the second item of each LS pair appears only once

(define one-to-one? ; Return true if 'fun' is a one-to-one finite function
  (lambda (fun)
    (fun? (revrel fun)))) ; Reverse the relation, and determine if result is function

(displayln ; Is this relation one-to-one?
 (one-to-one? '((chocolate chip) (doughy cookie)) ) ; #t
 )