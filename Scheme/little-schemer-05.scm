#|
little-schemer-05.scm
James Watson, 2013 October 
Chapter 10 of "The Little Schemer" by Friedman and Felleisen
|#
(load "little-schemer-04.scm") ; This will also load all previous
(displayln "Chapter 10")

(define new-entry build) ; new-entry now a name that calls the build function with same args

(displayln ; What is the result?
 (new-entry '(appetizer entree beverage) '(pate boeuf vin) )
 )

(displayln ; What is the result?
 (new-entry '(appetizer entree beverage) '(beer beer beer) )
 )

(displayln ; What is the result?
 (new-entry '(beverage dessert) '((food is) (number one with us)) )
 )

#| En entry is a pair of lists whose first list is a set. Also, the two lists must be of equal
length.|#

(define lookup-in-entry ; return value associated with name if entry, otherwise (entry-f name)
  (lambda (name entry entry-f) 
    ; unpack entry into appropriate args for a helper function
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help ; return value associated with name, by linear search,
  (lambda (name names values entry-f); or eval (entry-f name)
    (cond ((null? names) (entry-f name)) ; list of names null, invoke the contingency func
          ((eq? (car names) name) (car values)) ; item name match, return item value
          ; else, recur on sublists
          (else (lookup-in-entry-help name
                                      (cdr names)
                                      (cdr values)
                                      entry-f)))))

#| A table (also called an environment) is a list of entries. |#

(define extend-table cons) #| Define extend-table as an alias for cons. Takes an entry and a
table (possibly an empty one) and creates a new table by putting the new entry in the front
of the old table. |#

(define lookup-in-table ; return value associated with name in table, if name exists
  (lambda (name table table-f)
    (cond ((null? table) (table-f name)) ; table null, eval (table-f name)
          ; else, lookup in first entry
          (else (lookup-in-entry name
                                 (car table)
                                 ; if not found, recur on subtable (next entry)
                                 (lambda (name)
                                   (lookup-in-table name
                                                    (cdr table)
                                                    table-f))))))) ; pass on contingency func

(displayln ; What is the result?
 (lookup-in-table 'entree
                  '(((entree dessert) (spaghetti spumoni))
                    ((appetizer entree beverage) (food tastes good)))
                  (lambda (name) #f))
 ) ; spaghetti, 'entree appears twice as a name, but lookup-in-table returns the first (linear)

; Ill-formed s-expressions are not considered in this interpreter
; These can be detected by passing them to a checking function before attempting evaluation

(define expression-to-action ; evaluate based on whether arg is atom or list
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define atom-to-action; evaluate atom, id reserved words as constants, otherwise identifier
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e #t) *const)
          ((eq? e #f) *const)
          ((eq? e (quote cons)) *const)
          ((eq? e (quote car)) *const)
          ((eq? e (quote cdr)) *const)
          ((eq? e (quote null?)) *const)
          ((eq? e (quote eq?)) *const)
          ((eq? e (quote atom?)) *const)
          ((eq? e (quote zero?)) *const)
          ((eq? e (quote add1)) *const)
          ((eq? e (quote sub1)) *const)
          ((eq? e (quote number?)) *const)
          (else *identifier))))

(define list-to-action ; evaluate list, id known forms, otherwise application
  (lambda (e)
    (cond ((atom? (car e)) 
           (cond ((eq? (car e) (quote quote)) *quote)
                 ((eq? (car e) (quote lambda)) *lambda)
                 ((eq? (car e) (quote cond)) *cond)
                 (else *application)))
          (else *application))))

(define value ; this function, together with all the functions it uses, is an interpreter
  (lambda (e)
    (meaning e (quote ())))) ; call meaning on expression and an empty table

(define meaning ; lookup action for expression e, and apply to e and table
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const ; action for constants
  (lambda (e table)
    (cond ((number? e) e) ; is number, return it
          ((eq? e #t) #t) ; is true, return #t
          ((eq? e #f) #f) ; is false, return #f
          (else (build (quote primitive) e))))) ; else represents primitive

(define *quote ; action for quotes
  (lambda (e table)
    (text-of e))) ; return the second item of e

(define text-of second) ; text-of is an alias for second

(define *identifier ; action for identifiers
  (lambda (e table)
    (lookup-in-table e table initial-table))) ; lookup value of identifier in table

(define initial-table ; return an empty table
  (lambda (name)
    (car (quote ()))))

#| The difference between primitive and non-primitive functions is that we know
what primitives do because they are built into the interpreter. Non-primitives are defined
by their arguments and function bodies by the user using primitives. |#

(define *lambda
  (lambda (e table)
    (build (quote non-primitive) ; label form as non-primitive
           (cons table ; cons table onto 
                 (cdr e))))) ; list: (formals body)

(displayln ; What is the meaning of a lambda expression as follows?
 (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))) )
 ) ; (non-primitive ( (((y z) ((8) 9)))  (x)      (cons x y) ))
;                     table-----------|  formals  body-----|
;                   The list of (table formals body) is called a closure record

; - lambda helpers -
(define table-of first) ; alias, table is the first item
(define formals-of second) ; alias, formals list is the second item
(define body-of third) ; alias, body is the third item
; - end lambda -

; - cond helpers -
(define question-of first) ; alias, question / condition to eval is the first item
(define answer-of second) ; alias, answer / action to take is the second item
(define else? ; is the arg an 'else symbol?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else))) ; arg is atom. determine if 'else
          (else #f)))) ; not atom. cannot be 'else, return false

(define evcon ; evaluate cond, this is the guts of cond
  (lambda (lines table) ; given a list of cond lines and a table
    (cond ((else? (question-of (car lines))) ; item question is 'else
           (meaning (answer-of (car lines)) table)) ; eval item answer
          ((meaning (question-of (car lines)) table) ; eval item question -> is true
           (meaning (answer-of (car lines)) table)) ; eval item answer
          (else (evcon (cdr lines) table))))) ; else, recur on sublist lines and table
; note there was no action for the null? case, one of the above conditions better be true!

(define cond-lines-of cdr) ; alias, cond lines are in the sublist following func name
; - end cond -

(define *cond ; action for conds
  (lambda (e table)
    (evcon (cond-lines-of e) table))) ; call evcon on the cond lines and the table

(displayln ; what is the result?
 (*cond '(cond (coffee klatsch) (else party))
        '(((coffee) (#t))
          ((klatsch party) (5 (6)))) )
 ) ; 'coffee was true, lookup 'klatsch, found 5

#| An application is a list of expressions whose car position contains an expression whose
value is a function. An application must always determine the meaning of all its arguments.
|#

(define evlis ; take list of representations of expressions and return list of meanings
  (lambda (args table)
    (cond ((null? args) (quote ())) ; list end or list null, return null
          (else (cons (meaning (car args) table) ; cons the meaning of the args item
                      (evlis (cdr args) table)))))) ; onto recur sublist args and table

; - application helpers -
(define function-of car) ; alias, the function name is the first item
(define arguments-of cdr) ; alias. the arguments are in the sublist after first
; - end application -

(define *application ; action for functions
  (lambda (e table)
    ; call apply on ...
    (oapply (meaning (function-of e) table) ; retrieve action for function name
           (evlis (arguments-of e) table)))) ; get list of argument meanings

(define primitive? ; return true if the first item in l is the symbol 'primitive
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive? ; return true if the first item in l is the symbol 'non-primitive
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define oapply ; evaluate primitives and closures (non-primitive functions)
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive? fun) (apply-closure (second fun) vals)))))
;         there is no else

(define apply-primitive ; apply one of the primitive functions defined in our language
  (lambda (name vals)
    (cond ((eq? name (quote cons)) (cons (first vals) (second vals))) ; apply Scheme cons
          ((eq? name (quote car)) (car (first vals))) ; apply Scheme car
          ((eq? name (quote cdr)) (cdr (first vals))) ; apply Scheme cdr
          ((eq? name (quote null?)) (null? (first vals))) ; apply Scheme null?
          ((eq? name (quote eq?)) (eq? (first vals) (second vals))) ; apply Scheme eq?
          ((eq? name (quote atom?)) (:atom? (first vals))) ; apply LS :atom?
          ((eq? name (quote zero?)) (zero? (first vals))) ; apply Scheme zero?
          ((eq? name (quote add1)) (add1 (first vals))) ; apply LS add1
          ((eq? name (quote sub1)) (sub1 (first vals))) ; apply LS sub1
          ((eq? name (quote number?)) (number? (first vals)))))) ; apply Scheme number?

(define :atom? ; extends LS atom? to be used by our interpreter
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) (quote primitive)) #t)
          ((eq? (car x) (quote non-primitive)) #t)
          (else #f))))

#| Applying a closure to a list of values is the same as finding the meaning of the 
closure's body with its table extended by an entry of the form (formals values). In this
entry, formals is the formals of the closure and values is the result of evlis. |#

(define apply-closure ; apply a non-primitive function
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure)
                                      vals)
                           (table-of closure)))))

; Let's take the new interpreter for a spin!
(displayln (value 2) ) ; 2
(displayln (value '(cons 1 2)) ) ; (1 . 2)
(displayln (value '(car (cons 1 2))) ) ; 1
(displayln (value '(cdr (cons 1 2))) ) ; 2
(displayln (value '(null? '())) ) ; #t
(displayln (value '(null? 3)) ) ; #f
(displayln (value '(eq? 3 3)) ) ; #t
(displayln (value '(atom? (cons 1 2))) ) ; #f
(displayln (value '(atom? 4)) ) ; #t
(displayln (value '(zero? 0)) ) ; #t
(displayln (value '(add1 4)) ) ; 5
(displayln (value '(sub1 4)) ) ; 3
(displayln (value '(number? 3)) ) ; #t
(displayln (value '(number? 'horse)) ) ; #f
(displayln (value '(quote horse)) ) ; horse
(displayln (value '(cond (#t (quote was-true))
                         (else (quote was-false)))) ) ; was-true
(displayln (value '(cond (#f (quote was-true))
                         (else (quote was-false)))) ) ; was-false
(displayln (value '((lambda (x) x) 2)) ) ; 2
(displayln (value '((lambda (x) (cond ((number? x) (quote was-number))
                                      (else (quote not-number)))) 4)) ) ; was-number
(displayln (value '((lambda (x) (cond ((number? x) (quote was-number))
                                      (else (quote not-number)))) (quote horse))) ) ; not-number
