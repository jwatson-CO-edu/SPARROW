#|
seasoned-schemer-09.scm
James Watson, 2014 July 
Chapter 20 of "The Seasoned Schemer" by Friedman and Felleisen
|#

(load "seasoned-schemer-08.scm") ; This will also load all previous

; == Chapter 20 : What's in Store? ==
(displayln "Chapter 20")

#| If 'table' is a function, we can use that function to extract whatever is associated with
the name with the following function: |#
(define lookup ; fetch the contents of a table
  (lambda (table name) ; args: 'table' function to get table contents, 'name' table name
    (table name))) ; apply 'table' to 'name'

#| 'extend' takes a name, a value, and a table as arguments. It returns a table (function).
The new table function first compares its argument with the name, if they are identical, the
value is returned. Otherwise, the new table function returns whatever the old table function
returns. |#

(define extend ; return a function that returns value of matched name or result of inner lookup
  (lambda (name1 value table) ; 'name1' name to be added, 'value' of name, 'table' to extend
    (lambda (name2) ; argument of the returned table (lookup) function
      (cond ((eq? name2 name1) value) ; if 'name2' equal to new name, then return new value
            (else (table name2)))))) ; else, return result of next table lookup

#| Note that instead of passing tables and names to a lookup function, each table is a function
that encloses the names and their values, as well as the name of the table that it extends. |#

(define multi-extend
  (lambda (names values table)
    (cond ((null? names) table)
          (else (extend (car names)
                        (car values)
                        (multi-extend (cdr names) (cdr values) table))))))

; Will delay defining 'define', but only enable Little Scheme to identify 'define' expressions 
(define define? ; determine if an expression 'e' is a 'define' form
  (lambda (e)
    (cond ((atom? e) #f) ; if exp is atom, then it cannot be a 'define' form, return false
          ((atom? (car e)) (eq? (car e) 'define));if first exp item matches 'define symbol,true
          (else #f)))) ; else, is not atom but form doesn't match, return false
#| 'define?' is doing some form classification work that we might otherwise expect other parts
of the Little Scheme parser to do. Will watch to see how this develops. |#

(define *define ; 'extend' the 'global-table' with a name and a boxed value
  (lambda (e)
    (set! global-table (extend (name-of e) ; fetch name from expression
                               (box (the-meaning (right-side-of e))) ; box meaning of val exp 
                               global-table)))) ; the 'global-table' will be extended

#| Each function returned by 'box' contains an a value that was enclosed during the creation of
the box and a function definition that will be used by 'setbox' in order to change that
enclosed value. 

When you read the definition of both 'setbox' and 'unbox', you will see that inside the
definition of 'box' the name 'sel' is a placeholder for a function that we pass to the returned
box function that has a menu of arguments to choose from (including the boxed value) with which
we can do whatever we like. For example, 'unbox' returns the value from the menu only and does
nothing with the setting function inside the box. |#

(define box ; return a func that applies 'sel' to 'it' and a func that 'set!'s 'it' to 'new'
  (lambda (it) ; arg: 'it' - the enclosed value to be boxed
    (lambda (sel) ; the returned func with arg 'sel' to be applied to 'it' and setting func
      (sel ; apply function 'sel' to arg menu, 'sel's arguments must match menu
       it ; the enclosed value
       (lambda (new) ; a function that takes a 'new' value and 'set!'s 'it' to that value
         (set! it new))))))

(define setbox ; change the contents of a box
  (lambda (box new) ; args: 'box' to be changed, new value
    ; pass a func for the box's 'sel' that will reach for the box's enclosed value and set func
    (box (lambda (it set) (set new))))) ;compare func args to the args passed to 'sel' in 'box'

(define unbox ; fetch the enclosed value from a box function
  (lambda (box) ; arg: 'box' for which we want the value
    (box (lambda (it set) it)))) ;pass a func to the box which will only return value from menu

(define box-all ; create a list of boxed values corresponding to a list of values
  (lambda (vals) ; arg: 'vals' - a list of values to be boxed 
    (cond ((null? vals) '()) ; vals null, return null, else
          (else (cons (box (car vals)) ; cons a box containing item onto
                      (box-all (cdr vals))))))) ; recur on sublist

#| The process of boxing is necessary because we must rely on the table structure in order to
pair values and names. In other words, we are pretending that we cannot just use Scheme's var 
storage to find a value in memory assigned to a name. Only the table must be used. If we did 
not imbue the box with the capacity for its enclosed value to be changed, then setting a new
value for a name would involve rebuilding the entire 'global-table' for each reassignment. |#

(define lookup-in-global-table ; does just what it says
  (lambda (name) ; arg: 'name' to find in the 'global-table'
    (lookup global-table name)))

(define the-meaning ; find the meaning in the context of the 'global-table'
  (lambda (e)
    (meaning e lookup-in-global-table))) ; tables a functions now, see second arg

; 'meaning' same as Chapter 10
; '*quote' same as Chapter 10

; '*identifier' has changed to be compatible with "settable" table vars
(define *identifier ; fetch (unbox) the value associated 'e' in 'table'
  (lambda (e table)
    (unbox (lookup table e)))) ; 'lookup' will call 'table' func on 'e', 'unbox' the result

; now mechanisms are in place to 'set!' a new value to a var
(define *set ; change the value in of a name in the context of the 'table' 
  (lambda (e table) ; args: 'e' name-value pair, 'table' context for set operation
    ; send the following to 'setbox':
    (setbox (lookup table (name-of e)) ; box resulting from lookup in table
            (meaning (right-side-of e) table)))) ; result of eval of val exp in table context

(define beglis ; determine the values of a list of expressions, one at a time, return last val
  (lambda (es table)
    (cond ((null? (cdr es)) (meaning (car es) table));sublist null, meaning of item in 'table' 
          (else ((lambda (val) (belis (cdr es) table)) ; else apply recur on sublist to
                 (meaning (car es) table)))))) ; meaning of item in 'table', val not used above

#| When given (lambda (x y ...) ...), '*lambda' returns a function that takes the values of the
arguments and extends 'table', pairing each formal name x, y, ... with the corresponding 
argument value. |#
(define *lambda ; mechanism to define a function
  (lambda (e table) ; args: 'e' expression, 'table' context
    (lambda (args) ; args: 'args' - list of args.  return a function that
      (beglis (body-of e) ; evaluates each expression in the body of 'e'
              (multi-extend (formals-of e) ; in a context that includes the formals 
                            (box-all args) ; and their values
                            table))))) ; and all the name-value pairs of the parent context

;(define global-table (lambda (name) '()))
(define global-table '())
(set! global-table (lambda (name)
                     (the-empty-table name)))

#|(displayln (value '(define odd?
                     (lambda (n)
                       (cond ((zero? n) #f)
                             (else (even? (sub1 n)))))))
           )|#
; A 'define' expression is not possible yet!, see 'value' and 'the-empty-table' below


#| '*application' applies the value of the first expression in an application to the values of 
the rest of the application's expressions. |# 
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

#| 'evlis' determines the values of a list of expressions, one at a time, and returns the list
of values. |#
(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else ((lambda (val)
                   (cons val
                         (evlis (cdr args) table)))
                 (meaning (car args) table))))))

(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

(define a-prim ; representation of application of primitives with one argument
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

(define b-prim ; representation of application of primitives with two arguments
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))

#| The following definition of '*cosnt' uses 'lambda' isntead of 'let'. |#
(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond ((number? e) e)
             ((eq? e #t) #t)
             ((eq? e #f) #f)
             ((eq? e 'cons) :cons)
             ((eq? e 'car) :car)
             ((eq? e 'cdr) :cdr)
             ((eq? e 'null?) :null?)
             ((eq? e 'eq?) :eq?)
             ((eq? e 'atom?) :atom?)
             ((eq? e 'zero?) :zero?)
             ((eq? e 'add1) :add1)
             ((eq? e 'sub1) :sub1)
             ((eq? e 'number?) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))
             
#| *** The Fifteenth Commandment *** (final version)
Use 'let' to name the values of repeated expressions in a function definition if they may be
evaluated twice for the same use of the function. And use 'let' to name the values of 
expressions (without 'set!') that are re-evaluated every time a function is used. |#

; '*cond' same as Chapter 10
; 'evcon' same as Chapter 10

(displayln (value '(cond (else 0))) ) ; 0
;(displayln (value '(define foo 4)) )
; A 'define' expression is not possible yet!, see 'value' and 'the-empty-table' below

#| '*letcc' sets up the North Pole 'skip', turns it into a function that *application can use,
associates the name in 'e' with this function. and evaluates the value part of the expression|#
(define *letcc
  (lambda (e table)
    (letcc skip
      (beglis (ccbody-of e)
              (extend (name-of e)
                      (box (a-prim skip))
                      table)))))

; final definition of 'value' includes 'define' 
(define value
  (lambda (e)
    (letcc the-end
      (set! abort the-end)
      (cond ((define? e) (*define e))
            (else (the-meaning e))))))

(define abort 0)

(define the-empty-table
  (lambda (name)
    (abort (cons 'no-answer
                 (cons name '())))))

;(displayln (value '(define foo 4)) )
; 'define' expression is not possible yet, see definition of 'name-of' below

; 'expression-to-action' same as Chapter 10
; 'atom-to-action' same as Chapter 10

(define list-to-action ; evaluate list, id known forms, otherwise application
  (lambda (e)
    (cond ((atom? (car e)) 
           (cond ((eq? (car e) (quote quote)) *quote)
                 ((eq? (car e) (quote lambda)) *lambda)
                 ((eq? (car e) (quote cond)) *cond)
                 ((eq? (car e) (quote letcc)) *letcc) ; new!
                 ((eq? (car e) (quote cond)) *set) ; new!
                 (else *application)))
          (else *application))))

; 'text-of' same as Chapter 10
; 'formals-of' same as Chapter 10

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define ccbody-of 
  (lambda (x)
    (cddr x)))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond ((null? (cdr (cdr x))) 0)
          (else (car (cdr (cdr x)))))))

; 'cond-lines-of' same as Chapter 10
; 'else?' same as Chapter 10
; 'question-of' same as Chapter 10
; 'answer-of' same as Chapter 10
; 'function-of' same as Chapter 10
; 'arguments-of' same as Chapter 10

(displayln (value '(define foo 4)) ) ; #<void>
(displayln (value 'foo) ) ; 4 , define works!
(displayln (value '(define odd?
                     (lambda (n)
                       (cond ((zero? n) #f)
                             (else (even? (sub1 n)))))))
           ) ; #<void>
(displayln (value '(odd? 0)) ) ; #f , application of 'define'd functions works!
(displayln (value '(define even?
                     (lambda (n)
                       (cond ((zero? n) #t)
                             (else (odd? (sub1 n)))))))
           ) ; #<void>
(displayln (value '(odd? 4)) ) ; #f
(displayln (value '(even? 4)) ) ; #t