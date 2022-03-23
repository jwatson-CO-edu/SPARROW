#| callWcc_1.scm 
James Watson 2014 December
Learning about Call with Current Continuation, Part 1 
URL: http://community.schemewiki.org/?call-with-current-continuation
URL: http://en.wikipedia.org/wiki/Call-with-current-continuation
|#
(load "scheme-tools.scm")

; First of all, this introduction uses an abbreviation for the long-winded name: call/cc
(define call/cc call-with-current-continuation)

#| Taking a function f as its only argument, call/cc takes the current continuation 
(i.e., a "snapshot" of the current control context or control state of the program) 
as an object and applies f to it. The continuation object is a first-class value and is 
represented as a function, with function application as its only operation. When a continuation
object is applied to an argument, the existing continuation is eliminated and the applied 
continuation is restored in its place, so that the program flow will continue at the point at 
which the continuation was captured and the argument of the continuation will become the 
"return value" of the call/cc invocation. Continuations created with call/cc may be called more
than once, and even from outside the dynamic extent of the call/cc application.

Making this type of implicit program state visible as an object is known in computer science as
reification. (Note that Scheme does not syntactically distinguish continuation application from
function application.) 

One of the simplest uses of call/cc is to provide a simple escape mechanism out of any 
nested computation. In some languages, such as Java or C, this is known under names such as 
'break', 'continue', and 'return'. 

Now, how do we use 'call/cc'? The procedure gets as an argument another procedure, which is 
then called with a single argument: The 'return' procedure. As soon as the return procedure is 
called, the call to 'call/cc' returns. Specifically, 'call/cc' returns the argument passed to 
the 'return' procedure. Note that we did not explicitly 'define' the 'return' procedure, but we
can call on that name as an escape procedure to immediately assign its argument as the ultimate
evaluation result of 'call/cc', as below: |#
(define search (lambda (wanted? lst)
                 ; 'call/cc' takes a func as an arg, func has one arg, the escape "procedure"
                 (call/cc (lambda (return) ; make name 'return' an escape procedure
                            (for-each ; call the following procedure on every item of 'lst'
                             (lambda (item) (if (wanted? item) ; if 'wanted?' test on 'item' passes
                                                (return item))) ; then make 'item' the result of 'call/cc'
                                            ; The 'if' form does not require an alternate
                             lst) ; the list to be processed
                          #f)))) #| If execution has reached this point, then no call to 'return' has been
                                   made.  The result of evaluating 'call/cc' will be false. If the desired
                                   result had been 'return'ed, then '#f' would never be evaluated. |#

(displayln 
 (search (lambda (item) (eq? 'bats item)) '(frogs bats snakes)) 
) ; bats

(displayln 
 (search (lambda (item) (eq? 'toads item)) '(frogs bats snakes)) 
) ; #f
#| Here we can see how the procedure return is introduced into the scope of the for-each expression where 
we are using it. As soon as we found the element we were looking for, we pass it to the return procedure. 
As soon as this happens, the call/cc expression returns this value. Nothing else is done. The whole 
expression terminates. |#

(define good-element? (lambda (item) (eq? 'bats item))) ; The test for whether we consider an element good

; 'treat' tests if an element is good, if so, passes a special symbol to the procedure provided
(define treat (lambda (element like-it) ; args: the 'element' of a list, 'like-it' procedure for a match
                (if (good-element? element) ; if the test for a 'good-element?' passes, then
                    (like-it 'fnord)))) ; Pass 'fnord to the arg procedure, a non-local exit

(define search (lambda (treat lst) ; args: 'treat' procedure to test each element, 'lst' to search
                 (call/cc (lambda (return) ; make name 'return' an escape procedure
                            (for-each ; call the following procedure on every item of 'lst'
                             ; pass 'element' and response proc 'return' to 'treat'
                             (lambda (element) (treat element return))
                             lst) ; the list to process
                            #f)))) ; no jumps occurred, 'call/cc' evaluates to false
  
(displayln 
 (search treat '(frogs bats snakes)) 
) ; fnord ; the special symbol we designated

(displayln 
 (search treat '(frogs dolphins snakes)) 
) ; #f
#| As you can see, we pass treat the return procedure we got from call/cc. Treat then proceeds to do some 
processing, and in the event that it likes it, calls the return procedure with a value it's liking. 

This is no different from the use of return above, but as you can see, it works from anywhere in the 
program. That property gave this kind of operation a special name: non-local exit. We are not exiting 
locally at the location of the exit instruction, but at a completely different place, or locality.

All we did so far was escaping from a deeply nested call structure. These are called exit continuations.
Exit continuations are a special kind of continuation, and call/cc creates the general kind. 

'call/cc' captures the current continuation as a first-class object. The captured continuation has 
unlimited extent: it can be stored in a variable and can be called/activated as many times as desired. 
If and whenever the continuation is called, the then-current continuation will be abandoned, 
and the program flow will continue at the point at which the continuation was captured. The procedure
given to 'call/cc' is called with the current continuation, whatever would happen next if call/cc returned
normally, as its one argument. So by invoking that procedure the procedure given to call/cc can return to 
the context where call/cc was invoked, just like longjmp. It, or another procedure that gets hold of the
passed continuation somehow, can do this at any later time, and more than once|#

(define return #f) ; declare a global variable 'return'

(displayln
 ; add 1 plus ...
 (+ 1 (call/cc (lambda (cont) ; setup continuation with 'cont' as the exit procedure
                 (set! return cont) ; set global var 'return' to the exit procedure
                 1))) ; did not use 'cont' this time, 'call/cc' evaluates to 1
) ; 2

#| By setting the global var 'return' to the 'call/cc' escape procedure, we have let that procedure out in
the wild. Now, any other code can call the exit procedure, and the value passed to it will be the
value of 'call/cc' and evaluation will continue at wherever 'call/cc' declared the continuation. |#

(return 22); 23, we continued at 'call/cc' above, where 22 (the newest value of 'call/cc') is added to 1
;                and displayed. We re-entered the computation from outside, we did not just leave it.

(+ 2 (return 22)) ; 23, continued right at 'call/cc' with 22 as the value. 
;                       Forgot about adding 2 to '(return 22)'
