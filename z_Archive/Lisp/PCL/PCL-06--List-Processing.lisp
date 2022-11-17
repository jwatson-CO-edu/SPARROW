#|
PCL-06--List-Processing.lisp
James Watson, 2012 January
Lists and List Processing, the facilities for which Lisp is named 
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

#| Historically, lists were Lisp's original composite data type, though it has 
been decades since they were its only such data type. Practically speaking, 
lists remain in the language because they're an excellent solution to certain 
problems. One such problem--how to represent code as data in order to support 
code-transforming and code-generating macros--is particular to Lisp, which may 
explain why other languages don't feel the lack of Lisp-style lists. More 
generally, lists are an excellent data structure for representing any kind of 
heterogeneous and/or hierarchical data. They're also quite lightweight and 
support a functional style of programming that's another important part of 
Lisp's heritage. |#

; Please see file
; SICP-03--Hierarchical-Data.lisp
; for the nature of list structure as a composite of cons cells

#| Because the values in a cons cell can be references to any kind of object, 
you can build larger structures out of cons cells by linking them together. So 
when I say a particular value is a list, what I really mean is it's either NIL 
or a reference to a cons cell.|#

; = Functional Programming and Lists=
#| The essence of functional programming is that programs are built entirely of 
functions with no side effects that compute their results based solely on the 
values of their arguments. The advantage of the functional style is that it 
makes programs easier to understand. And since the result of a function is 
determined only by the values of its arguments, its behavior is easier to 
understand and test. For instance, when you see an expression such as (+ 3 4), 
you know the result is uniquely determined by the definition of the + function 
and the values 3 and 4. You don't have to worry about what may have happened 
earlier in the execution of the program since there's nothing that can change 
the result of evaluating that expression. 

A list, on the other hand, can be mutated  by SETFing the CARs and CDRs of the 
cons cells that make up its backbone. However, lists can be treated as a 
functional data type if you consider their value to be determined by the 
elements they contain. Thus, any list of the form (1 2 3 4) is functionally 
equivalent to any other list containing those four values, regardless of what 
cons cells are actually used to represent the list. And any function that takes 
a list as an argument and returns a value based solely on the contents of the 
list can likewise be considered functional. 

The reason most list functions are written functionally is it allows them to 
return results that share cons cells with their arguments. To take a concrete 
example, the function APPEND takes any number of list arguments and returns a 
new list containing the elements of all its arguments. For instance: |#
(append (list 1 2) (list 3 4)) ==> (1 2 3 4)

#| From a functional point of view, APPEND's job is to return the list (1 2 3 4)
without modifying any of the cons cells in the lists (1 2) and (3 4). One 
obvious way to achieve that goal is to create a completely new list consisting 
of four new cons cells. However, that's more work than is necessary. Instead, 
APPEND actually makes only two new cons cells to hold the values 1 and 2, 
linking them together and pointing the CDR of the second cons cell at the head 
of the last argument, the list (3 4). It then returns the cons cell containing 
the 1. None of the original cons cells has been modified, and the result is 
indeed the list (1 2 3 4). The only wrinkle is that the list returned by APPEND 
shares some cons cells with the list (3 4). |#

#| Two lists to APPEND
    _____  _____      _____   _____
 -->|*|*-->|*|/|   -->|*|*--->|*|/|
     |      |      ^   |       |
    _v_    _v_     |  _v_     _v_
    |1|    |2|     |  |3|     |4| 
                   |
New list created   |
    _____  _____   |
 -->|*|*-->|*|*----+  Only two new cons cells created, with the last cdr
     |      |         pointing to the last list passed to APPEND. This last
    _v_    _v_        list is not duplicated.
    |1|    |2|                                   |#

;; == Destructive Operations ==
#| Because of Lisp's functional heritage, operations that modify existing 
objects are called destructive--in functional programming, changing an object's 
state "destroys" it since it no longer represents the same value. However, using
the same term to describe all state-modifying operations leads to a certain 
amount of confusion since there are two very different kinds of destructive 
operations, for-side-effect operations and recycling operations. |#

; = For-Side-Effect Operations =
#| For-side-effect operations are those used specifically for their side 
effects. All uses of SETF are destructive in this sense, as are functions that 
use SETF under the covers to change the state of an existing object such as 
VECTOR-PUSH or VECTOR-POP. But it's a bit unfair to describe these operations as
destructive--they're not intended to be used in code written in a functional 
style, so they shouldn't be described using functional terminology. However, if 
you mix nonfunctional, for-side-effect operations with functions that return 
structure-sharing results, then you need to be careful not to inadvertently 
modify the shared structure. 

For instance, consider these three definitions: |#
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
*list-1* #|-->|# (1 2)
*list-2* #|-->|# (3 4)
*list-3* #|-->|# (1 2 3 4) #| *list-3* and *list-2* share structure just like 
                              the lists in the previous diagram. |#
(setf (first *list-2*) 0) #|-->|# 0
*list-2*                  #|-->|# (0 4)     ; as expected
*list-3*                  #|-->|# (1 2 0 4) ; maybe not what you wanted

; = Recycling Operations =
#| The other kind of destructive operations, recycling operations, are intended 
to be used in functional code. They use side effects only as an optimization. In
particular, they reuse certain cons cells from their arguments when building 
their result. However, unlike functions such as APPEND that reuse cons cells by 
including them, unmodified, in the list they return, recycling functions reuse 
cons cells as raw material, modifying the CAR and CDR as necessary to build the 
desired result. Thus, recycling functions can be used safely only when the 
original lists aren't going to be needed after the call to the recycling 
function. 

To see how a recycling function works, let's compare REVERSE, the nondestructive
function that returns a reversed version of a sequence, to NREVERSE, a recycling
version of the same function. Because REVERSE doesn't modify its argument, it 
must allocate a new cons cell for each element in the list being reversed. But 
suppose you write something like this:|#
(setf *list* (reverse *list*))
#| By assigning the result of REVERSE back to *list*, you've removed the 
reference to the original value of *list*. Assuming the cons cells in the 
original list aren't referenced anywhere else, they're now eligible to be 
garbage collected. However, in many Lisp implementations it'd be more efficient 
to immediately reuse the existing cons cells rather than allocating new ones and
letting the old ones become garbage.

NREVERSE allows you to do exactly that. The N stands for non-consing, meaning it
doesn't need to allocate any new cons cells. The exact side effects of NREVERSE 
are intentionally not specified--it's allowed to modify any CAR or CDR of any 
cons cell in the list--but a typical implementation might walk down the list 
changing the CDR of each cons cell to point to the previous cons cell, 
eventually returning the cons cell that was previously the last cons cell in the
old list and is now the head of the reversed list. No new cons cells need to be 
allocated, and no garbage is created. 

NCONC is the recycling version of APPEND, and DELETE, DELETE-IF, DELETE-IF-NOT,
and DELETE-DUPLICATES are the the recycling versions of the REMOVE family of 
sequence functions.

The side effects of most recycling functions aren't specified tightly enough to 
be relied upon. However, the waters are further muddied by a handful of 
recycling functions with specified side effects that can be relied upon. They 
are; NCONC, and NSUBSTITUTE and its -IF and -IF-NOT variants. |#

; - NCONC -
#| Like APPEND, NCONC returns a concatenation of its list arguments, but it 
builds its result in the following way: for each nonempty list it's passed, 
NCONC sets the CDR of the list's last cons cell to point to the first cons cell 
of the next nonempty list. It then returns the first list, which is now the head
of the spliced-together result. Thus: |#
(defparameter *x* (list 1 2 3))
(nconc *x* (list 4 5 6)) #|-->|# (1 2 3 4 5 6)
*x* #|-->|# (1 2 3 4 5 6)

; - NSUBSTITUTE -
#| NSUBSTITUTE and variants can be relied on to walk down the list structure of 
the list argument and to SETF the CARs of any cons cells holding the old value 
to the new value and to otherwise leave the list intact. It then returns the 
original list, which now has the same value as would've been computed by 
SUBSTITUTE. |#

;; == Combining Recycled with Shared Structure ==
#| In practice, recycling functions tend to be used in a few idiomatic ways. By 
far the most common recycling idiom is to build up a list to be returned from a 
function by "consing" onto the front of a list, usually by PUSHing elements onto
a list stored in a local variable and then returning the result of NREVERSEing 
it.

This is an efficient way to build a list because each PUSH has to create only 
one cons cell and modify a local variable and the NREVERSE just has to zip down 
the list reassigning the CDRs. Because the list is created entirely within the 
function, there's no danger any code outside the function has a reference to any
of its cons cells. Here's a function that uses this idiom to build a list of the
first n numbers, starting at zero: |#
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10) #|-->|# (0 1 2 3 4 5 6 7 8 9)

#| The next most common recycling idiom9 is to immediately reassign the value 
returned by the recycling function back to the place containing the potentially 
recycled value. For instance, you'll often see expressions like the following, 
using DELETE, the recycling version of REMOVE: |#
(setf foo (delete nil foo))
#| This sets the value of foo to its old value except with all the NILs removed.
However, even this idiom must be used with some care--if foo shares structure 
with lists referenced elsewhere, using DELETE instead of REMOVE can destroy the 
structure of those other lists. |#

;; == List Manipulation Functions ==
; = nthcdr =
#| NTHCDR takes an index and a list and returns the result of calling CDR n 
times. There's no way to get to the nth element of a list without following n 
CDR references. |#

; = Composite car/cdr Functions =
#| The 28 composite CAR/CDR functions are named by placing a sequence of up to 
four As and Ds between a C and R, with each A representing a call to CAR and 
each D a call to CDR. In other words, these are really functions on trees rather
than lists. Thus:  |#
; This place | Is equivalent to this place 
; -----------+----------------------------  
  (caar x)     (car (car x))                    
  (cadr x)     (car (cdr x))                    
  (cdar x)     (cdr (car x))                    
  (cddr x)     (cdr (cdr x))                    
  (caaar x)    (car (car (car x)))              
  (caadr x)    (car (car (cdr x)))              
  (cadar x)    (car (cdr (car x)))              
  (caddr x)    (car (cdr (cdr x)))              
  (cdaar x)    (cdr (car (car x)))              
  (cdadr x)    (cdr (car (cdr x)))              
  (cddar x)    (cdr (cdr (car x)))              
  (cdddr x)    (cdr (cdr (cdr x)))              
  (caaaar x)   (car (car (car (car x))))        
  (caaadr x)   (car (car (car (cdr x))))        
  (caadar x)   (car (car (cdr (car x))))        
  (caaddr x)   (car (car (cdr (cdr x))))        
  (cadaar x)   (car (cdr (car (car x))))        
  (cadadr x)   (car (cdr (car (cdr x))))        
  (caddar x)   (car (cdr (cdr (car x))))        
  (cadddr x)   (car (cdr (cdr (cdr x))))        
  (cdaaar x)   (cdr (car (car (car x))))        
  (cdaadr x)   (cdr (car (car (cdr x))))        
  (cdadar x)   (cdr (car (cdr (car x))))        
  (cdaddr x)   (cdr (car (cdr (cdr x))))        
  (cddaar x)   (cdr (cdr (car (car x))))        
  (cddadr x)   (cdr (cdr (car (cdr x))))        
  (cdddar x)   (cdr (cdr (cdr (car x))))        
  (cddddr x)   (cdr (cdr (cdr (cdr x))))        

;; == Mapping ==
#| Although MAP can be used with both lists and vectors, Common Lisp also 
provides six mapping functions specifically for lists. The differences between 
the six functions have to do with how they build up their result and whether 
they apply the function to the elements of the list or to the cons cells of the 
list structure. |#

; = mapcar =
#| MAPCAR is the function most like MAP. Because it always returns a list, it 
doesn't require the result-type argument MAP does. Instead, its first argument 
is the function to apply, and subsequent arguments are the lists whose elements 
will provide the arguments to the function. Otherwise, it behaves like MAP: the 
function is applied to successive elements of the list arguments, taking one 
element from each list per application of the function. The results of each 
function call are collected into a new list. For example: |#
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) #|-->|# (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30)) #|-->|# (11 22 33)

; = maplist =
#| MAPLIST is just like MAPCAR except instead of passing the elements of the 
list to the function, it passes the actual cons cells. Thus, the function has 
access not only to the value of each element of the list (via the CAR of the 
cons cell) but also to the rest of the list (via the CDR). |#

; = mapcan & mapcon =
#| MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the way they build 
up their result. While MAPCAR and MAPLIST build a completely new list to hold 
the results of the function calls, MAPCAN and MAPCON build their result by 
splicing together the results--which must be lists--as if by NCONC. Thus, each 
function invocation can provide any number of elements to be included in the 
result. MAPCAN, like MAPCAR, passes the elements of the list to the mapped 
function while MAPCON, like MAPLIST, passes the cons cells. |#

; = mapc & mapl =
#| MAPC and MAPL are control constructs disguised as functions--they simply 
return their first list argument, so they're useful only when the side effects 
of the mapped function do something interesting. MAPC is the cousin of MAPCAR 
and MAPCAN while MAPL is in the MAPLIST/MAPCON family.  |#