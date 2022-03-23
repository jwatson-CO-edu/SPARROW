#|
PCL-07--Cons-Composite.lisp
James Watson, 2012 January
Data structures composited from conses, other than lists
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

;; == Trees ==
#| The difference between a function that treats a bunch of cons cells as a list
and a function that treats the same bunch of cons cells as a tree has to do with
which cons cells the functions traverse to find the values of the list or 
tree. Tree structure is traversed by following both CAR and CDR references for 
as long as they point to other cons cells. The values in a tree are thus the 
atomic--non-cons-cell-values referenced by either the CARs or the CDRs of the 
cons cells in the tree structure. |#

#| For instance, the following box-and-arrow diagram shows the cons cells that 
make up the list of lists:  |#
((1 2) (3 4) (5 6)) #|

 Note: PCL Box & Arrow Diagrams differ slightly in that leaves are placed INSIDE
       boxes, where convenient, as opposed to the SICP convention of showing the
       arrow pointing to the place where the leaf value is stored.  (The latter
       is more correct, but takes up more space.)
    _____         _____         _____ 
 -->|*|*--------->|*|*--------->|*|/|     Original List
     |             |             |
    _v___  _____  _v___  _____  _v___  _____ 
    |1|*-->|2|/|  |3|*-->|4|/|  |5|*-->|6|/|   Sub-lists
     ^             ^             ^
    _|___         _|___         _|___    
 -->|*|*--------->|*|*--------->|*|/|    conses created by COPY-LIST, pointing
                                         to existing sub-lists

To see the difference between a list function and a tree function, you can 
consider how the functions COPY-LIST and COPY-TREE will copy this bunch of cons 
cells. 

; = copy-list =
COPY-LIST, as a list function, copies the cons cells that make up the 
list structure. That is, it makes a new cons cell corresponding to each of the 
cons cells inside the dashed box. The CARs of each of these new cons cells 
reference the same object as the CARs of the original cons cells in the list 
structure. Thus, COPY-LIST doesn't copy the sublists (1 2), (3 4), or (5 6)

    _____         _____         _____ 
 -->|*|*--------->|*|*--------->|*|/|     Original List
     |             |             |
    _v___  _____  _v___  _____  _v___  _____ 
    |1|*-->|2|/|  |3|*-->|4|/|  |5|*-->|6|/|   Sub-lists

    _____         _____         _____ 
 -->|*|*--------->|*|*--------->|*|/|   New list created by COPY-TREE
     |             |             |
    _v___  _____  _v___  _____  _v___  _____ 
    |1|*-->|2|/|  |3|*-->|4|/|  |5|*-->|6|/|  New sub-lists created as well

; = copy-tree =
COPY-TREE, on the other hand, makes a new cons cell for each of the cons cells 
in the diagram and links them together in the same structure. |# 

; = tree-equal =
#| TREE-EQUAL, which compares two trees (walking both the CARs and the CDRs as 
it goes), considering them equal if the tree structure is the same shape and if 
the leaves are EQL (or if they satisfy the test supplied with the :test keyword 
argument) |#

; = subst =
#| SUBST, like SUBSTITUTE, takes a new item, an old item, and a tree (as opposed
to a sequence), along with :key and :test keyword arguments, and it returns a 
new tree with the same shape as the original tree but with all instances of the 
old item replaced with the new item. |#
(subst 10 1 '(1 2 (3 2 1) ((1 1) (2 2)))) #|->|# (10 2 (3 2 10) ((10 10) (2 2)))

; = subst-if/-not =
#| SUBST-IF is analogous to SUBSTITUTE-IF. Instead of an old item, it takes a 
one-argument function--the function is called with each atomic value in the 
tree, and whenever it returns true, the position in the new tree is filled with 
the new value. SUBST-IF-NOT is the same except the values where the test returns
NIL are replaced. |#

; NSUBST, NSUBST-IF, and NSUBST-IF-NOT are the recycling versions of the SUBST 
;functions.


;; == Sets ==
#| Sets can also be implemented in terms of cons cells. In fact, you can treat 
any list as a set--Common Lisp provides several functions for performing set-
theoretic operations on lists. However, you should bear in mind that because of 
the way lists are structured, these operations get less and less efficient the 
bigger the sets get. (You can always replace the lists with sets built on top of
hash tables or bit vectors.)|#

; = adjoin =
#| To build up a set, you can use the function ADJOIN. ADJOIN takes an item and 
a list representing a set and returns a list representing the set containing the
item and all the items in the original set. To determine whether the item is pre
sent, it must scan the list; if the item isn't found, ADJOIN creates a new cons 
cell holding the item and pointing to the original list and returns it. 
Otherwise, it returns the original list. 

ADJOIN also takes :key and :test keyword arguments, which are used when 
determining whether the item is present in the original list. Like CONS, ADJOIN 
has no effect on the original list--if you want to modify a particular list, you
need to assign the value returned by ADJOIN to the place where the list came 
from. The modify macro PUSHNEW does this for you automatically. |#
(defparameter *set* ())
(adjoin 1 *set*) #|-->|# (1)
*set* #|-->|# NIL
(setf *set* (adjoin 1 *set*)) #|-->|# (1)
(pushnew 2 *set*) #|-->|#(2 1) ; Performs the same operation as previous exprssn
*set* #|-->|# (2 1)
(pushnew 2 *set*) #|-->|# (2 1) ; 2 already belongs to set, nothing added

; = member/-if/-not =
#| You can test whether a given item is in a set with MEMBER and the related 
functions MEMBER-IF and MEMBER-IF-NOT. These functions are similar to the 
sequence functions FIND, FIND-IF, and FIND-IF-NOT except they can be used only 
with lists. And instead of returning the item when it's present, they return the
cons cell containing the item--in other words, the sublist starting with the 
desired item. When the desired item isn't present in the list, all three 
functions return NIL. |#

; = Other Set Operations =
#| INTERSECTION, UNION, SET-DIFFERENCE, and SET-EXCLUSIVE-OR. Each of these 
functions takes two lists and :key and :test keyword arguments and returns a new
list representing the set resulting from performing the appropriate set-
theoretic operation on the two lists |#

; = subsetp =
#| takes two lists and the usual :key and :test keyword arguments and returns 
true if the first list is a subset of the second--if every element in the first 
list is also present in the second list. The order of the elements in the lists 
doesn't matter.  |#


;; == Lookup Tables ==
#| Two flavors of cons-based lookup tables are commonly used: 
association lists, also called alists, and property lists, also known as plists.
While you wouldn't use either alists or plists for large tables--for that you'd 
use a hash table--it's worth knowing how to work with them both because for 
small tables they can be more efficient than hash tables and because they have 
some useful properties of their own. |#

; = alists =
#| An alist is a data structure that maps keys to values and also supports 
reverse lookups, finding the key when given a value. Alists also support adding 
key/value mappings that shadow existing mappings in such a way that the 
shadowing mapping can later be removed and the original mappings exposed again.

Under the covers, an alist is essentially a list whose elements are themselves 
cons cells. Each element can be thought of as a key/value pair with the key in 
the cons cell's CAR and the value in the CDR. 
    _____  _____  _____ 
 -->|*|*-->|*|*-->|*|/|     
     |      |      |
    _v___  _v___  _v___   
    |A|1|  |B|2|  |C|3| 

The alist diagramed in the previous figure is printed like this: |#
((A . 1) (B . 2) (C . 3))

; - assoc -
#| The main lookup function for alists is ASSOC, which takes a key and an alist 
and returns the first cons cell whose CAR matches the key or NIL if no match is 
found. |#
(assoc 'a '((a . 1) (b . 2) (c . 3))) #|-->|# (A . 1)
(assoc 'c '((a . 1) (b . 2) (c . 3))) #|-->|# (C . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3))) #|-->|# NIL
#| To get the value corresponding to a given key, you simply pass the result of 
ASSOC to CDR. |#
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3)))) #|-->|# 1
#| By default the key given is compared to the keys in the alist using EQL, but 
you can change that with the standard combination of :key and :test keyword 
arguments. For instance, if you wanted to use string keys, you might write 
this: |#
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=) #|-->|# ("a" . 1)
; two strings with the same contents aren't necessarily EQL. 

; - assoc-if/-not -
#| ASSOC-IF and ASSOC-IF-NOT functions, which return the first key/value pair 
whose CAR satisfies (or not, in the case of ASSOC-IF-NOT) the test function 
passed in the place of a specific item.  |#

; - rassoc/-if/-not -
#| Functions for reverse-lookup: RASSOC, RASSOC-IF, and RASSOC-IF-NOT--work just
like the corresponding ASSOC functions except they use the value in the CDR of 
each element as the key, |#

; - acons -
; add a pair to the front of an alist
(acons 'new-key 'new-value alist)
; Which is equivalent to the following expression using CONS
(cons (cons 'new-key 'new-value) alist)

#| ACONS is a function and thus can't modify the place holding the alist it's 
passed. If you want to modify an alist, you need to write either this: |#
(setf alist (acons 'new-key 'new-value alist))
; or this:
(push (cons 'new-key 'new-value) alist)

; - copy-alist -
#| COPY-ALIST is similar to COPY-TREE except, instead of copying the whole tree 
structure, it copies only the cons cells that make up the list structure, plus 
the cons cells directly referenced from the CARs of those cells. In other words,
the original alist and the copy will both contain the same objects as the keys 
and values, even if those keys or values happen to be made up of cons cells. |#

; - pairlis -
#| build an alist from two separate lists of keys and values with the function 
PAIRLIS. The resulting alist may contain the pairs either in the same order as 
the original lists or in reverse order. |#
(pairlis '(a b c) '(1 2 3))
#| gives this --> |# ((C . 3) (B . 2) (A . 1))
   #| or this --> |# ((A . 1) (B . 2) (C . 3))

; = plists =
#| Structurally a plist is just a regular list with the keys and values as 
alternating values. For instance, a plist mapping A, B, and C, to 1, 2, and 3 is
simply the list (A 1 B 2 C 3). In boxes-and-arrows form, it looks like this:  
    _____  _____  _____  _____  _____  _____ 
 -->|A|*-->|1|*-->|B|*-->|2|*-->|C|*-->|3|/|    

However, plists are less flexible than alists. In fact, plists support only one 
fundamental lookup operation... |#

; - getf -
#| takes a plist and a key and returns the associated value or NIL if the key 
isn't found. GETF also takes an optional third argument, which will be returned 
in place of NIL if the key isn't found. 

GETF always uses EQ to test whether the provided key matches the keys in the 
plist. Consequently, you should never use numbers or characters as keys in a 
plist as the behavior of EQ for those types is essentially undefined. 
Practically speaking, the keys in a plist are almost always symbols, which makes
sense since plists were first invented to implement symbolic "properties," 
arbitrary mappings between names and values. |#

#| You can use SETF with GETF to set the value associated with a given key. SETF
also treats GETF a bit specially in that the first argument to GETF is treated 
as the place to modify. Thus, you can use SETF of GETF to add a new key/value 
pair to an existing plist. |#
(defparameter *plist* ())
*plist* #|-->|# NIL
(setf (getf *plist* :a) 1) #|-->|# 1
*plist* #|-->|# (:A 1)
(setf (getf *plist* :a) 2) #|-->|# 2
*plist* #|-->|# (:A 2)

; - remf -
#| To remove a key/value pair from a plist, you use the macro REMF, which sets 
the place given as its first argument to a plist containing all the key/value 
pairs except the one specified. It returns true if the given key was actually 
found. |#
(remf *plist* :a) #|-->|# T ; Using the same *plist* as above
*plist* #|-->|# NIL

; - get-properties -
#| GET-PROPERTIES makes it more efficient to extract multiple values from a 
single plist. It takes a plist and a list of keys to search for and returns, as 
multiple values, the first key found, the corresponding value, and the head of 
the list starting with the found key. This allows you to process a property 
list, extracting the desired properties, without continually rescanning from the
front of the list. |#

#| The last special thing about plists is the relationship they have with 
symbols: every symbol object has an associated plist that can be used to store 
information about the symbol. The plist can be obtained via the function 
SYMBOL-PLIST. However, you rarely care about the whole plist; more often you'll 
use the functions GET, which takes a symbol and a key and is shorthand for a 
GETF of the same key in the symbols SYMBOL-PLIST. |#
(get 'symbol 'key) === (getf (symbol-plist 'symbol) 'key)
#| Being able to attach arbitrary information to names is quite handy when doing
any kind of symbolic programming. |#

; - remprop -
#| To remove a property from a symbol's plist, you can use either REMF of SYMBOL
-PLIST or the convenience function REMPROP. |#
(remprop 'symbol 'key) === (remf (symbol-plist 'symbol key))


;; == The destructuring-bind Macro ==
#| provides a way to destructure arbitrary lists, similar to the way macro 
parameter lists can take apart their argument list. The basic skeleton of a 
DESTRUCTURING-BIND is as follows: |#
(destructuring-bind (parameter*) list
  body-form*)
#| The parameter list can include any of the types of parameters supported in 
macro parameter lists such as &optional, &rest, and &key parameters.5 And, as in
macro parameter lists, any parameter can be replaced with a nested destructuring
parameter list, which takes apart the list that would otherwise have been bound 
to the replaced parameter. The list form is evaluated once and should return a 
list, which is then destructured and the appropriate values are bound to the 
variables in the parameter list. Then the body-forms are evaluated in order with
those bindings in effect. Some simple examples follow:  |#
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z)) ; --> 
(:X 1 :Y 2 :Z 3)

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z)) ; --> 
(:X 1 :Y (2 20) :Z 3)

; Flattening a tree
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; --> 
(:X 1 :Y1 2 :Y2 20 :Z 3)

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; --> 
(:X 1 :Y1 2 :Y2 NIL :Z 3)

(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ; --> 
(:X 3 :Y 2 :Z 1)

#| The &whole parameter, if specified, must be the first parameter in a 
parameter list, and it's bound to the whole list form.6 After a &whole 
parameter, other parameters can appear as usual and will extract specific parts 
of the list just as they would if the &whole parameter weren't there. An example
of using &whole with DESTRUCTURING-BIND looks like this: |#
(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole)) ; -->
(:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))