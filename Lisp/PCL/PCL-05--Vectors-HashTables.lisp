#|
PCL-05--Vectors-HashTables.lisp
James Watson, 2012 January
Working with Collections: Vectors and Hash Tables
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

;; == Vectors ==
#| Vectors are Common Lisp's basic integer-indexed collection, and they come in 
two flavors. Fixed-size vectors are a lot like arrays in a language such as 
Java: a thin veneer over a chunk of contiguous memory that holds the vector's 
elements. Resizable vectors, on the other hand, abstract the actual storage, 
allowing the vector to grow and shrink as elements are added and removed. |#

; = vector =
; You can make fixed-size vectors with the function VECTOR
(vector)     #| --> |# #()
(vector 1)   #| --> |# #(1)
(vector 1 2) #| --> |# #(1 2)
#| The #(...) syntax is the literal notation for vectors used by the Lisp 
printer and reader. This syntax allows you to save and restore vectors by 
PRINTing them out and READing them back in. You can use the #(...) syntax to 
include literal vectors in your code, but as the effects of modifying literal 
objects aren't defined, you should always use VECTOR or the more general 
function MAKE-ARRAY to create vectors you plan to modify. |#

; = make-array =
#| MAKE-ARRAY is more general than VECTOR since you can use it to create arrays 
of any dimensionality as well as both fixed-size and resizable vectors. The one 
required argument to MAKE-ARRAY is a list containing the dimensions of the 
array. Since a vector is a one-dimensional array, this list will contain one 
number, the size of the vector. As a convenience, MAKE-ARRAY will also accept a 
plain number in the place of a one-item list. With no other arguments, 
MAKE-ARRAY will create a vector with uninitialized elements. To create a vector 
with the elements all set to a particular value, you can pass an 
:initial-element argument. Thus, to make a five-element vector with its elements
initialized to NIL, you can write the following: |#
(make-array 5 :initial-element nil) #| --> |# #(NIL NIL NIL NIL NIL)

#| MAKE-ARRAY is also the function to use to make a resizable vector. A 
resizable vector is a slightly more complicated object than a fixed-size vector;
in addition to keeping track of the memory used to hold the elements and the 
number of slots available, a resizable vector also keeps track of the number of 
elements actually stored in the vector. This number is stored in the vector's 
fill pointer, so called because it's the index of the next position to be filled
when you add an element to the vector. |#
(make-array 5 :fill-pointer 0) #| --> |# #() ; Appears empty b/c 
                                             ;fill-pointer is 0

; = vector-push & vector-pop =
#| To add an element to the end of a resizable vector, you can use the function 
VECTOR-PUSH. It adds the element at the current value of the fill pointer and 
then increments the fill pointer by one, returning the index where the new 
element was added. The function VECTOR-POP returns the most recently pushed 
item, decrementing the fill pointer in the process. |#
(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*) #| --> |# 0
                 *x* #| --> |# #(A)
(vector-push 'b *x*) #| --> |# 1
                 *x* #| --> |# #(A B)
(vector-push 'c *x*) #| --> |# 2
                 *x* #| --> |# #(A B C)
    (vector-pop *x*) #| --> |# C
                 *x* #| --> |# #(A B)
    (vector-pop *x*) #| --> |# B
                 *x* #| --> |# #(A)
    (vector-pop *x*) #| --> |# A
                 *x* #| --> |# #()
#| However, even a vector with a fill pointer is limited to the size specified.
To make an arbitrarily resizable vector, you need to pass MAKE-ARRAY another key
word argument: :adjustable. |#
(make-array 5 :fill-pointer 0 :adjustable t) #| --> |# #()
#| To add elements to an adjustable vector, you use VECTOR-PUSH-EXTEND, which 
works just like VECTOR-PUSH except it will automatically expand the array if you
try to push an element onto a full vector |#

; = Vector Sub-Types =
#| It's also possible to create specialized vectors that are restricted to 
holding certain types of elements. One reason to use specialized vectors is they
may be stored more compactly and can provide slightly faster access to their 
elements than general vectors. |#

#| Strings are vectors specialized to hold characters. Strings are important 
enough to get their own read/print syntax (double quotes) and a set of string-
specific functions. Hpwever, because they're also vectors, all the functions 
that take vector arguments can also be used with strings. 

Literal strings, such as "foo", are like literal vectors written with the #() 
syntax--their size is fixed, and they must not be modified. However, you can use
MAKE-ARRAY to make resizable strings by adding the keyword argument 
:element-type. This argument takes a type descriptor, and you can create a 
string by passing the symbol CHARACTER to it. Note that you need to quote the 
symbol to prevent it from being treated as a variable name. For example, to make
an initially empty but resizable string, you can write this: |#
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) #|-->|# ""

#| Bit vectors have a special read/print syntax that looks like #*00001111 and a
fairly large library of functions, which I won't discuss, for performing 
bit-twiddling operations such as "anding" together two bit arrays. The type 
descriptor to pass as the :element-type to create a bit vector is the symbol 
BIT. |#

; = Vectors as Sequences =
#| Vectors and lists are the two concrete subtypes of the abstract type 
sequence. Therefore functions that accept valid sequences as arguments should
have the capacity to handle both vectors and lists. |#

; - length -
; returns the length of a sequence

; - elt (element) -
#| Takes a sequence and an integer index between zero (inclusive) and the length
of the sequence (exclusive) and returns the corresponding element. ELT will 
signal an error if the index is out of bounds. |#
(defparameter *x* (vector 1 2 3))
(length *x*) #| --> |# 3
(elt *x* 0)  #| --> |# 1
(elt *x* 1)  #| --> |# 2
(elt *x* 2)  #| --> |# 3
(elt *x* 3)  #| --> |# error
#| Like LENGTH, ELT treats a vector with a fill pointer as having the length 
specified by the fill pointer. ELT is also a SETFable place, so you can set the 
value of a particular element like this: |#
(setf (elt *x* 0) 10) ; Using *x* from above
*x* #| --> |# #(10 2 3)

; = Sequence Iterating Functions =
#| While in theory all operations on sequences boil down to some combination of 
LENGTH, ELT, and SETF of ELT operations, Common Lisp provides a large library of
sequence functions. |#

; - COUNT -
; Args: Item and sequence
; Return: Number of times item appears in sequence
(count 1 #(1 2 1 2 3 1 2 3 4)) #| --> |# 3

; - FIND -
; Args: Item and sequence
; Return: Item or NIL
(find 1 #(1 2 1 2 3 1 2 3 4))  #| --> |# 1
(find 10 #(1 2 1 2 3 1 2 3 4)) #| --> |# NIL

; - POSITION -
; Args: Item and sequence
; Return: Index into sequence or NIL
(position 1 #(1 2 1 2 3 1 2 3 4)) #| --> |# 0

; - REMOVE -
; Args: Item and sequence
; Return: Sequence with instances of item removed
(remove 1 #(1 2 1 2 3 1 2 3 4)) #| --> |# #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4)) #| --> |# (2 2 3 2 3 4)
(remove #\a "foobarbaz")        #| --> |# "foobrbz"

; - SUBSTITUTE -
; Args: New item, item, and sequence
; Return: Sequence with instances of item replaced with new item
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) #| --> |# #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) #| --> |# (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz")       #| --> |# "fooxarxaz"

#| REMOVE and SUBSTITUTE always return a sequence of the same type as their 
sequence argument. |#

#| You can modify the behavior of these five functions in a variety of ways 
using keyword arguments. You can change the criteria by which the functions 
regard elements as equal in two ways: First, you can use the :test keyword to 
pass a function that accepts two arguments and returns a boolean. If provided, 
it will be used to compare item to each element instead of the default object 
equality test, EQL. Second, with the :key keyword you can pass a one-argument 
function to be called on each element of the sequence to extract a key value, 
which will then be compared to the item in the place of the element itself. 
Note, however, that functions such as FIND that return elements of the sequence 
continue to return the actual element, not just the extracted key. |#
(count "foo" #("foo" "bar" "baz") :test #'string=) #| --> |# 1
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) #| --> |# (C 30)

#| To limit the effects of these functions to a particular subsequence of the 
sequence argument, you can provide bounding indices with :start and :end 
arguments. Passing NIL for :end or omitting it is the same as specifying the 
length of the sequence. |#

#| If a non-NIL :from-end argument is provided, then the elements of the 
sequence will be examined in reverse order. By itself :from-end can affect the 
results of only FIND and POSITION. For instance: |#
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first) #| --> |# (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) #|-->|# (A 30)

#| However, the :from-end argument can affect REMOVE and SUBSTITUTE in 
conjunction with another keyword parameter, :count, that's used to specify how m
any elements to remove or substitute. If you specify a :count lower than the 
number of matching elements, then it obviously matters which end you start 
from: |#
(remove #\a "foobarbaz" :count 1)             #|-->|# "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t) #|-->|# "foobarbz"
#| And while :from-end can't change the results of the COUNT function, it does 
affect the order the elements are passed to any :test and :key functions, which 
could possibly have side effects. |#


; = Sequence Iterating with Higher-Order Functions =
#| For each of the functions just discussed, Common Lisp provides two higher-
order function variants that, in the place of the item argument, take a function
to be called on each element of the sequence. One set of variants are named the 
same as the basic function with an -IF appended. These functions count, find, 
remove, and substitute elements of the sequence for which the function argument 
returns true. The other set of variants are named with an -IF-NOT suffix and 
count, find, remove, and substitute elements for which the function argument 
does not return true. |#
(count-if #'evenp #(1 2 3 4 5)) #|-->|# 2
(count-if-not #'evenp #(1 2 3 4 5)) #|-->|# 3
(position-if #'digit-char-p "abcd0001") #|-->|# 4
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
  #("foo" "bar" "baz" "foom"))
#|-->|# #("foo" "foom")

#| According to the language standard, the -IF-NOT variants are deprecated. 
However, that deprecation is generally considered to have itself been ill-
advised. 

With a :key argument, the value extracted by the :key function is passed to the 
function instead of the actual element.|#


; = Whole Sequence Manipulations =
#| COPY-SEQ and REVERSE each take a single argument, a sequence, and each 
returns a new sequence of the same type. The sequence returned by COPY-SEQ 
contains the same elements as its argument while the sequence returned by 
REVERSE contains the same elements but in reverse order. Note that neither 
function copies the elements themselves--only the returned sequence is a new 
object. 

The CONCATENATE function creates a new sequence containing the concatenation of 
any number of sequences. However, unlike REVERSE and COPY-SEQ, which simply 
return a sequence of the same type as their single argument, CONCATENATE must be
told explicitly what kind of sequence to produce in case the arguments are of 
different types. |#
(concatenate 'vector #(1 2 3) '(4 5 6))    #|-->|# #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))      #|-->|# (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) #|-->|# "abcdef"

; = Sorting and Merging =
#| The functions SORT and STABLE-SORT provide two ways of sorting a sequence. 
They both take a sequence and a two-argument predicate and return a sorted 
version of the sequence. |#
(sort (vector "foo" "bar" "baz") #'string<) ==> #("bar" "baz" "foo")
#| The difference is that STABLE-SORT is guaranteed to not reorder any elements 
considered equivalent by the predicate while SORT guarantees only that the 
result is sorted and may reorder equivalent elements. |#

#| Both these functions are examples of what are called destructive functions. 
Destructive functions are allowed--typically for reasons of efficiency--to 
modify their arguments in more or less arbitrary ways. This has two 
implications: one, you should always do something with the return value of these
functions (such as assign it to a variable or pass it to another function), and,
two, unless you're done with the object you're passing to the destructive 
function, you should pass a copy instead. 

You need to remember to write the following: |#
(setf my-sequence (sort my-sequence #'string<))
; rather than just this:
(sort my-sequence #'string<)

#| The MERGE function takes two sequences and a predicate and returns a sequence
produced by merging the two sequences, according to the predicate. It's related 
to the two sorting functions in that if each sequence is already sorted by the 
same predicate, then the sequence returned by MERGE will also be sorted. Like 
the sorting functions, MERGE takes a :key argument. Like CONCATENATE, and for 
the same reason, the first argument to MERGE must be a type descriptor 
specifying the type of sequence to produce. |#
(merge 'vector #(1 3 5) #(2 4 6) #'<) #|-->|# #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<) #|-->|# (1 2 3 4 5 6)

; = Sub-sequence Manipulations =
; - subseq -
#| Extracts a subsequence starting at a particular index and continuing to a 
particular ending index or the end of the sequence. For instance: |#
(subseq "foobarbaz" 3)   #|-->|# "barbaz"
(subseq "foobarbaz" 3 6) #|-->|# "bar"
#| SUBSEQ is also SETFable, but it won't extend or shrink a sequence; if the new
value and the subsequence to be replaced are different lengths, the shorter of 
the two determines how many characters are actually changed. |#

; - fill -
#| You can use the FILL function to set multiple elements of a sequence to a 
single value. The required arguments are a sequence and the value with which to 
fill it. By default every element of the sequence is set to the value; :start 
and :end keyword arguments can limit the effects to a given subsequence. |#

; - search -
#| If you need to find a subsequence within a sequence, the SEARCH function 
works like POSITION except the first argument is a sequence rather than a single
item.  |#
(position #\b "foobarbaz") #|-->|# 3
(search "bar" "foobarbaz") #|-->|# 3

; - mismatch -
#| To find where two sequences with a common prefix first diverge, you can use 
the MISMATCH function. It takes two sequences and returns the index of the first
pair of mismatched elements. |#
(mismatch "foobarbaz" "foom") #|-->|# 3
#| It returns NIL if the strings match. MISMATCH also takes many of the standard
keyword arguments: a :key argument for specifying a function to use to extract 
the values to be compared; a :test argument to specify the comparison function; 
and :start1, :end1, :start2, and :end2 arguments to specify subsequences within 
the two sequences. And a :from-end argument of T specifies the sequences should 
be searched in reverse order, causing MISMATCH to return the index, in the first
sequence, where whatever common suffix the two sequences share begins. |#

; = Sequence Predicates =
#| EVERY, SOME, NOTANY, and NOTEVERY, which iterate over sequences testing a 
boolean predicate. The first argument to all these functions is the predicate, 
and the remaining arguments are sequences. The predicate should take as many 
arguments as the number of sequences passed. The elements of the sequences are 
passed to the predicate--one element from each sequence--until one of the 
sequences runs out of elements or the overall termination test is met: |#

; - every -
#| EVERY terminates, returning false, as soon as the predicate fails. If the 
predicate is always satisfied, it returns true. |#
(every #'evenp #(1 2 3 4 5)) #|-->|# NIL
(every #'> #(1 2 3 4) #(5 4 3 2)) #|-->|# NIL

; - some -
#| returns the first non-NIL value returned by the predicate or returns false if
the predicate is never satisfied. |#
(some #'evenp #(1 2 3 4 5)) #|-->|# T
(some #'> #(1 2 3 4) #(5 4 3 2)) #|-->|# T

; - notany -
#| returns false as soon as predicate is satisfied or true if it never is |#
(notany #'evenp #(1 2 3 4 5)) #|-->|# NIL
(notany #'> #(1 2 3 4) #(5 4 3 2)) #|-->|# NIL

; - notevery -
#| returns true as soon as the predicate fails or false if the predicate is 
always satisfied. |#
(notevery #'evenp #(1 2 3 4 5)) #|-->|# T
(notevery #'> #(1 2 3 4) #(5 4 3 2)) #|-->|# T

; == Sequence Mapping ==

; = map =
#| MAP, like the sequence predicate functions, takes a n-argument function and n
sequences. But instead of a boolean value, MAP returns a new sequence containing
the result of applying the function to subsequent elements of the sequences. 
Like CONCATENATE and MERGE, MAP needs to be told what kind of sequence to 
create. |#
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) #|-->|# #(10 18 24 28 30)

; = map-into =
#| like MAP except instead of producing a new sequence of a given type, it 
places the results into a sequence passed as the first argument. This sequence 
can be the same as one of the sequences providing values for the function. For 
instance, to sum several vectors--a, b, and c--into one, you could write 
this: |#
(map-into a #'+ a b c)
#| If the sequences are different lengths, MAP-INTO affects only as many 
elements as are present in the shortest sequence, including the sequence being 
mapped into. However, if the sequence being mapped into is a vector with a fill 
pointer, the number of elements affected isn't limited by the fill pointer but 
rather by the actual size of the vector. After a call to MAP-INTO, the fill 
pointer will be set to the number of elements mapped. MAP-INTO won't, however, 
extend an adjustable vector. |#

; = reduce =
#| maps over a single sequence, applying a two-argument function first to the 
first two elements of the sequence and then to the value returned by the 
function and subsequent elements of the sequence. Thus, the following expression
sums the numbers from one to ten: |#
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) #|-->|# 55
; to find the maximum value in a sequence of numbers, you can write 
 (reduce #'max #(1 2 3 4 5)) #|-->|# 5
#| REDUCE also takes a full complement of keyword arguments (:key, :from-end, 
:start, and :end) and one unique to REDUCE (:initial-value). The latter 
specifies a value that's logically placed before the first element of the 
sequence (or after the last if you also specify a true :from-end argument). |#

;; == Hash Tables ==
#| The other general-purpose collection provided by Common Lisp is the hash 
table. Where vectors provide an integer-indexed data structure, hash tables 
allow you to use arbitrary objects as the indexes, or keys. When you add a value
to a hash table, you store it under a particular key. Later you can use the same
key to retrieve the value. Or you can associate a new value with the same key--
each key maps to a single value. |#

; = make-hash-table =
#| With no arguments MAKE-HASH-TABLE makes a hash table that considers two keys 
equivalent if they're the same object according to EQL. This is a good default 
unless you want to use strings as keys, since two strings with the same contents
aren't necessarily EQL. In that case you'll want a so-called EQUAL hash table, 
which you can get by passing the symbol EQUAL as the :test keyword argument to 
MAKE-HASH-TABLE. Two other possible values for the :test argument are the 
symbols EQ and EQUALP. These are, of course, the names of the standard object 
comparison functions. 

MAKE-HASH-TABLE's :test can't be used to specify an arbitrary function--only 
the values EQ, EQL, EQUAL, and EQUALP. This is because hash tables actually need
two functions, an equivalence function and a hash function that computes a 
numerical hash code from the key in a way compatible with how the equivalence 
function will ultimately compare two keys. However, although the language 
standard provides only for hash tables that use the standard equivalence 
functions, most implementations provide some mechanism for defining custom hash 
tables. |#

; = gethash =
#| provides access to the elements of a hash table. It takes two arguments--a 
key and the hash table--and returns the value, if any, stored in the hash table 
under that key or NIL. For example:  |#
(defparameter *h* (make-hash-table))
(gethash 'foo *h*) #|-->|# NIL
(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*) #|-->|# QUUX

#| Since GETHASH returns NIL if the key isn't present in the table, there's no 
way to tell from the return value the difference between a key not being in a 
hash table at all and being in the table with the value NIL. GETHASH solves this
problem with a feature I haven't discussed yet--multiple return values. GETHASH 
actually returns two values; the primary value is the value stored under the 
given key or NIL. The secondary value is a boolean indicating whether the key is
present in the hash table. Because of the way multiple values work, the extra 
return value is silently discarded unless the caller explicitly handles it 
with a form that can "see" multiple values. |#

; = Hash Table Iteration =
; - maphash -
#| MAPHASH takes a two-argument function and a hash table and invokes the 
function once for each key/value pair in the hash table. For instance, to print 
all the key/value pairs in a hash table, you could use MAPHASH like this: |#
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
#| The consequences of adding or removing elements from a hash table while 
iterating over it aren't specified (and are likely to be bad) with two 
exceptions: you can use SETF with GETHASH to change the value of the current 
entry, and you can use REMHASH to remove the current entry. For instance, to 
remove all the entries whose value is less than ten, you could write this: |#
(maphash #'(lambda (k v) (when (< v 10) (remhash k *h*))) *h*)

;The LOOP equivalent of the first MAPHASH expression would look like this:
(loop for k being the hash-keys in *h* using (hash-value v)
  do (format t "~a => ~a~%" k v))