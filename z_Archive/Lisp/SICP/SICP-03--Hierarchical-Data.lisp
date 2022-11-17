#|
SICP-03--Hierarchical-Data.lisp
James Watson, 2011 December

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Data structures composed of cons'
|#

; = Box and Pointer Notation =
#| In this representation, which is called box-and-pointer notation, each object
is shown as a pointer to a box. The box for a primitive object contains a 
representation of the object. For example, the box for a number contains a 
numeral. The box for a pair is actually a double box, the left part containing 
 (a pointer to) the car of the pair and the right part containing the cdr. 
      _____   ___
  --> |*|*--->|2|      
       |         
      _v_    
      |1|         
   _____   _____   ___
-->|*|*--->|*|*--->|4|
    |       |   
  __v__    _v_
  |*|*|    |3|
  /   \
_v_   _v_
|1|   |2|  |#

; = Representing Sequences =
#| One of the useful structures we can build with pairs is a sequence -- an 
ordered collection of data objects. There are, of course, many ways to 
represent sequences in terms of pairs. One particularly straightforward 
representation has the sequence 1, 2, 3, 4 represented as a chain of pairs, as 
seen below. 
    _____  _____   _____   _____
 -->|*|*-->|*|*--->|*|*--->|*|/|
     |      |       |       |
    _v_    _v_     _v_     _v_
    |1|    |2|     |3|     |4| 

The car of each pair is the corresponding item in the chain, and the cdr of the 
pair is the next pair in the chain. The cdr of the final pair signals the end of
the sequence by pointing to a distinguished value that is not a pair, 
represented in box-and-pointer diagrams as a diagonal line and in programs as 
the value of the variable nil. The entire sequence is constructed by nested cons
operations: |# 
(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))
#| Common Lisp provides a facility to contruct just such a sequence 
automatically with the list object, created with the built-in LIST function.|#
(defvar one-through-four (list 1 2 3 4))
#| Be careful not to confuse the expression (list 1 2 3 4) with the list 
 (1 2 3 4), which is the result obtained when the expression is evaluated. 
Attempting to evaluate the expression (1 2 3 4) will signal an error when the 
interpreter tries to apply the procedure 1 to arguments 2, 3, and 4.

A proper list is a chain of conses terminated by the empty list, (), which is 
itself a proper list. A dotted list is a list which has a terminating atom that 
is not the empty list. A circular list is a chain of conses that has no 
termination because some cons in the chain is the cdr of a later cons. |#
; HYPERSPEC: http://www.lispworks.com/documentation/HyperSpec/Body/t_list.htm 

; = List Operations =
#| The use of pairs to represent sequences of elements as lists is accompanied 
by conventional programming techniques for manipulating lists by successively 
"cdr-ing down" the lists. |#
(defun list-ref(items n)
  "Access list with zero-based indices"
   (if (= n 0) ; If at the correct index
       (car items) ; car the cons at this index
       (list-ref (cdr items) (- n 1)))) ; otherwise recrusively cdr w/ index - 1
 
(defvar squares (list 1 4 9 16 25))
(list-ref squares 3) #| --> |# 16

(defun length-sicp(items) ; LENGTH already defined in Common Lisp
   (if (null items) ; If items has a value of nil
       0 ; then add zero to total items, stop
       (+ 1 (length (cdr items))))) ; Otherwise add 1 to total and move to next
; Alternatively, LENGTH-SICP could be defined iteratively
(length-sicp squares) #| --> |# 5
; Common Lisp LENGTH also works on strings or any proper sequence, see hyperspec

; Lists may be onnect to one another with APPEND
(append (list 1 2) (list 3 4)) #| --> |# (1 2 3 4)

; = Mapping Over Lists =
#| One extremely useful operation is to apply some transformation to each 
element in a list and generate the list of results. For instance, the following 
procedure scales each number in a list by a given factor: |#
(defun scale-list(items factor)
   (if (null items) ; If item is nil
       nil ; then return nil 
       (cons (* (car items) factor) ; Otherwise add scaled cons item
             (scale-list (cdr items) factor)))) ; Construct next item at cdr

(scale-list (list 1 2 3 4 5) 10) #| --> |# (10 20 30 40 50)

#| This idea is a common pattern expressed with the function MAP |#
(map 'list (lambda(x) (* x x)) (list 1 2 3 4)) #| --> |# (1 4 9 16)
; (map <result-type> <function-to-apply> <list-to-transform>)

#| Map is an important construct, not only because it captures a common pattern,
but because it establishes a higher level of abstraction in dealing with lists. 
In effect, map helps establish an abstraction barrier that isolates the 
implementation of procedures that transform lists from the details of how the 
elements of the list are extracted and combined. |#

; = Hierarchical Structures =
#| The representation of sequences in terms of lists generalizes naturally to 
represent sequences whose elements may themselves be sequences. |#
(cons (list 1 2) (list 3 4)) #| --> |# ((1 2) 3 4)
#|                                       /     \ \__
                                     (1 2)      \   \
                                     /   \       3   4
                                    1     2           |#
#| This representation may not be immediately obvious until you consider that
Lisp structres a list as a series of conses licked by the cdr. |#

#| Another way to think of sequences whose elements are sequences is as trees. 
The elements of the sequence are the branches of the tree, and elements that are
themselves sequences are subtrees. Recursion is a natural tool for dealing with 
tree structures, since we can often reduce operations on trees to operations on 
their branches, which reduce in turn to operations on the branches of the 
branches, and so on, until we reach the leaves of the tree. |#

; Function to count the leaves on a tree
(defun count-leaves(x)
  "Count the leaves of (sub-) tree x, recursively"
  (cond ((null x) 0) ; If node is null, no leaves added
        ((not (consp x)) 1) ; If node is not a cons, it is a leaf, count 1
        (t (+ (count-leaves (car x)) ; If node is a cons, then count leaves
                 (count-leaves (cdr x)))))) ;of sub-trees car and cdr

(count-leaves (list 1 (list 2 (list 3 4)))) #| --> |# 4

; A tree-based map for applying a function to nested lists
(defun tree-map (func tree)
  (mapcar
    (lambda (subtree)
      (if (consp subtree) ; If subtree is a cons
        (tree-map func subtree) ; then call tree-map on it
        (funcall func subtree))) ; Otherwise, it is a leaf, apply FUNC
    tree))
; URL: http://eli.thegreenplace.net/2007/08/10/sicp-section-222/

#| MAPCAR operates on successive elements of the lists. FUNC is applied to the 
first element of each list, then to the second element of each list, and so on. 
The iteration terminates when the shortest list runs out, and excess elements in
other lists are ignored. The value returned by MAPCAR is a list of the results 
of successive calls to function. |#


; = Sequences as Conventional Interfaces =
#| A signal-processing engineer would find it natural to conceptualize processes
in terms of signals flowing through a cascade of stages, each of which 
implements part of the program plan. 

The key to organizing programs so as to more clearly reflect the signal-flow 
structure is to concentrate on the "signals" that flow from one stage in the 
process to the next. If we represent these signals as lists, then we can use 
list operations to implement the processing at each of the stages.|#

(defun filter(predicate sequence)
  "Filter a sequence to select only elements that satisfy a given predicate"
  (cond ((null sequence) nil) ; If arg is nil, then return nil
        ((funcall predicate (car sequence)) ; If predicate satisfied
         (cons (car sequence) ; then append item
               (filter predicate (cdr sequence)))) ; and filter remaining list
        (t (filter predicate (cdr sequence))))) ; Otherwise filter remaining
                                                ;list without appending current
(filter #'oddp (list 1 2 3 4 5)) #| --> |# (1 3 5)
      ; ^-- Signifies that we have passed a function name, ODDP

(defun accumulate(op initial sequence)
  "Perform an operation on the elements of a list and return the result"
  (if (null sequence) ; If sequence is nil
      initial ; Then return the initial value
      (funcall op (car sequence) ; Otherwise, sequence is non-nil,
          (accumulate op initial (cdr sequence))))) ; Perform OP on first elem
                                                    ;and the remaining elem
; Summing all the elements in a list with ACCUMULATE
(accumulate #'+ 0 (list 1 2 3 4 5)) #| --> |# 15
          ; ^-- Passed the addition function name + to OP

#| FILTER is just another name for the CL library function REMOVE-IF-NOT and 
ACCUMULATE can be trivially modeled with REDUCE |#

(defun enumerate-interval(low high)
  "Generate an integer sequence from LOW to HIGH"
  (if (> low high) ; If lower bound is greater than upper bound
      nil ; Then return NIL
      (cons low (enumerate-interval (+ low 1) high)))) ; Otherwise add element
                                                    ;and append next in sequence

(defun enumerate-tree(tree)
  "Flatten the leaves of a tree into a list, depth-first"
  (cond ((null tree) nil) ; If sub-tree is NIL, then return NIL
        ((not (consp tree)) (list tree)) ; If sub-tree is a leaf, wrap in list
        ; This is so that APPEND can be called on the leaf
        (t (append (enumerate-tree (car tree)) ; Otherwise is a cons, so
                   (enumerate-tree (cdr tree)))))) ; compose enumerated sub-
                                                   ;trees into a list
(enumerate-tree (list 1 (list 2 (list 3 4)) 5)) #| --> |# (1 2 3 4 5)

; The above can be chained together to apply successive processes to data
#| For sum-odd-squares, we enumerate the sequence of leaves of the tree, filter 
this to keep only the odd numbers in the sequence, square each element, and sum 
the results: |#
(defun sum-odd-squares(tree)
   (accumulate #'+
               0
               (map (lambda(x) (* x x))
                    (filter #'oddp
                            (enumerate-tree tree)))))
#| The value of expressing programs as sequence operations is that this helps us
make program designs that are modular, that is, designs that are constructed by 
combining relatively independent pieces. We can encourage modular design by 
providing a library of standard components together with a conventional 
interface for connecting the components in flexible ways. |#

; = Matrix Operations =
#| The procedure accumulate-n is similar to accumulate except that it takes as 
its third argument a sequence of sequences, which are all assumed to have the 
same number of elements. It applies the designated accumulation procedure to 
combine all the first elements of the sequences, all the second elements of the 
sequences, and so on, and returns a sequence of the results. |#
(defun accumulate-n(op init seqs)
  (if (null (car seqs))
    nil
    (cons (accumulate op init (mapcar #'car seqs))
          (accumulate-n op init (mapcar #'cdr seqs)))))
#| Suppose we represent vectors v = (v[i]) as sequences of numbers, and matrices
m = (m[i][j]) as sequences of vectors (the rows of the matrix). For example, the
matrix 
+-       -+  
| 1 2 3 4 |
| 4 5 6 6 |  is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9))
| 6 7 8 9 |
+-       -+  With this representation, we can use sequence operations to 
concisely express the basic matrix and vector operations. These operations are 
the following: |#
(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar
    (lambda(row)
      (dot-product row v))
    m))

(defun transpose (m)
  (accumulate-n #'cons nil m))

(defun matrix-*-matrix (m n)
  (let ((n-t (transpose n)))
    (mapcar (lambda(row) (matrix-*-vector n-t row)) m)))

#| The accumulate procedure is also known as fold-right, because it combines the
first element of the sequence with the result of combining all the elements to 
the right. There is also a fold-left, which is similar to fold-right, except 
that it combines elements working in the opposite direction: |#
(defun fold-right (op init seq)
  (if (null seq)
    init
    (funcall op
      (car seq)
      (fold-right op init (cdr seq)))))

(defun fold-left (op init seq)
  (labels (
    (iter (result rest)
      (if (null rest)
        result
        (iter (funcall op result (car rest))
              (cdr rest)))))
    (iter init seq)))