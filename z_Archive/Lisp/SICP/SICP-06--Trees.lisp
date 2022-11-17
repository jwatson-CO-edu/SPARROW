#|
SICP-06--Trees.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Working with trees of symbols, with application to representing sets
|#

; = Sets as Binary Trees =
#| We can do better than the ordered-list representation by arranging the set 
elements in the form of a tree. Each node of the tree holds one element of the 
set, called the "entry" at that node, and a link to each of two other (possibly 
empty) nodes. The "left" link points to elements smaller than the one at the 
node, and the "right" link to elements greater than the one at the node.  

The advantage of the tree representation is this: Suppose we want to check 
whether a number x is contained in a set. We begin by comparing x with the entry
in the top node. If x is less than this, we know that we need only search the 
left subtree; if x is greater, we need only search the right subtree. Now, if 
the tree is "balanced", each of these subtrees will be about half the size of 
the original. Thus, in one step we have reduced the problem of searching a tree 
of size n to searching a tree of size n/2. Since the size of the tree is halved 
at each step, we should expect that the number of steps needed to search a tree 
of size n grows as O(log n). 

We can represent trees by using lists. Each node will be a list of three items: 
the entry at the node, the left subtree, and the right subtree. A left or a 
right subtree of the empty list will indicate that there is no subtree connected
there. We can describe this representation by the following procedures: |#

(defun entry (tree) 
  "Return the entry at the TREE root"
  (car tree))

(defun left-branch (tree) 
  "Return the left branch of TREE"
  (cadr tree))

(defun right-branch (tree) 
  "Return the right branch of TREE"
  (caddr tree))

(defun make-tree (entry left right)
  "Creates a tree from an ENTRY, LEFT branch, and RIGHT branch"
  (list entry left right))

(defun element-of-set? (x set)
  "Determine if X is a member of SET, a binary tree"
  (cond ((null set) nil)
        ((= x (entry set)) t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

#| Adjoining an item to a set is implemented similarly and also requires 
O(log n) steps. To adjoin an item x, we compare x with the node entry to 
determine whether x should be added to the right or to the left branch, and 
having adjoined x to the appropriate branch we piece this newly constructed 
branch together with the original entry and the other branch. If x is equal to 
the entry, we just return the node. If we are asked to adjoin x to an empty 
tree, we generate a tree that has x as the entry and empty right and left 
branches. Here is the procedure: |#

(defun adjoin-set (x set)
  (cond ((null set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

#| The above claim that searching the tree can be performed in a logarithmic 
number of steps rests on the assumption that the tree is "balanced", i.e., 
that the left and the right subtree of every tree have approximately the same 
number of elements, so that each subtree contains about half the elements of its
parent. 

But how can we be certain that the trees we construct will be balanced? For 
example, if we start with an empty set and adjoin the numbers 1 through 7 in 
sequence we end up with the highly unbalanced tree; in this tree all the left 
subtrees are empty. One way to solve this problem is to define an operation that
transforms an arbitrary tree into a balanced tree with the same elements. Then 
we can perform this transformation after every few adjoin-set operations to keep
our set in balance. There are also other ways to solve this problem, most of 
which involve designing new data structures for which searching and insertion 
both can be done in O(log n) steps. |#

;; == Huffman Encoding Trees ==
#| The application is to methods for representing data as sequences of ones and 
zeros (bits). 

If all our messages are made up of the eight symbols A, B, C, D, E, F, G, and H,
we can choose a code with three bits per character, for example

A 000	C 010	E 100	G 110
B 001	D 011	F 101	H 111

Codes such as ASCII and the A-through-H code above are known as fixed-length 
codes, because they represent each symbol in the message with the same number of
bits. It is sometimes advantageous to use variable-length codes, in which 
different symbols may be represented by different numbers of bits. In general, 
if our messages are such that some symbols appear very frequently and some very 
rarely, we can encode data more efficiently (i.e., using fewer bits per message)
if we assign shorter codes to the frequent symbols.

One of the difficulties of using a variable-length code is knowing when you have
reached the end of a symbol in reading a sequence of zeros and ones. This can be
solved by having a special separator code. Another solution is to design the 
code in such a way that no complete code for any symbol is the beginning (or 
prefix) of the code for another symbol. Such a code is called a prefix code. In 
the example above, A is encoded by 0 and B is encoded by 100, so no other symbol
can have a code that begins with 0 or with 100. 

A Huffman code can be represented as a binary tree whose leaves are the symbols 
that are encoded. At each non-leaf node of the tree there is a set containing 
all the symbols in the leaves that lie below the node. In addition, each symbol 
at a leaf is assigned a weight (which is its relative frequency), and each 
non-leaf node contains a weight that is the sum of all the weights of the leaves
lying below it. The weights are not used in the encoding or the decoding 
process. 

Given a Huffman tree, we can find the encoding of any symbol by starting at the 
root and moving down until we reach the leaf that holds the symbol. Each time we
move down a left branch we add a 0 to the code, and each time we move down a 
right branch we add a 1. (We decide which branch to follow by testing to see 
which branch either is the leaf node for the symbol or contains the symbol in 
its set.) 

To decode a bit sequence using a Huffman tree, we begin at the root and use the 
successive zeros and ones of the bit sequence to determine whether to move down 
the left or the right branch. Each time we come to a leaf, we have generated a 
new symbol in the message, at which point we start over from the root of the 
tree to find the next symbol. |#

; = Generating Huffman Trees =
#| The algorithm for generating a Huffman tree is very simple. The idea is to 
arrange the tree so that the symbols with the lowest frequency appear farthest 
away from the root. Begin with the set of leaf nodes, containing symbols and 
their frequencies, as determined by the initial data from which the code is to 
be constructed. Now find two leaves with the lowest weights and merge them to 
produce a node that has these two nodes as its left and right branches. The 
weight of the new node is the sum of the two weights. Remove the two leaves from
the original set and replace them by this new node. Now continue this process. 
At each step, merge two nodes with the smallest weights, removing them from the 
set and replacing them with a node that has these two as its left and right 
branches. The process stops when there is only one node left, which is the root 
of the entire tree. |#

; = Representing Huffman Trees =
#| Leaves of the tree are represented by a list consisting of the symbol leaf, 
the symbol at the leaf, and the weight: |#

(defun make-leaf (symbol weight)
  "Creates a leaf for a Huffman tree"
  (list 'leaf symbol weight))

(defun leaf? (object)
  "Returns true if OBJECT is a leaf"
  (eql (car object) 'leaf))

(defun symbol-leaf (x) 
  "Return the symbol stored at node X"
  (cadr x))

(defun weight-leaf (x) 
  "Return the weight at node X"
  (caddr x))

#| Since our symbol sets are represented as lists, we can form the union by 
using the append procedure define earlier. |#

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; If we make a tree in this way, we have the following selectors:

(defun left-branch (tree) 
  "Return the left branch of a Huffman TREE"
  (car tree))

(defun right-branch (tree) 
  "Return the right branch of a Huffman TREE"
  (cadr tree))

#| The procedures symbols and weight must do something slightly different 
depending on whether they are called with a leaf or a general tree. |#

(defun symbols (tree)
  "Return the symbols of a Huffman TREE"
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(defun weight (tree)
  "Return the weight of a Huffman TREE"
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; = The Decoding Procedure =
#| The following procedure implements the decoding algorithm. It takes as 
arguments a list of zeros and ones, together with a Huffman tree. 

The procedure decode-1 takes two arguments: the list of remaining bits and the 
current position in the tree. It keeps moving "down" the tree, choosing a left 
or a right branch according to whether the next bit in the list is a zero or a 
one. (This is done with the procedure choose-branch.) When it reaches a leaf, it
returns the symbol at that leaf as the next symbol in the message by consing it 
onto the result of decoding the rest of the message, starting at the root of the
tree. Note the error check in the final clause of choose-branch, which complains
if the procedure finds something other than a zero or a one in the input data.|#

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(defun decode (bits tree)
  (labels (decode-1 bits current-branch)
    (if (null bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; = Sets of Weighted Elements =
#| Since we will be required to repeatedly find the smallest item in a set, it 
is convenient to use an ordered representation for this kind of set. We will 
represent a set of leaves and trees as a list of elements, arranged in 
increasing order of weight. In the following adjoin-set procedure for 
constructing sets, items are compared by their weights, and the element being 
added to the set is never already in it. |#

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set)
                    (adjoin-set x (cdr set))))))

#| The following procedure takes a list of symbol-frequency pairs such as 
 ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves, 
ready to be merged according to the Huffman algorithm: |#

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))
