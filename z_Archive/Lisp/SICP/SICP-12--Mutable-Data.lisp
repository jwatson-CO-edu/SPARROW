#|
SICP-12--Mutable-Data.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Modeling with Mutable Data
|#

#| The desire to model systems composed of objects that have changing state 
leads us to the need to modify compound data objects, as well as to construct 
and select from them. In order to model compound objects with changing state, we
will design data abstractions to include, in addition to selectors and 
constructors, operations called mutators, which modify data objects. 

Earlier text introduced pairs as a general-purpose "glue" for synthesizing 
compound data. We begin this section by defining basic mutators for pairs, so 
that pairs can serve as building blocks for constructing mutable data objects. 
These mutators greatly enhance the representational power of pairs, enabling us 
to build data structures other than the sequences and trees. We also present 
some examples of simulations in which complex systems are modeled as collections
of objects with local state. |#


;; == Mutable List Structure ==

#| The basic operations on pairs -- cons, car, and cdr -- can be used to 
construct list structure and to select parts from list structure, but they are 
incapable of modifying list structure. The same is true of the list operations 
we have used so far, such as append and list, since these can be defined in 
terms of cons, car, and cdr. To modify list structures we need new 
operations. 

It is actually quite interesting to compare Scheme with Common Lisp in this 
respect, because they're a bit different. Scheme has a setter function for most 
types it supports, including pairs (set-car!, set-cdr!). On the other hand, CL 
has a common SETF macro which is useful for all types. It can be used to set 
variables, structure elements, array cells, pairs, and so on. |#

(defun set-car! (pair-name new-value)
  "Common Lisp implementation of Scheme's native SET-CAR!"
  (setf (car pair-name) new-value))

(defun set-cdr! (pair-name new-value)
  "Common Lisp implementation of Scheme's native SET-CDR!"
  (setf (cdr pair-name) new-value))

; SICP-APPEND forms a new list by successively consing the elements of x onto y
(defun SICP-append (x y)
  (if (null x)
    y
    (cons (car x) (SICP-append (cdr x) y))))

#| The procedure SICP-APPEND! is similar to append, but it is a mutator rather 
than a constructor. It appends the lists by splicing them together, modifying 
the final pair of x so that its cdr is now y. (It is an error to call 
SICP-APPEND! with an empty x.) |#
(defun SICP-append! (x y)
  (setf (cdr (last x)) y)
  x)
; LAST returns the last cons of the list X. Must not call on a circular list

#| This creates a circular list. The cdr of the last cell in the list, instead 
of pointing to nil, points to the first cell of the list. Now any attempt to 
walk the list or print it will result in an infinite loop. |#
(defun make-cycle (x)
  "Transform list X into a circular list, destructively"
  (setf (cdr (last x)) x)
  x)

; Reversing a list
(defun SICP-reverse (x)
  "Reverse list X, destructively"
  (labels ((my-loop (x y)
	     (if (null x)
		 y
		 (let ((temp (cdr x)))
		   (setf (cdr x) y)
		   (my-loop temp x)))))
    (my-loop x '())))


; = Sharing and Identity =

(defvar x (list 'a 'b))
(defvar z1 (cons x x)) #|
has the following structure
       _________
z1 --> | * | * |   Both cells of the z1 cons point to the same list 
       ---\-/---
         __V__  _____
 x -->   |*|*-->|*|/|
          |      |
          v      v
          a      b    

This sharing of x by the car and cdr of z1 is a consequence of the 
straightforward way in which cons is implemented. In general, using cons to 
construct lists will result in an interlinked structure of pairs in which many 
individual pairs are shared by many different structures.|#

(defvar z2 (cons (list 'a 'b) (list 'a 'b))) #|
has the following structure
       _____  _____  _____
z2 --> |*|*-->|*|*-->|*|/|
        |      |      |
        |      v      v
        |      a      b
        |      ^      ^
        |     _|___  _|___
        +---->|*|*-->|*|/|

In this structure, the pairs in the two (a b) lists are distinct, although the 
actual symbols are shared. 

In general, sharing is completely undetectable if we operate on lists using only
cons, car, and cdr. Both of the above appear to create ((A B) A B)
However, if we allow mutators on list structure, sharing becomes significant. As
an example of the difference that sharing can make, consider the following 
procedure, which modifies the car of the structure to which it is applied: |#
(defun set-to-foo! (x)
  (setf (car (car x)) 'foo)
  x) ; Return X

(set-to-foo! z1) #|-->|# ((FOO B) FOO B)
(set-to-foo! z2) #|-->|# ((FOO B) A B)

#| Even though z1 and z2 are "the same" structure, applying set-to-foo! to them 
yields different results. With z1, altering the car also changes the cdr, 
because in z1 the car and the cdr are the same pair. With z2, the car and cdr 
are distinct, so set-to-foo! modifies only the car. 

One way to detect sharing is to use one of the equality functions to test
whether the items are actually the same object. (Each object in CL is EQ to 
itself.)

We can exploit sharing to greatly extend the repertoire of data structures that 
can be represented by pairs. On the other hand, sharing can also be dangerous, 
since modifications made to structures will also affect other structures that 
happen to share the modified parts. |#


; = Representing Queues =

#| The mutators setf enables us to use pairs to construct data structures that 
cannot be built with cons, car, and cdr alone. This section shows how to use 
pairs to represent a data structure called a queue. A queue is a sequence in 
which items are inserted at one end (called the rear of the queue) and deleted 
from the other end (the front). Because items are always removed in the order in
which they are inserted, a queue is sometimes called a FIFO (first in, first 
out) buffer.

In terms of data abstraction, we can regard a queue as defined by the following 
set of operations:

* a constructor:
  (make-queue)
  returns an empty queue (a queue containing no items).

* two selectors:
  (empty-queue? <queue>)
  tests if the queue is empty.
  (front-queue <queue>)
  returns the object at the front of the queue, signaling an error if the queue 
  is empty; it does not modify the queue.

* two mutators:
  (insert-queue! <queue> <item>)
  inserts the item at the rear of the queue and returns the modified queue as 
  its value.
  (delete-queue! <queue>)
  removes the item at the front of the queue and returns the modified queue as 
  its value, signaling an error if the queue is empty before the deletion. 

Because a queue is a sequence of items, we could certainly represent it as an 
ordinary list; the front of the queue would be the car of the list, inserting an
item in the queue would amount to appending a new element at the end of the 
list, and deleting an item from the queue would just be taking the cdr of the 
list. However, this representation is inefficient, because in order to insert an
item we must scan the list until we reach the end. Since the only method we have
for scanning a list is by successive cdr operations, this scanning requires O(n)
steps for a list of n items. A simple modification to the list representation 
overcomes this disadvantage by allowing the queue operations to be implemented 
so that they require O(1) steps; that is, so that the number of steps needed is 
independent of the length of the queue. The modification that avoids the 
drawback is to represent the queue as a list, together with an additional 
pointer that indicates the final pair in the list. That way, when we go to 
insert an item, we can consult the rear pointer and so avoid scanning the 
list. 

A queue is represented, then, as a pair of pointers, front-ptr and rear-ptr, 
which indicate, respectively, the first and last pairs in an ordinary list. 
Since we would like the queue to be an identifiable object, we can use cons to 
combine the two pointers. Thus, the queue itself will be the cons of the two 
pointers. 
     _____
q -->|*|*-----------+
      |front-ptr    |rear-ptr
    __v__  _____  __v__
    |*|*-->|*|*-->|*|/|
     |      |      |
     v      v      v
     a      b      c

To define the queue operations we use the following procedures: |#
(defun front-ptr (queue) 
  "Return the front of the QUEUE"
  (car queue))

(defun rear-ptr (queue) 
  "Return the rear of the QUEUE"
  (cdr queue))

(defun set-front-ptr! (queue item)
  "Set the front of the QUEUE to ITEM"
  (setf (car queue) item))

(defun set-rear-ptr! (queue item) 
  "Set the rear of the QUEUE equal to ITEM"
  (setf (cdr queue) item))

(defun empty-queue? (queue) 
  "Return T if QUEUE is empty (FRONT-PTR is NULL)"
  (null (front-ptr queue)))

(defun make-queue () 
  "Return an empty queue, where the car and cdr are both an empty list"
  (cons '() '()))

(defun front-queue (queue)
  "Return front of the QUEUE, if it exists"
  (if (empty-queue? queue)
      (warn "FRONT called with an empty queue: ~a" queue)
      (car (front-ptr queue))))

(defun insert-queue! (queue item)
  "Add an item to the rear of the QUEUE, and return it"
  (let ((new-pair (cons item '()))) ; create a new terminating element
    (cond ((empty-queue? queue) ; If empty, point front & rear at the item
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (t ; else
           (setf (cdr (rear-ptr queue)) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

#| To delete the item at the front of the queue, we merely modify the front 
pointer so that it now points at the second item in the queue, which can be 
found by following the cdr pointer of the first item. |#
(defun delete-queue! (queue)
  "Delete the first item by shifting the front pointer, return QUEUE"
  (cond ((empty-queue? queue)
         (warn "DELETE! called with an empty queue: ~a" queue))
        (t
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

#| When printing a queue, the Lisp printer sees just a pair created with cons, 
so it prints its car and cdr. Its car is the front queue pointer, which points 
to a list. This is the first element printed. The rear pointer points to the 
last element of the list. Hence that last element is shown twice.

Here’s a printing function that just prints the queue itself: |#
(defun print-queue (queue)
  "Print QUEUE in a human-readable way"
  (format t "~a~%" (car queue)))

#| Instead of representing a queue as a pair of pointers, we can build a queue 
as a procedure with local state. The local state will consist of pointers to the
beginning and the end of an ordinary list. |#
(defun make-queue ()
  (let ((front '())
        (rear '()))
    (labels (
      (front-ptr ()
        front)
      (rear-ptr ()
        rear)
      (empty-queue? ()
        (null front))
      (set-front-ptr! (item)
        (setf front item))
      (set-rear-ptr! (item)
        (setf rear item))
      (front-queue ()
        (if (empty-queue?)
          (error "FRONT on empty queue")
          (car front-ptr)))
      (insert-queue! (item)
        (let ((new-pair (cons item '())))
          (cond ((empty-queue?)
                  (set-front-ptr! new-pair)
                  (set-rear-ptr! new-pair))
                (t
                  (setf (cdr (rear-ptr)) new-pair)
                  (set-rear-ptr! new-pair)))))
      (delete-queue! ()
        (cond ((empty-queue?)
                (error "DELETE! on empty queue"))
              (t
                (set-front-ptr!
                  (cdr (front-ptr))))))
      (print-queue ()
        (format t "~a~%" (front-ptr)))
      (dispatch (m)
        (case m
          ('front-ptr #'front-ptr)
          ('rear-ptr #'rear-ptr)
          ('empty-queue? #'empty-queue?)
          ('set-front-ptr! #'set-front-ptr!)
          ('set-rear-ptr! #'set-rear-ptr!)
          ('front-queue #'front-queue)
          ('insert-queue! #'insert-queue!)
          ('delete-queue! #'delete-queue!)
          ('print-queue #'print-queue)
          (otherwise (error "Bad dispatch ~A" m)))))
      #'dispatch)))

; Create and operate on the enclosed queue thusly:
(defvar q (make-queue))
(funcall (funcall q 'insert-queue!) 't)
(funcall (funcall q 'insert-queue!) 'a)
(funcall (funcall q 'print-queue))


; = Representing tables =
; THIS SECTION SKIPPED