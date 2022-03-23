#|
SICP-15--Concurrency.lisp
James Watson, 2012 May

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Multiple processes accessing common data
|#

#| The central issue lurking beneath the omplexity of state, sameness, and 
change is that by introducing assignment we are forced to admit time into our
computational models. The execution of assignment statements delineates moments
in time when values change.

We can go further in structuring computational models to match our perception
of the physical world. Objects in the world do not change one at a time in
sequence. Rather, we perceive them as happening concurrently - all at once. It 
is often apropriate to divide computational models into parts that evolve 
separately and concurrently. Even if the programs are to be executed on a
sequential computer, the practice of writing programs as if they were to be
executed concurrently forces the programmer to avoid inessential timing 
constraints and thus makes programs more modular.

In addition to making programs more modular, concurrent computation can provide
a speed advantage by decomposing a problem into pieces that are relatively 
independent (and need to communicate only rarely), and then allocating these 
pieces to their own parallel processors. This has the potential to produce a
speed advantage roughly proportional to the number of available processors.

Unfortunately, the complexities introduced by assignment become even more 
problematic in the presence of concurrency. |#


;; == The Nature of Time in Concurrent Systems ==

#| On the surface, time seems straightforward. It is an ordering imposed on 
events. For any events A and B; either A occurs before B, A and B are 
simultaneous, or A occurs after B. For example, if Peter and Paul share a 
bank account, and both withdraw differing amounts of money from it, the 
sequence of values representing the account balance will depend on the order
in time of Peter and Paul's respective withdrawals.

In complex situations, however, such a view can be problematic. Suppose that 
Peter, Paul, and other people besides, are accessing the same bank account
through a network of banking machines distributed all over the world. The actual
sequence of balances will depend critically on the detailed timing of the 
accesses and the details of the communication among the machines. 

This indeterminacy in the order of events can pose serious problems in the 
design of concurrent systems. For instance, suppose that the withdrawals made
by Peter and Paul are implemented as two separate processes sharing a common
variable BALANCE. If the two processes operate independently, then Peter might 
test the balance and attempt to withdraw a legitimate amount.

If the two processes operate independently, then Peter might test the balance 
and attempt to withdraw a legitimate amount. However, Paul might withdraw some 
funds in between the time that Peter checks the balance and the time Peter 
completes the withdrawal, thus invalidating Peter's test. |#
(defun withdraw (amount)
  (if (>= balance amount)
      (progn (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

; Things can be worse still. Consider the expression
(setq balance (- balance amount))
#| executed as part of each withdrawal process. This consists of three steps: 
 (1) accessing the value of the balance variable
 (2) computing the new balance 
 (3) setting balance to this new value. 
If Peter and Paul's withdrawals execute this statement concurrently, then the 
two withdrawals might interleave the order in which they access balance and set 
it to the new value. 

BALANCE starts at 100, Peter withdraws 10, Paul withdraws 25, and yet the final 
value of balance is 75. The reason for this anomaly is that Paul's assignment of
75 to balance is made under the assumption that the value of balance to be 
decremented is 100. That assumption, however, became invalid when Peter changed 
balance to 90. This is a catastrophic failure for the banking system, because 
the total amount of money in the system is not conserved. Before the 
transactions, the total amount of money was $100. Afterwards, Peter has $10, 
Paul has $25, and the bank has $75. |#


; == Correct Behavior of Concurrent Programs ==

#| With concurrent processes we must be especially careful about assignments, 
because we may not be able to control the order of the assignments made by the 
different processes. If several such changes might be made concurrently (as with
two depositors accessing a joint account) we need some way to ensure that our 
system behaves correctly. For example, in the case of withdrawals from a joint 
bank account, we must ensure that money is conserved. To make concurrent 
programs behave correctly, we may have to place some restrictions on concurrent 
execution. 

One possible restriction on concurrency would stipulate that no two operations 
that change any shared state variables can occur at the same time. This is an 
extremely stringent requirement. For distributed banking, it would require the 
system designer to ensure that only one transaction could proceed at a time. 
This would be both inefficient and overly conservative.

A less stringent restriction on concurrency would ensure that a concurrent 
system produces the same result as if the processes had run sequentially in some
order. There are two important aspects to this requirement. First, it does not 
require the processes to actually run sequentially, but only to produce results 
that are the same as if they had run sequentially. Second, there may be more 
than one possible "correct" result produced by a concurrent program, because we 
require only that the result be the same as for some sequential order. 

There are still weaker requirements for correct execution of concurrent 
programs. A program for simulating diffusion (say, the flow of heat in an 
object) might consist of a large number of processes, each one representing a 
small volume of space, that update their values concurrently. Each process 
repeatedly changes its value to the average of its own value and its neighbors' 
values. This algorithm converges to the right answer independent of the order in
which the operations are done; there is no need for any restrictions on 
concurrent use of the shared values. |#


;; == Mechanisms for Controlling Concurrency ==

#| We've seen that the difficulty in dealing with concurrent processes is rooted
in the need to consider the interleaving of the order of events in the different
processes. For example, suppose we have two processes, one with three ordered 
events (a,b,c) and one with three ordered events (x,y,z). If the two processes 
run concurrently, with no constraints on how their execution is interleaved, 
then there are 20 different possible orderings for the events that are 
consistent with the individual orderings for the two processes. As programmers 
designing this system, we would have to consider the effects of each of these 20
orderings and check that each behavior is acceptable. Such an approach rapidly 
becomes unwieldy as the numbers of processes and events increase.

A more practical approach to the design of concurrent systems is to devise 
general mechanisms that allow us to constrain the interleaving of concurrent 
processes so that we can be sure that the program behavior is correct. Many 
mechanisms have been developed for this purpose. One of these is the 
serializer. |#


; = Serializing Access to a Shared State =

#| Serialization implements the following idea: Processes will execute 
concurrently, but there will be certain collections of procedures that cannot be
executed concurrently. More precisely, serialization creates distinguished sets 
of procedures such that only one execution of a procedure in each serialized set
is permitted to happen at a time. If some procedure in the set is being 
executed, then a process that attempts to execute any procedure in the set will 
be forced to wait until the first execution has finished.

We can use serialization to control access to shared variables. For example, if 
we want to update a shared variable based on the previous value of that 
variable, we put the access to the previous value of the variable and the 
assignment of the new value to the variable in the same procedure. We then 
ensure that no other procedure that assigns to the variable can run concurrently
with this procedure by serializing all of these procedures with the same 
serializer. This guarantees that the value of the variable cannot be changed 
between an access and the corresponding assignment. |#

; - Running Parallel Processes in SBCL -
(defvar x 10)

#| Two processes, named P1 and P2 below, will be attempting to access the same
variable, 'x'. |#
(defun compete ()
  (setq x 10)
  (sb-thread:make-thread (lambda () (setq x (* x x))))  ; P1
  (sb-thread:make-thread (lambda () (setq x (+ x 1))))) ; P2
#| This creates two concurrent processes -- P1, which sets x to x times x, and 
P2, which increments x. After execution is complete, x will be left with one of 
five possible values, depending on the interleaving of the events of P1 and P2: 

101:  P1 sets x to 100 and then P2 increments x to 101.
121:  P2 increments x to 11 and then P1 sets x to x times x.
110:  P2 changes x from 10 to 11 between the two times that P1 accesses the 
      value of x during the evaluation of (* x x).
 11:  P2 accesses x, then P1 sets x to 100, then P2 sets x.
100:  P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

SBCL, Linux, IBM T23: Repeated runs of COMPETE consistently return 100
|#

; - Using MUTEXes to Serialize in SBCL -
; URL: http://www.sbcl.org/manual/Mutex-Support.html
(defvar *a-mutex* (sb-thread:make-mutex :name "my lock"))

(defun thread-fn ()
       (format #.*standard-output* "Thread ~A running ~%" 
	       sb-thread:*current-thread*)
       (sb-thread:with-mutex (*a-mutex*)
         (format #.*standard-output* "Thread ~A got the lock~%" 
		 sb-thread:*current-thread*)
         (sleep (random 5)))
       (format #.*standard-output* "Thread ~A dropped lock, dying now~%" 
	       sb-thread:*current-thread*))

(defun compete ()
	   (sb-thread:make-thread #'thread-fn)
	   (sb-thread:make-thread #'thread-fn))

(compete) #| -->
Thread #<THREAD RUNNING {B318F09}> running 
Thread #<THREAD RUNNING {B318F09}> got the lock
Thread #<THREAD RUNNING {B318F09}> dropped lock, dying now
Thread #<THREAD RUNNING {B31D001}> running 
Thread #<THREAD RUNNING {B31D001}> got the lock
Thread #<THREAD RUNNING {B31D001}> dropped lock, dying now |#

#| Here is an example of bank accounts where the deposits and withdrawals have 
been serialized: |#

(defclass account ()
  ((balance :initarg :balance :initform 0 :accessor balance)
   (account-lock :initform (sb-thread:make-mutex) :accessor lock)))

(defgeneric deposit (obj amount)
  (:documentation "Add money to an ACCOUNT with a free mutex"))

(defgeneric withdraw (obj amount)
  (:documentation "Deduct money, if available, from ACCOUNT with a free mutex"))

(defmethod deposit ((obj account) amount)
  ; Create a thread that will execute when the object mutex has been freed
  (sb-thread:with-mutex (lock obj)
    (setf (balance obj) (+ (balance obj) amount))))

(defmethod withdraw ((obj account) amount)
  ; Create a thread that will execute when the object mutex has been freed
  (sb-thread:with-mutex (lock obj)
    (if (>= (balance obj) amount)
        (progn (setf (balance obj) (- (balance obj) amount))
               (format #.*standard-output* balance))
        (format #.*standard-output* "Insufficient funds"))))

#| With this implementation, two processes cannot be withdrawing from or 
depositing into a single account concurrently. This eliminates the source of the
error  where Peter changes the account balance between the times when Paul 
accesses the balance to compute the new value and when Paul actually performs 
the assignment. On the other hand, each account has its own mutex lock, so that 
deposits and withdrawals for different accounts can proceed concurrently. |#

; TODO: Work up an example to show that the above works!


;; == Implementing Serializers ==

#|  Mutexes and semaphores are already available as stock facilities of many
implementations of CL.  However, I have chosen to follow SICP through its 
development of such to try to find out how these should be constructed. 

  = Availability Test for Use with Mutexes =

Test-and-set! tests the cell and returns the result of the test. In addition, if
the test was false, test-and-set! sets the cell contents to true before 
returning false. We can express this behavior as the following procedure: |#
(defun test-and-set! (cell)
  (if (car cell)
      t
      (progn (setf (car cell) t)
             nil)))
#| However, this implementation of test-and-set! does not suffice as it stands. 
There is a crucial subtlety here, which is the essential place where concurrency
control enters the system: The test-and-set! operation must be performed 
atomically. That is, we must guarantee that, once a process has tested the cell 
and found it to be false, the cell contents will actually be set to true before 
any other process can test the cell. If we do not make this guarantee, then the 
mutex can fail. 

The actual implementation of test-and-set! depends on the details of how our 
system runs concurrent processes. For example, we might be executing concurrent 
processes on a sequential processor using a time-slicing mechanism that cycles 
through the processes, permitting each process to run for a short time before 
interrupting it and moving on to the next process. In that case, test-and-set! 
can work by disabling time slicing during the testing and setting. 
Alternatively, multiprocessing computers provide instructions that support 
atomic operations directly in hardware. 

  = Mutex Definition =

A mutex is an object that supports two operations -- the mutex can be acquired,
and the mutex can be released. Once a mutex has been acquired, no other acquire 
operations on that mutex may proceed until the mutex is released.

The mutex is a mutable object (here we'll use a one-element list, which we'll 
refer to as a cell) that can hold the value true or false. When the value is 
false, the mutex is available to be acquired. When the value is true, the mutex 
is unavailable, and any process that attempts to acquire the mutex must wait.

Our mutex constructor make-mutex begins by initializing the cell contents to 
false. To acquire the mutex, we test the cell. If the mutex is available, we set
the cell contents to true and proceed. Otherwise, we wait in a loop, attempting 
to acquire over and over again, until we find that the mutex is available. To 
release the mutex, we set the cell contents to false. |#
(defun clear! (cell)
  (setf (car cell) false))

(defun make-mutex ()
  (let ((cell (list nil)))            
    (labels ((the-mutex (m)
		       (cond ((eq m 'acquire)
			      (if (test-and-set! cell)
				  (the-mutex 'acquire))) ; retry
			     ((eq? m 'release) (clear! cell)))))
	    the-mutex)))

; = Serializer Definition =

#| In our implementation, each serializer has an associated mutex. Given a 
procedure p, the serializer returns a procedure that acquires the mutex, runs p,
and then releases the mutex. This ensures that only one of the procedures 
produced by the serializer can be running at once, which is precisely the 
serialization property that we need to guarantee. |#

(defun make-serializer ()
  (let ((mutex (make-mutex)))
    (lambda (p)
      (labels ((serialized-p (&rest args)
		 (mutex 'acquire)
		 (let ((val (funcall p args)))
		   (mutex 'release)
		   val))
	       #'serialized-p)))))