#|
SICP-10--Assignment-and-State.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Assignment and Local State
|#

#| One powerful design strategy, which is particularly appropriate to the 
construction of programs for modeling physical systems, is to base the structure
of our programs on the structure of the system being modeled. For each object in
the system, we construct a corresponding computational object. For each system 
action, we define a symbolic operation in our computational model. Our hope in 
using this strategy is that extending the model to accommodate new objects or 
new actions will require no strategic changes to the program, only the addition 
of the new symbolic analogs of those objects or actions. If we have been 
successful in our system organization, then to add a new feature or debug an old
one we will have to work on only a localized part of the system.

We ordinarily view the world as populated by independent objects, each of which 
has a state that changes over time. An object is said to "have state" if its 
behavior is influenced by its history. A bank account, for example, has state in
that the answer to the question "Can I withdraw $100?" depends upon the 
history of deposit and withdrawal transactions. We can characterize an object's 
state by one or more state variables, which among them maintain enough 
information about history to determine the object's current behavior. In a 
simple banking system, we could characterize the state of an account by a 
current balance rather than by remembering the entire history of account 
transactions. 

For such a model to be modular, it should be decomposed into computational 
objects that model the actual objects in the system. Each computational object 
must have its own local state variables describing the actual object's state. 
Since the states of objects in the system being modeled change over time, the 
state variables of the corresponding computational objects must also change. If 
we choose to model the flow of time in the system by the elapsed time in the 
computer, then we must have a way to construct computational objects whose 
behaviors change as our programs run. In particular, if we wish to model state 
variables by ordinary symbolic names in the programming language, then the 
language must provide an assignment operator to enable us to change the value 
associated with a name. |#

; = Local State Variables =
#| Let us model the situation of withdrawing money from a bank account. We will 
do this using a procedure withdraw, which takes as argument an amount to be 
withdrawn. If there is enough money in the account to accommodate the 
withdrawal, then withdraw should return the balance remaining after the 
withdrawal. Otherwise, withdraw should return the message "Insufficient funds". 

Observe that the expression (withdraw 25), evaluated twice, yields different 
values. This is a new kind of behavior for a procedure. To implement withdraw, 
we can use a variable balance to indicate the balance of money in the account 
and define withdraw as a procedure that accesses balance. The withdraw procedure
checks to see if balance is at least as large as the requested amount. If so, 
withdraw decrements balance by amount and returns the new value of balance. 
Otherwise, withdraw returns the Insufficient funds message. Here are the 
definitions of balance and withdraw: |#

(defvar balance 100)

(defun withdraw (amount)
  (if (>= balance amount) ; If funds are sufficient
      ; subtract AMOUNT from BALANCE, return BALANCE
      (progn (setq balance (- balance amount)) ; could have instead used DECF
             balance)
      "Insufficient funds")) ; Otherwise return string, unsuccessful withdrawal

; Result of WITHDRAW depends on state of, and alters the state of, BALANCE
(withdraw 55) #|-->|# 45
(withdraw 55) #|-->|# "Insufficient funds"

#| Although withdraw works as desired, the variable balance presents a problem. 
As specified above, balance is a name defined in the global environment and is 
freely accessible to be examined or modified by any procedure. It would be much 
better if we could somehow make balance internal to withdraw, so that withdraw 
would be the only procedure that could access balance directly and any other 
procedure could access balance only indirectly (through calls to withdraw). |#

(let ((balance 100))
  (defun new-withdraw (amount)
      (if (>= balance amount)
          (progn (decf balance amount)
                 balance)
          "Insufficient funds")))

#| Unfortunately, using this technique raises a serious problem: When we first 
introduced procedures, we also introduced the substitution model of evaluation 
to provide an interpretation of what procedure application means. We said that 
applying a procedure should be interpreted as evaluating the body of the 
procedure with the formal parameters replaced by their values. The trouble is 
that, as soon as we introduce assignment into our language, substitution is no 
longer an adequate model of procedure application. As a consequence, we 
technically have at this point no way to understand why the new-withdraw 
procedure behaves as claimed above. In order to really understand a procedure 
such as new-withdraw, we will need to develop a new model of procedure 
application. 

The following procedure, make-account, creates "withdrawal processors". The 
formal parameter balance in make-account specifies the initial amount of money 
in the account. |#

(defun make-account (balance)
  #'(lambda (amount)
      (if (>= balance amount)
          (progn (decf balance amount)
                 balance)
          "Insufficient funds")))

(defvar foo (make-account 100)) ; FOO holds an account closure with 100 starting
                                ;balance
(funcall foo 55) ; Deduct 55 from account balance enclosed by FOO -->
45

#| All functions created by MAKE-ACCOUNT are completely independent objects, 
each with its own local state variable BALANCE. Withdrawals from one do not 
affect the other. |#

(defun make-account (balance)
  #'(lambda (m quan)
      (labels ((withdraw (amount)
		     (if (>= balance amount)
			 (progn (setq balance (- balance amount))
				balance)
			 "Insufficient funds"))
	       (deposit (amount)
		 (setq balance (+ balance amount))
		 balance))
	(cond ((eql m 'withdraw) (withdraw quan))
	      ((eql m 'deposit) (deposit quan))
	      (t (error "Unknown request -- MAKE-ACCOUNT"))))))
; Create account closure
(defvar acc (make-account 100)) ; ACC, an account with starting BALANCE 100
; Use the various utilities that close over BALANCE
(funcall acc 'deposit 40) #|-->|# 140
(funcall acc 'withdraw 80) #|-->|# 60

#| Each call to acc in turn calls the locally defined deposit or withdraw 
procedure, which is then applied to the specified amount. As was the case with 
make-withdraw, another call to make-aaccount will produce a completely separate 
account object, which maintains its own local balance. |#

; - Accumulators -
#| An accumulator is a procedure that is called repeatedly with a single numeric
argument and accumulates its arguments into a sum. Each time it is called, it 
returns the currently accumulated sum. |#
(defun make-accumulator (initial)
  (let ((accumulator initial))
    #'(lambda (addon)
	(setq accumulator (+ accumulator addon))
	accumulator)))
#| Note that we don't need progn here, because the body of a lambda definition 
is implicitly wrapped in one. |#


;; == The Costs of Introducing Assignment ==

#| As we have seen, the SETQ operation enables us to model objects that have 
local state. However, this advantage comes at a price. Our programming language 
can no longer be interpreted in terms of the substitution model of procedure 
application. Moreover, no simple model with "nice" mathematical properties can 
be an adequate framework for dealing with objects and assignment in programming 
languages. 

So long as we do not use assignments, two evaluations of the same procedure with
the same arguments will produce the same result, so that procedures can be 
viewed as computing mathematical functions. Programming without any use of 
assignments is accordingly known as functional programming. 

The substitution model is based ultimately on the notion that the symbols in our
language are essentially names for values. But as soon as we introduce setq and 
the idea that the value of a variable can change, a variable can no longer be 
simply a name. Now a variable somehow refers to a place where a value can be 
stored, and the value stored at this place can change. |#

; = Sameness and Change =

#| The issue surfacing here is more profound than the mere breakdown of a 
particular model of computation. As soon as we introduce change into our 
computational models, many notions that were previously straightforward become 
problematical. Consider the concept of two things being "the same". 

Suppose we call make-decrementer twice with the same argument to create two 
procedures: |#

(defun make-decrementer (balance)
  "Return function that closes over BALANCE, when called returns it minus arg"
  #'(lambda (amount)
      (- balance amount))) ; note that closed-over BALANCE unchnaged

(defvar D1 (make-decrementer 25))
(defvar D2 (make-decrementer 25))

#| Are D1 and D2 the same? An acceptable answer is yes, because D1 and D2 have 
the same computational behavior -- each is a procedure that subtracts its input 
from 25. In fact, D1 could be substituted for D2 in any computation without 
changing the result. 

Contrast this with making two calls to make-simplified-withdraw: |#

(defun make-simplified-withdraw (balance)
  "Return function closing over BALANCE, subtracts AMOUNT from it when called"
  #'(lambda (amount)
      (setq balance (- balance amount)) ; value of BALANCE changed
      balance))

#| Are W1 and W2 the same? Surely not, because calls to W1 and W2 have distinct 
effects, as shown by the following sequence of interactions: |#
(funcall W1 20) #|-->|# 5
(funcall W1 20) #|-->|# -15
(funcall W2 20) #|-->|# 5

#| Even though W1 and W2 are "equal" in the sense that they are both created by 
evaluating the same expression, (make-simplified-withdraw 25), it is not true 
that W1 could be substituted for W2 in any expression without changing the 
result of evaluating the expression.

A language that supports the concept that "equals can be substituted for equals"
in an expresssion without changing the value of the expression is said to be 
referentially transparent. Referential transparency is violated when we include 
setq in our computer language. This makes it tricky to determine when we can 
simplify expressions by substituting equivalent expressions. Consequently, 
reasoning about programs that use assignment becomes drastically more 
difficult. 

As an example of how this issue arises in programming, consider the situation 
where Peter and Paul have a bank account with $100 in it. There is a substantial
difference between modeling this as |#
(defvar peter-acc (make-simplified-withdraw 100))
(defvar paul-acc (make-simplified-withdraw 100))
; and modeling it as
(defvar peter-acc (make-simplified-withdraw 100))
(defvar paul-acc peter-acc)

#| In the first situation, the two bank accounts are distinct. Transactions made
by Peter will not affect Paul's account, and vice versa. In the second 
situation, however, we have defined paul-acc to be the same thing as peter-acc. 
In effect, Peter and Paul now have a joint bank account, and if Peter makes a 
withdrawal from peter-acc Paul will observe less money in paul-acc. These two 
similar but distinct situations can cause confusion in building computational 
models. With the shared account, in particular, it can be especially confusing 
that there is one object (the bank account) that has two different names 
 (peter-acc and paul-acc); if we are searching for all the places in our program
where paul-acc can be changed, we must remember to look also at things that 
change peter-acc. 

With reference to the above remarks on "sameness" and "change", observe that if 
Peter and Paul could only examine their bank balances, and could not perform 
operations that changed the balance, then the issue of whether the two accounts 
are distinct would be moot. In general, so long as we never modify data objects,
we can regard a compound data object to be precisely the totality of its pieces.
A bank account is still ``the same'' bank account even if we change the balance 
by making a withdrawal; conversely, we could have two different bank accounts 
with the same state information. This complication is a consequence, not of our 
programming language, but of our perception of a bank account as an object. We 
do not, for example, ordinarily regard a rational number as a changeable object 
with identity, such that we could change the numerator and still have "the same"
rational number. |#


; = Pitfalls of Imperitive Programming =

#| In contrast to functional programming, programming that makes extensive use 
of assignment is known as imperative programming. In addition to raising 
complications about computational models, programs written in imperative style 
are susceptible to bugs that cannot occur in functional programs. 

In general, programming with assignment forces us to carefully consider the 
relative orders of the assignments to make sure that each statement is using the
correct version of the variables that have been changed. This issue simply does 
not arise in functional programs. |#