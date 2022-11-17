#|
PCL-09--Object-Orientation.lisp
James Watson, 2012 March
Using the Common Lisp Object System (CLOS) I
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

#| The CLOS began as an experiment in adding OOP to Lisp, but is now a core 
part of the language. 

The Object System does not attempt to solve problems of encapsulation or 
protection.

The fundamental idea of object orientation is that a powerful way to organize a 
program is to define data types and then associate operations with those data 
types.  The classic example used is an operation DRAW that can be applied to 
objects representing various geometric shapes. The different implementations of 
DRAW are defined separately, and new versions can be defined that DRAW other 
shapes without having to change the code of either the caller or any of the 
other DRAW implementations. This feature of object orientation goes by the fancy
Greek name polymorphism, meaning "many forms," because a single conceptual 
operation, such as drawing an object, can take many different concrete forms.

Object means any Lisp datum--Common Lisp doesn't distinguish, as some languages 
do, between objects and "primitive" data types; all data in Common Lisp are 
objects, and every object is an instance of a class.

Common Lisp, like most object-oriented languages today, is class-based; all 
objects are instances of a particular class. The class of an object determines 
its representation--built-in classes such as NUMBER and STRING have opaque 
representations accessible only via the standard functions for manipulating 
those types, while instances of user-defined classes, as you'll see in the next 
chapter, consist of named parts called slots.

Classes are arranged in a hierarchy, a taxonomy for all objects. A class can be 
defined as a subclass of other classes, called its superclasses. A class 
inherits part of its definition from its superclasses and instances of a class 
are also considered instances of the superclasses. In Common Lisp, the hierarchy
of classes has a single root, the class T, which is a direct or indirect 
superclass of every other class. Thus, every datum in Common Lisp is an instance
of T. Common Lisp also supports multiple inheritance--a single class can have 
multiple direct superclasses.

  == Generic Functions and Methods ==

As far as CLOS is concerned, the truth is that - with the exception of slot 
accessors - all of your application's functionality lives in function and 
method definitions, not in classes.

Generic functions are the heart of the CLOS. A generic function defines an 
abstract operation, specifying its name and a parameter list but no 
implementation. Here, for example, is how you might define a generic function, 
draw, that will be used to draw different kinds of shapes on the screen: |#
(defgeneric draw (shape) ; SHAPE here refers to the object to be dealt with
  (:documentation "Draw the given shape on the screen."))
#| Note that this definition doesn't contain any actual code. 

A generic function is generic in the sense that it can--at least in theory--
accept any objects as arguments. However, by itself a generic function can't 
actually do anything; if you just define a generic function, no matter what 
arguments you call it with, it will signal an error. The actual implementation 
of a generic function is provided by methods. Each method provides an 
implementation of the generic function for particular classes of arguments. 
Perhaps the biggest difference between a generic function-based system and a 
message-passing system is that methods don't belong to classes; they belong to 
the generic function, which is responsible for determining what method or 
methods to run in response to a particular invocation. 

Methods indicate what kinds of arguments they can handle by specializing the 
required parameters defined by the generic function. For instance, on the 
generic function draw, you might define one method that specializes the shape 
parameter for objects that are instances of the class circle while another 
method specializes shape for objects that are instances of the class triangle. 
They would look like this, sans the actual drawing code: |#
(defmethod draw ((shape circle))
  ...)

(defmethod draw ((shape triangle))
  ...)
#| When a generic function is invoked, it compares the actual arguments it was 
passed with the specializers of each of its methods to find the applicable 
methods--those methods whose specializers are compatible with the actual 
arguments. If you invoke draw, passing an instance of circle, the method that 
specialized shape on the class circle is applicable. 

You can specialize a parameter in two ways--usually you'll specify a class that 
the argument must be an instance of. Because instances of a class are also 
considered instances of that class's superclasses, a method with a parameter 
specialized on a particular class can be applicable whenever the corresponding 
argument is a direct instance of the specializing class or of any of its 
subclasses. The other kind of specializer is a so-called EQL specializer, which 
specifies a particular object to which the method applies.

When a generic function has only methods specialized on a single parameter and 
all the specializers are class specializers, the result of invoking a generic 
function is quite similar to the result of invoking a method in a message-
passing system--the combination of the name of the operation and the class of 
the object on which it's invoked determines what method to run. 

However, reversing the order of lookup opens up possibilities not found in 
message-passing systems. Generic functions support methods that specialize on 
multiple parameters, provide a framework that makes multiple inheritance much 
more manageable, and let you use declarative constructs to control how methods 
are combined into an effective method, supporting several common usage patterns 
without a lot of boilerplate code. |#

; = defgeneric =
#| For now just assume that certain classes already exist: for starters, assume 
there's a class bank-account and that it has two subclasses, checking-account 
and savings-account. 

The first generic function will be withdraw, which decreases the account balance
by a specified amount. If the balance is less than the amount, it should signal 
an error and leave the balance unchanged. |#
(defgeneric withdraw (account amount)
                     ;^       ^-- arguments begin with second item in list
                     ; \_- first item in list refers to the object
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

#| The basic form of DEFGENERIC is similar to DEFUN except with no body. The 
parameter list of DEFGENERIC specifies the parameters that must be accepted by 
all the methods that will be defined on the generic function. In the place of 
the body, a DEFGENERIC can contain various options. One option you should always
include is :documentation, which you use to provide a string describing the 
purpose of the generic function. Because a generic function is purely abstract, 
it's important to be clear to both users and implementers what it's for.  |#

; = defmethod =
#| More generally, methods must have the same number of required and optional 
parameters and must be capable of accepting any arguments corresponding to any 
&rest or &key parameters specified by the generic function. A method can 
"accept" &key and &rest arguments defined in its generic function by having a 
&rest parameter, by having the same &key parameters, or by specifying 
&allow-other-keys along with &key. A method can also specify &key parameters not
found in the generic function's parameter list--when the generic function is 
called, any &key parameter specified by the generic function or any applicable 
method will be accepted. 

Since the basics of withdrawing are the same for all accounts, you can define a 
method that specializes the account parameter on the bank-account class. You can
assume the function balance returns the current balance of the account and can 
be used with SETF--and thus with DECF--to set the balance. The function ERROR is
a standard function used to signal an error. |#
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
	              ; ^-- When referring to the object within a method      
                  ;     definition, use the generic symbol ACCOUNT, not the 
                  ;     specified type BANK-ACCOUNT
    (error "Account overdrawn."))
  (decf (balance account) amount))
#| As this code suggests, the form of DEFMETHOD is even more like that of DEFUN 
than DEFGENERIC's is. The only difference is that the required parameters can be
specialized by replacing the parameter name with a two-element list. The first 
element is the name of the parameter, and the second element is the specializer,
either the name of a class or an EQL specializer, the form of which I'll discuss
in a moment. The parameter name can be anything--it doesn't have to match the 
name used in the generic function, though it often will. 

Now suppose all checking accounts have overdraft protection. You can assume that
the function overdraft-account takes a checking-account object and returns a 
bank-account object representing the linked account. |#
(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

#| The function CALL-NEXT-METHOD is part of the generic function machinery used 
to combine applicable methods. It indicates that control should be passed from 
this method to the method specialized on bank-account. When it's called with no 
arguments, as it is here, the next method is invoked with whatever arguments 
were originally passed to the generic function. It can also be called with 
arguments, which will then be passed onto the next method. You aren't required 
to invoke CALL-NEXT-METHOD in every method. However, if you don't, the new 
method is then responsible for completely implementing the desired behavior of 
the generic function. 

Finally, DEFMETHOD also allows you to create methods specialized on a particular
object with an EQL specializer. For example, suppose the banking app is going to
be deployed in a particularly corrupt bank. Suppose the variable 
*account-of-bank-president* holds a reference to a particular bank account that 
belongs to the bank's president. Further suppose the variable *bank* represents 
the bank as a whole, and the function embezzle steals money from the bank. The 
bank president might ask you to "fix" withdraw to handle his account 
specially. |#
(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
  (call-next-method)))

#| Note, however, that the form in the EQL specializer that provides the object 
to specialize on--*account-of-bank-president* in this case--is evaluated once, 
when the DEFMETHOD is evaluated. This method will be specialized on the value of
*account-of-bank-president* at the time the method is defined; changing the 
variable later won't change the method. |#

;; == Method Combination ==
#| Outside the body of a method, CALL-NEXT-METHOD has no meaning. Within a 
method, it's given a meaning by the generic function machinery that builds an 
effective method each time the generic function is invoked using all the methods
applicable to that particular invocation. This notion of building an effective 
method by combining applicable methods is the heart of the generic function 
concept making the generic function, rather than the class, the prime mover. 

Conceptually, the effective method is built in three steps: First, the generic 
function builds a list of applicable methods based on the actual arguments it 
was passed. Second, the list of applicable methods is sorted according to the 
specificity of their parameter specializers. Finally, methods are taken in order
from the sorted list and their code combined to produce the effective method. 

A method is applicable if, and only if, all the specializers are compatible with
the corresponding arguments. 

When the specializer is the name of a class, it's compatible if it names the 
actual class of the argument or one of its superclasses. (Recall that parameters
without explicit specializers are implicitly specialized on the class T so will 
be compatible with any argument.) An EQL specializer is compatible only when the
argument is the same object as was specified in the specializer.  If two class 
specializers differ, one will be a subclass of the other. In that case, the 
specializer naming the subclass is considered more specific. This is why the 
method that specialized account on checking-account was considered more specific
than the method that specialized it on bank-account. 

Because all the arguments are checked against the corresponding specializers, 
they all affect whether a method is applicable. Methods that explicitly 
specialize more than one parameter are called multimethods. 

An EQL specializer is always more specific than any class specializer, and 
because only applicable methods are being considered, if more than one method 
has an EQL specializer for a particular parameter, they must all have the same 
EQL specializer. The comparison of those methods will thus be decided based on 
other parameters. |#


;; == Standard Method Combination == 
#| By default, generic functions use what's called the standard method 
combination. The standard method combination combines methods so that 
CALL-NEXT-METHOD works as you've already seen--the most specific method runs 
first, and each method can pass control to the next most specific method via 
CALL-NEXT-METHOD. (CALL-NEXT-METHOD is roughly analogous to invoking a method on
super in Java or using an explicitly class-qualified method or function name in 
Python or C++.)

The methods discussed so far are called primary methods. Primary methods, as 
their name suggests, are responsible for providing the primary implementation of
a generic function. The standard method combination also supports three kinds of
auxiliary methods: :before, :after, and :around methods. An auxiliary method 
definition is written with DEFMETHOD like a primary method but with a method 
qualifier, which names the type of method, between the name of the method and 
the parameter list. For instance, a :before method on withdraw that specializes 
the account parameter on the class bank-account would start like this: |#
(defmethod withdraw :before ((account bank-account) amount) ...)

#| Each kind of auxiliary method is combined into the effective method in a 
different way. All the applicable :before methods--not just the most specific--
are run as part of the effective method. They run, as their name suggests, 
before the most specific primary method and are run in most-specific-first 
order. Thus, :before methods can be used to do any preparation needed to ensure 
that the primary method can run. For instance, you could've used a :before 
method specialized on checking-account to implement the overdraft protection on 
checking accounts like this: |#
(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))
#| This :before method has three advantages over a primary method. 
* One is that it makes it immediately obvious how the method changes the overall
behavior of the withdraw function--it's not going to interfere with the main 
behavior or change the result returned.
* The next advantage is that a primary method specialized on a class more 
specific than checking-account won't interfere with this :before method, making 
it easier for an author of a subclass of checking-account to extend the behavior
of withdraw while keeping part of the old behavior.
* Lastly, since a :before method doesn't have to call CALL-NEXT-METHOD to pass 
control to the remaining methods, it's impossible to introduce a bug by 
forgetting to. 

The other auxiliary methods also fit into the effective method in ways suggested
by their names. All the :after methods run after the primary methods in 
most-specific-last order, that is, the reverse of the :before methods. Thus, the
:before and :after methods combine to create a sort of nested wrapping around 
the core functionality provided by the primary methods--each more-specific 
:before method will get a chance to set things up so the less-specific :before 
methods and primary methods can run successfully, and each more-specific :after 
method will get a chance to clean up after all the primary methods and 
less-specific :after methods.

Finally, :around methods are combined much like primary methods except they're 
run "around" all the other methods. That is, the code from the most specific 
:around method is run before anything else. Within the body of an :around 
method, CALL-NEXT-METHOD will lead to the code of the next most specific :around
method or, in the least specific :around method, to the complex of :before, 
primary, and :after methods. Almost all :around methods will contain such a call
to CALL-NEXT-METHOD because an :around method that doesn't will completely 
hijack the implementation of the generic function from all the methods except 
for more-specific :around methods.

Occasionally that kind of hijacking is called for, but typically :around methods
are used to establish some dynamic context in which the rest of the methods will
run--to bind a dynamic variable, for example, or to establish an error handler.
|#

;; == Other Method Combinations ==
#| In addition to the standard method combination, the language specifies nine 
other built-in method combinations known as the simple built-in method 
combinations. You can also define custom method combinations, though that's a 
fairly esoteric feature. 

All the simple combinations follow the same pattern: instead of invoking the 
most specific primary method and letting it invoke less-specific primary methods
via CALL-NEXT-METHOD, the simple method combinations produce an effective method
that contains the code of all the primary methods, one after another, all 
wrapped in a call to the function, macro, or special operator that gives the 
method combination its name. The nine combinations are named for the operators: 
+, AND, OR, LIST, APPEND, NCONC, MIN, MAX, and PROGN. The simple combinations 
also support only: primary methods and :around methods. Both of these function
in the same way they would in the standard method combination. 

A generic function that uses the + method combination will return the sum of all
the results returned by its primary methods. Note that the AND and OR method 
combinations won't necessarily run all the primary methods because of those 
macros' short-circuiting behavior--a generic function using the AND combination 
will return NIL as soon as one of the methods does and will return the value of 
the last method otherwise. Similarly, the OR combination will return the first 
non-NIL value returned by any of the methods.

To define a generic function that uses a particular method combination, you 
include a :method-combination option in the DEFGENERIC form. The value supplied 
with this option is the name of the method combination you want to use. For 
example, to define a generic function, priority, that returns the sum of values 
returned by individual methods using the + method combination, you might write 
this: |#
(defgeneric priority (job)
  (:documentation "Return the priority at which the job should be run.")
  (:method-combination +))

#| By default all these method combinations combine the primary methods in 
most-specific-first order. However, you can reverse the order by including the 
keyword :most-specific-last after the name of the method combination in the 
DEFGENERIC form. The order probably doesn't matter if you're using the + 
combination unless the methods have side effects, but for demonstration purposes
you can change priority to use most-specific-last order like this: |#
(defgeneric priority (job)
  (:documentation "Return the priority at which the job should be run.")
  (:method-combination + :most-specific-last))

#| The primary methods on a generic function that uses one of these combinations
must be qualified with the name of the method combination. Thus, a primary 
method defined on priority might look like this: |#
(defmethod priority + ((job express-job)) 10)

; The :most-specific-last option doesn't affect the order of :around methods.


;; == Multimethods ==
#| Methods that explicitly specialize more than one of the generic function's 
required parameters are called multimethods. Multimethods are where generic 
functions and message passing really part ways. Multimethods don't fit into 
message-passing languages because they don't belong to a particular class; 
instead, each multimethod defines a part of the implementations of a given 
generic function that applies when the generic function is invoked with 
arguments that match all the method's specialized parameters. 

Multimethods are perfect for all those situations where you struggle to decide 
to which class a certain behavior ought to belong. Is the sound a drum makes 
when it's hit with a drumstick a function of what kind of drum it is or what 
kind of stick you use to hit it? Both, of course. To model this situation in 
Common Lisp, you simply define a generic function beat that takes two 
arguments. |#
(defgeneric beat (drum stick)
  (:documentation
   "Produce a sound by hitting the given drum with the given stick."))

#| Then you can define various multimethods to implement beat for the 
combinations you care about. For example: |#
(defmethod beat ((drum snare-drum) (stick wooden-drumstick)) ...)
(defmethod beat ((drum snare-drum) (stick brush)) ...)
(defmethod beat ((drum snare-drum) (stick soft-mallet)) ...)
(defmethod beat ((drum tom-tom) (stick wooden-drumstick)) ...)
(defmethod beat ((drum tom-tom) (stick brush)) ...)
(defmethod beat ((drum tom-tom) (stick soft-mallet)) ...)

#| Multimethods don't help with the combinatorial explosion--if you need to 
model five kinds of drums and six kinds of sticks, and every combination makes a
different sound, there's no way around it; you need thirty different methods to 
implement all the combinations, with or without multimethods. What multimethods 
do save you from is having to write a bunch of dispatching code by letting you 
use the same built-in polymorphic dispatching that's so useful when dealing with
methods specialized on a single parameter. 

Multimethods also save you from having to tightly couple one set of classes with
the other. In the drum/stick example, nothing requires the implementation of the
drum classes to know about the various classes of drumstick, and nothing 
requires the drumstick classes to know anything about the various classes of 
drum. The multimethods connect the otherwise independent classes to describe 
their joint behavior without requiring any cooperation from the classes 
themselves.|#

