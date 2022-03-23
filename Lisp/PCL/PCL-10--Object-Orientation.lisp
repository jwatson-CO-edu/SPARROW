#|
PCL-10--Object-Orientation.lisp
James Watson, 2012 March
Using the Common Lisp Object System (CLOS) II
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

#| If generic functions are the verbs of the object system, classes are the 
nouns. All values in a Common Lisp program are instances of some class. 
Furthermore, all classes are organized into a single hierarchy rooted at the 
class T. 

The class hierarchy consists of two major families of classes, built-in and 
user-defined classes. Classes that represent the data types such as INTEGER, 
STRING, and LIST, are all built-in. They live in their own section of the class 
hierarchy, arranged into appropriate sub- and superclass relationships. You 
can't subclass these classes, but you can define methods that specialize on 
them, effectively extending the behavior of those classes. |#

;; == defclass ==
#| You create user-defined classes with the DEFCLASS macro. Because behaviors 
are associated with a class by defining generic functions and methods 
specialized on the class, DEFCLASS is responsible only for defining the class as
a data type.

The three facets of the class as a data type are its name, its relation to other
classes, and the names of the slots that make up instances of the class. The 
basic form of a DEFCLASS is quite simple. |#
(defclass name (direct-superclass-name*)
  (slot-specifier*))

#| As with functions and variables, you can use any symbol as the name of a new 
class. Class names are in a separate namespace from both functions and 
variables, so you can have a class, function, and variable all with the same 
name. You'll use the class name as the argument to MAKE-INSTANCE, the function 
that creates new instances of user-defined classes. 

The direct-superclass-names specify the classes of which the new class is a 
subclass. If no superclasses are listed, the new class will directly subclass 
STANDARD-OBJECT. Any classes listed must be other user-defined classes, which 
ensures that each new class is ultimately descended from STANDARD-OBJECT. 
STANDARD-OBJECT in turn subclasses T, so all user-defined classes are part of 
the single class hierarchy that also contains all the built-in classes. |#

(defclass bank-account () ...)
(defclass checking-account (bank-account) ...)
(defclass savings-account (bank-account) ...)

; = Slot Specifiers =
#| Each slot specifier defines a slot that will be part of each instance of the 
class. Each slot in an instance is a place that can hold a value, which can be 
accessed using the SLOT-VALUE function. SLOT-VALUE takes an object and the name 
of a slot as arguments and returns the value of the named slot in the given 
object. It can be used with SETF to set the value of a slot in an object. 

A class also inherits slot specifiers from its superclasses, so the set of slots
actually present in any object is the union of all the slots specified in a 
class's DEFCLASS form and those specified in all its superclasses. 

At the minimum, a slot specifier names the slot, in which case the slot 
specifier can be just a name. For instance, you could define a bank-account 
class with two slots, customer-name and balance, like this: |#
(defclass bank-account ()
  (customer-name
   balance))
#| Each instance of this class will contain two slots, one to hold the name of 
the customer the account belongs to and another to hold the current balance. 
With this definition, you can create new bank-account objects using 
MAKE-INSTANCE. |#
(make-instance 'bank-account) #|-->|# #<BANK-ACCOUNT @ #x724b93ba>

#| The argument to MAKE-INSTANCE can be either the name of the class or
a class object returned by the function CLASS-OF or FIND-CLASS. The value 
returned is the new object. 

The printed representation of an object is determined by the generic function 
PRINT-OBJECT. In this case, the applicable method will be one provided by the 
implementation, specialized on STANDARD-OBJECT. Since not every object can be 
printed so that it can be read back, the STANDARD-OBJECT print method uses the 
#<> syntax, which will cause the reader to signal an error if it tries to read 
it. The rest of the representation is implementation-defined but will typically 
be something like the output just shown, including the name of the class and 
some distinguishing value such as the address of the object in memory. 

Using the definition of bank-account just given, new objects will be created 
with their slots unbound. Any attempt to get the value of an unbound slot 
signals an error, so you must set a slot before you can read it. |#
(defparameter *account* (make-instance 'bank-account))  #|-->|# *ACCOUNT*
(setf (slot-value *account* 'customer-name) "John Doe") #|-->|# "John Doe"
(setf (slot-value *account* 'balance) 1000)             #|-->|# 1000
; Now you can access the value of the slots.
(slot-value *account* 'customer-name) #|-->|# "John Doe"
(slot-value *account* 'balance)       #|-->|# 1000


; = Object Initialzation =
#| Since you can't do much with an object with unbound slots, it'd be nice to be
able to create objects with their slots already initialized. Common Lisp 
provides three ways to control the initial value of slots. The first two involve
adding options to the slot specifier in the DEFCLASS form: with the :initarg 
option, you can specify a name that can then be used as a keyword parameter to 
MAKE-INSTANCE and whose argument will be stored in the slot. A second option, 
:initform, lets you specify a Lisp expression that will be used to compute a 
value for the slot if no :initarg argument is passed to MAKE-INSTANCE. Finally, 
for complete control over the initialization, you can define a method on the 
generic function INITIALIZE-INSTANCE, which is called by MAKE-INSTANCE. 
 ( Another way to affect the values of slots is with the :default-initargs 
option to DEFCLASS. This option is used to specify forms that will be evaluated 
to provide arguments for specific initialization parameters that aren't given a 
value in a particular call to MAKE-INSTANCE. )

For example, if you want to modify the definition of bank-account to allow 
callers of MAKE-INSTANCE to pass the customer name and the initial balance and 
to provide a default value of zero dollars for the balance, you'd write this: |#
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))
;Now you can create an account and specify the slot values at the same time.
(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe" :balance 1000))

(slot-value *account* 'customer-name) #|-->|# "John Doe"
(slot-value *account* 'balance)       #|-->|# 1000

#| If you don't supply a :balance argument to MAKE-INSTANCE, the SLOT-VALUE of 
balance will be computed by evaluating the form specified with the :initform 
option. But if you don't supply a :customer-name argument, the customer-name 
slot will be unbound, and an attempt to read it before you set it will signal an
error. 

Revised bank-account class with a new slot, account-number, that's initialized 
with the value of an ever-increasing counter. |#
(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))

#| However, while an initform can be any Lisp expression, it has no access to 
the object being initialized, so it can't initialize one slot based on the value
of another. For that you need to define a method on the generic function 
INITIALIZE-INSTANCE. The primary method on INITIALIZE-INSTANCE specialized on 
STANDARD-OBJECT takes care of initializing slots based on their :initarg and 
:initform options. Since you don't want to disturb that, the most common way to 
add custom initialization code is to define an :after method specialized on your
class. Adding an :after method to INITIALIZE-INSTANCE is the Common Lisp analog 
to defining a constructor in Java or C++ or an __init__ method in Python. 

For instance, suppose you want to add a slot account-type that needs to be set 
to one of the values :gold, :silver, or :bronze based on the account's initial 
balance. You might change your class definition to this, adding the account-type
slot with no options: |#
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   (account-type)))
#| Then you can define an :after method on INITIALIZE-INSTANCE that sets the 
account-type slot based on the value that has been stored in the balance 
slot. |#
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))
#| The &key in the parameter list is required to keep the method's parameter 
list congruent with the generic function's--the parameter list specified for the
INITIALIZE-INSTANCE generic function includes &key in order to allow individual 
methods to supply their own keyword parameters but doesn't require any 
particular ones. Thus, every method must specify &key even if it doesn't specify
any &key parameters. |#

#| One mistake you might make until you get used to using auxiliary methods is 
to define a method on INITIALIZE-INSTANCE but without the :after qualifier. If 
you do that, you'll get a new primary method that shadows the default one. You 
can remove the unwanted primary method using the functions REMOVE-METHOD and 
FIND-METHOD. Certain development environments may provide a graphical user 
interface to do the same thing. |#
(remove-method #'initialize-instance
  (find-method #'initialize-instance () (list (find-class 'bank-account))))

#| On the other hand, if an INITIALIZE-INSTANCE method specialized on a 
particular class does specify a &key parameter, that parameter becomes a legal 
parameter to MAKE-INSTANCE when creating an instance of that class. For 
instance, if the bank sometimes pays a percentage of the initial balance as a 
bonus when an account is opened, you could implement that using a method on 
INITIALIZE-INSTANCE that takes a keyword argument to specify the percentage of 
the bonus like this: |#
(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))
; called thusly
(defparameter *acct* (make-instance
                                'bank-account
                                 :customer-name "Sally Sue"
                                 :balance 1000
                                 :opening-bonus-percentage 5))
; and so
(slot-value *acct* 'balance) #|-->|# 1050

;; == Accessor Functions ==
#| Directly accessing the slots (or fields or member variables) of an object can
lead to fragile code. The problem is that directly accessing slots ties your 
code too tightly to the concrete structure of your class. For example, suppose 
you decide to change the definition of bank-account so that, instead of storing 
the current balance as a number, you store a list of time-stamped withdrawals 
and deposits. Code that directly accesses the balance slot will likely break if 
you change the class definition to remove the slot or to store the new list in 
the old slot. On the other hand, if you define a function, balance, that 
accesses the slot, you can redefine it later to preserve its behavior even if 
the internal representation changes. 

Another advantage to using accessor functions rather than direct access to slots
via SLOT-VALUE is that they let you limit the ways outside code can modify a 
slot. Of course, providing an accessor function doesn't really limit anything 
since other code can still use SLOT-VALUE to get at slots directly. Common Lisp 
doesn't provide strict encapsulation of slots the way some languages such as C++
and Java do. 

It's trivial to define a function that reads the value of the balance slot. |#
(defun balance (account)
  (slot-value account 'balance))
#| However, if you know you're going to define subclasses of bank-account, it 
might be a good idea to define balance as a generic function. That way, you can 
provide different methods on balance for those subclasses or extend its 
definition with auxiliary methods. So you might write this instead: |#
(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

; = SETF Functions =
#| A SETF function is a way to extend SETF, defining a new kind of place that it
knows how to set. The name of a SETF function is a two-item list whose first 
element is the symbol setf and whose second element is a symbol, typically the 
name of a function used to access the place the SETF function will set. A SETF 
function can take any number of arguments, but the first argument is always the 
value to be assigned to the place. You could, for instance, define a SETF 
function to set the customer-name slot in a bank-account like this: |#
(defun (setf customer-name) (name account)
  (setf (slot-value account 'customer-name) name))
; After evaluating that definition, an expression like the following one:
(setf (customer-name my-account) "Sally Sue")
#| will be compiled as a call to the SETF function you just defined with "Sally 
Sue" as the first argument and the value of my-account as the second 
argument. 

Of course, as with reader functions, you'll probably want your SETF function to 
be generic, so you'd actually define it like this: |#
(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))

; And of course you'll also want to define a reader function for customer-name.
(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

; Thus
(setf (customer-name *account*) "Sally Sue") #|-->|# "Sally Sue"
(customer-name *account*)                    #|-->|# "Sally Sue"

#| There's nothing hard about writing these accessor functions, but it wouldn't 
be in keeping with The Lisp Way to have to write them all by hand. Thus, 
DEFCLASS supports three slot options that allow you to automatically create 
reader and writer functions for a specific slot. 

The :reader option specifies a name to be used as the name of a generic function
that accepts an object as its single argument. When the DEFCLASS is evaluated, 
the generic function is created, if it doesn't already exist. Then a method 
specializing its single argument on the new class and returning the value of the
slot is added to the generic function. The name can be anything, but it's 
typical to name it the same as the slot itself. Thus, instead of explicitly 
writing the balance generic function and method as shown previously, you could 
change the slot specifier for the balance slot in the definition of bank-account
to this: |#
(balance
 :initarg :balance
 :initform 0
 :reader balance)

#| The :writer option is used to create a generic function and method for 
setting the value of a slot. The function and method created follow the 
requirements for a SETF function, taking the new value as the first argument and
returning it as the result, so you can define a SETF function by providing a 
name such as (setf customer-name). For instance, you could provide reader and 
writer methods for customer-name equivalent to the ones you just wrote by 
changing the slot specifier to this: |#
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :reader customer-name
 :writer (setf customer-name))

#| Since it's quite common to want both reader and writer functions, DEFCLASS 
also provides an option, :accessor, that creates both a reader function and the 
corresponding SETF function. |#
(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :accessor customer-name)

#| The :documentation option allows you to provide a string that documents the 
purpose of the slot. Putting it all together and adding a reader method for the 
account-number and account-type slots, the DEFCLASS form for the bank-account 
class would look like this: |#
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))


;; == WITH-SLOTS and WITH-ACCESSORS ==
#| While using accessor functions will make your code easier to maintain, they 
can still be a bit verbose. And there will be times, when writing methods that 
implement the low-level behaviors of a class, that you may specifically want to 
access slots directly to set a slot that has no writer function or to get at the
slot value without causing any auxiliary methods defined on the reader function 
to run. 

Two standard macros, WITH-SLOTS and WITH-ACCESSORS, create a block of code in 
which simple variable names can be used to refer to slots on a particular 
object. WITH-SLOTS provides direct access to the slots, as if by SLOT-VALUE, 
while WITH-ACCESSORS provides a shorthand for accessor methods. 

The basic form of WITH-SLOTS is as follows: |#
(with-slots (slot*) instance-form
  body-form*)

#| WITH-SLOTS is the object equivalent of LET. Use it, and not LET, as LET
 does not behave normally inside subclasses.  WITH-SLOTS may be used 
 anywhere, not only within DEFMETHOD. WITH-SLOTS also allows you to give
 nick-names to slots to be used within the WITH-SLOTS block. |#

(with-slots ((nickname-var slot)*) instance-form
  body-form*)

#| Each element of slots can be either the name of a slot, which is also used as
a variable name, or a two-item list where the first item is a name to use as a 
variable and the second is the name of the slot. The instance-form is evaluated 
once to produce the object whose slots will be accessed. Within the body, each 
occurrence of one of the variable names is translated to a call to SLOT-VALUE 
with the object and the appropriate slot name as arguments. 

Such is used in the methof assess-low-balance-penalty, which assesses a penalty 
on a bank-account if its balance falls below a certain minimum: |#

(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))
; or, using the two-item list form, like this:
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

#| If you had defined balance with an :accessor rather than just a :reader, then
you could also use WITH-ACCESSORS. The form of WITH-ACCESSORS is the same as 
WITH-SLOTS except each element of the slot list is a two-item list containing a 
variable name and the name of an accessor function. Within the body of 
WITH-ACCESSORS, a reference to one of the variables is equivalent to a call to 
the corresponding accessor function. If the accessor function is SETFable, then 
so is the variable. |#
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-accessors ((balance balance)) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))
#| The first balance is the name of the variable, and the second is the name of 
the accessor function; they don't have to be the same. You could, for instance, 
write a method to merge two accounts using two calls to WITH-ACCESSORS, one for 
each account. |#
(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
    (with-accessors ((balance2 balance)) account2
      (incf balance1 balance2)
      (setf balance2 0))))

#| The choice of whether to use WITH-SLOTS versus WITH-ACCESSORS is the same as 
the choice between SLOT-VALUE and an accessor function: low-level code that 
provides the basic functionality of a class may use SLOT-VALUE or WITH-SLOTS to 
directly manipulate slots in ways not supported by accessor functions or to 
explicitly avoid the effects of auxiliary methods that may have been defined on 
the accessor functions. But you should generally use accessor functions or 
WITH-ACCESSORS unless you have a specific reason not to. 

The "variable" names provided by WITH-SLOTS and WITH-ACCESSORS aren't true 
variables; they're implemented using a special kind of macro, called a symbol 
macro, that allows a simple name to expand into arbitrary code. Symbol macros 
were introduced into the language to support WITH-SLOTS and WITH-ACCESSORS, but 
you can also use them for your own purposes. |#

;; == Class-Allocated Slots ==
#| The last slot option you need to know about is :allocation. The value of 
:allocation can be either :instance or :class and defaults to :instance if not 
specified. When a slot has :class allocation, the slot has only a single value, 
which is stored in the class and shared by all instances.

However, :class slots are accessed the same as :instance slots--they're accessed
with SLOT-VALUE or an accessor function, which means you can access the slot 
value only through an instance of the class even though it isn't actually stored
in the instance. The :initform and :initarg options have essentially the same 
effect except the initform is evaluated once when the class is defined rather 
than each time an instance is created. On the other hand, passing an initarg to 
MAKE-INSTANCE will set the value, affecting all instances of the class.

Because you can't get at a class-allocated slot without an instance of the 
class, class-allocated slots aren't really equivalent to static or class fields 
in languages such as Java, C++, and Python. Rather, class-allocated slots are 
used primarily to save space; if you're going to create many instances of a 
class and all instances are going to have a reference to the same object--say, a
pool of shared resources--you can save the cost of each instance having its own 
reference by making the slot class-allocated. |#

;; == Slots and Inheritance ==
#| Classes inherit behavior from their superclasses thanks to the generic 
function machinery--a method specialized on class A is applicable not only to 
direct instances of A but also to instances of A's subclasses. Classes also 
inherit slots from their superclasses, but the mechanism is slightly 
different. 

In Common Lisp a given object can have only one slot with a particular name. 
However, it's possible that more than one class in the inheritance hierarchy of 
a given class will specify a slot with a particular name. This can happen either
because a subclass includes a slot specifier with the same name as a slot 
specified in a superclass or because multiple superclasses specify slots with 
the same name.

Common Lisp resolves these situations by merging all the specifiers with the 
same name from the new class and all its superclasses to create a single 
specifier for each unique slot name. When merging specifiers, different slot 
options are treated differently. For instance, since a slot can have only a 
single default value, if multiple classes specify an :initform, the new class 
uses the one from the most specific class. This allows a subclass to specify a 
different default value than the one it would otherwise inherit. 

On the other hand, :initargs needn't be exclusive--each :initarg option in a 
slot specifier creates a keyword parameter that can be used to initialize the 
slot; multiple parameters don't create a conflict, so the new slot specifier 
contains all the :initargs. Callers of MAKE-INSTANCE can use any of the 
:initargs to initialize the slot. If a caller passes multiple keyword arguments 
that initialize the same slot, then the leftmost argument in the call to 
MAKE-INSTANCE is used.

Inherited :reader, :writer, and :accessor options aren't included in the merged 
slot specifier since the methods created by the superclass's DEFCLASS will 
already apply to the new class. The new class can, however, create its own 
accessor functions by supplying its own :reader, :writer, or :accessor options.

Finally, the :allocation option is, like :initform, determined by the most 
specific class that specifies the slot. Thus, it's possible for all instances of
one class to share a :class slot while instances of a subclass may each have 
their own :instance slot of the same name. And a sub-subclass may then redefine 
it back to :class slot, so all instances of that class will again share a single
slot. In the latter case, the slot shared by instances of the sub-subclass is 
different than the slot shared by the original superclass.

For instance, suppose you have these classes: |#
(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))

(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initarg :the-b :accessor the-b :allocation :class)))

#| When instantiating the class bar, you can use the inherited initarg, :a, to 
specify a value for the slot a and, in fact, must do so to avoid an error, since
the :initform supplied by bar supersedes the one inherited from foo. To 
initialize the b slot, you can use either the inherited initarg :b or the new 
initarg :the-b. However, because of the :allocation option on the b slot in bar,
the value specified will be stored in the slot shared by all instances of bar. 
That same slot can be accessed either with the method on the generic function b 
that specializes on foo or with the new method on the generic function the-b 
that specializes directly on bar. To access the a slot on either a foo or a bar,
you'll continue to use the generic function a. |#

;; == Multiple Inheritance ==
#| Common Lisp also supports multiple inheritance--a class can have multiple 
direct superclasses, inheriting applicable methods and slot specifiers from all 
of them. This complicates the notion of class specificity that's used both when 
building the effective methods for a generic function and when merging inherited
slot specifiers.

When a class has multiple direct superclasses, those superclasses are typically 
not related to each other. In that case, the rule that subclasses are more 
specific than their superclasses isn't enough to order all the superclasses. So 
Common Lisp uses a second rule that sorts unrelated superclasses according to 
the order they're listed in the DEFCLASS's direct superclass list--classes 
earlier in the list are considered more specific than classes later in the list.
This rule is admittedly somewhat arbitrary but does allow every class to have a 
linear class precedence list, which can be used to determine which superclasses 
should be considered more specific than others. Note, however, there's no global
ordering of classes--each class has its own class precedence list, and the same 
classes can appear in different orders in different classes' class precedence 
lists. 

To see how this works, let's add a class to the banking app: 
money-market-account. A money market account combines the characteristics of a 
checking account and a savings account: a customer can write checks against it, 
but it also earns interest. You might define it like this: |#
(defclass money-market-account (checking-account savings-account) ())

; The class precedence list for money-market-account will be as follows:
(money-market-account
 checking-account
 savings-account
 bank-account
 standard-object
 t)

#| This class defines no slots of its own but will inherit slots from both of 
its direct superclasses, including the slots they inherit from their 
superclasses. Likewise, any method that's applicable to any class in the class 
precedence list will be applicable to a money-market-account object. Because 
all slot specifiers for the same slot are merged, it doesn't matter that 
money-market-account inherits the same slot specifiers from bank-account 
twice. (In other words, Common Lisp doesn't suffer from the diamond inheritance 
problem the way, say, C++ does. In C++, when one class subclasses two classes 
that both inherit a member variable from a common superclass, the bottom class 
inherits the member variable twice, leading to no end of confusion.) 

It's also possible to inherit different methods for the same generic function 
from different superclasses. In that case, the class precedence list does come 
into play. |#

;; == Misc ==

; = class-of =
(class-of OBJECT) ; Returns the class of which the OBJECT is a direct instance.