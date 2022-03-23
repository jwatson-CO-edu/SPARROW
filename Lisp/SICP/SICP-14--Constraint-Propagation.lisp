#|
SICP-14--Constraint-Propagation.lisp
James Watson, 2012 May

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

A system that operates on relations/constraints of data
|#

#| We often model systems in terms of relations among quantities. For example, a
mathematical model of a mechanical structure might include the information that 
the deflection d of a metal rod is related to the force F on the rod, the length
L of the rod, the cross-sectional area A, and the elastic modulus E via the 
equation:

dAE = FL
 
Given any four of the quantities, we can use it to compute the fifth. Yet 
translating the equation into a traditional computer language would force us to 
choose one of the quantities to be computed in terms of the other four. Thus, a 
procedure for computing the area A could not be used to compute the deflection 
d, even though the computations of A and d arise from the same equation.

Here we sketch the design of a language that enables us to work in terms of 
relations themselves. The primitive elements of the language are primitive 
constraints, which state that certain relations hold between quantities. For 
example, (adder a b c) specifies that the quantities a, b, and c must be related
by the equation a + b = c, (multiplier x y z) expresses the constraint xy = z, 
and (constant 3.14 x) says that the value of x must be 3.14.

Our language provides a means of combining primitive constraints in order to 
express more complex relations. We combine constraints by constructing 
constraint networks, in which constraints are joined by connectors. A connector 
is an object that "holds" a value that may participate in one or more 
constraints. For example, we know that the relationship between Fahrenheit and 
Celsius temperatures is:

9C = 5(F-32)

Such a constraint can be thought of as a network consisting of primitive adder, 
multiplier, and constant constraints. (See figure below) In the figure, we see 
on the left a multiplier box with three terminals, labeled m1, m2, and p. These 
connect the multiplier to the rest of the network as follows: The m1 terminal is
linked to a connector C, which will hold the Celsius temperature. The m2 
terminal is linked to a connector w, which is also linked to a constant box that
holds 9. The p terminal, which the multiplier box constrains to be the product 
of m1 and m2, is linked to the p terminal of another multiplier box, whose m2 is
connected to a constant 5 and whose m1 is connected to one of the terms in a 
sum. 
    ,~~~~~~~~~,        ,~~~~~~~~~,   v    ,~~~~~~~~~,
C --# m1      |   u    |      m1 #--------# a1      |
    |    *  p #--------# p  *    |        |    *  s #-- F
 +--# m2      |        |      m2 #--+  +--# a2      |
 |  '~~~~~~~~~'        '~~~~~~~~~'  |  |  '~~~~~~~~~'
 |W ,~~~,                    ,~~~, x|  |y ,~~~~, 
 +--# 9 |                    | 5 #--+  +--# 32 |
    '~~~'                    '~~~'        '~~~~'

Computation by such a network proceeds as follows: When a connector is given a 
value (by the user or by a constraint box to which it is linked), it awakens all
of its associated constraints (except for the constraint that just awakened it) 
to inform them that it has a value. Each awakened constraint box then polls its 
connectors to see if there is enough information to determine a value for a 
connector. If so, the box sets that connector, which then awakens all of its 
associated constraints, and so on. For instance, in conversion between Celsius 
and Fahrenheit, w, x, and y are immediately set by the constant boxes to 9, 5, 
and 32, respectively. The connectors awaken the multipliers and the adder, which
determine that there is not enough information to proceed. If the user (or some 
other part of the network) sets C to a value (say 25), the leftmost multiplier 
will be awakened, and it will set u to 25 * 9 = 225. Then u awakens the second 
multiplier, which sets v to 45, and v awakens the adder, which sets F to 77. |#

#| A connector is represented with: a with local state variables value, the 
current value of the connector; informant, the object that set the connector's 
value; and constraints, a list of the constraints in which the connector 
participates. |#
(defclass connector ()
  ( (value
      :initform nil
      :accessor value ; returns the connector's current value
      :documentation
        "Current value of the connector")
    (informant
      :initform nil
      :accessor informant
      :documentation
        "The object that set the connector's value")
    (constraints
      :initform '()
      :accessor constraints
      :documentation
        "The constraints in which the connector
        participages")))

(defun make-connector ()
  (make-instance 'connector))

(defmethod has-value? ((c connector))
  "tells whether the connector has a value"
  (if (informant c)
    t
    '()))

#| The connector's procedure set-value! is called when there is a request to set 
the connector's value. If the connector does not currently have a value, it will 
set its value and remember as informant the constraint that requested the value 
to be set. Then the connector will notify all of its participating constraints 
except the constraint that requested the value to be set. |#
(defmethod set-value! ((c connector) newval setter)
  "indicates that informant is requesting connector to set value to new value"
  (cond ((not (has-value? c))
          (setf (value c) newval)
          (setf (informant c) setter)
          (for-each-except
            setter
            #'process-new-value
            (constraints c)))
        ((not (= (value c) newval))
          (error "Contradiction"
            (list (value c) newval)))))

(defmethod forget-value! ((c connector) retractor)
  "tells the connector that the retractor is requesting it to forget its value"
  (when (eq retractor (informant c))
    (setf (informant c) nil)
    (for-each-except
      retractor
      #'process-forget-value
      (constraints c))))

(defmethod connect ((c connector) new-constraint)
  "tells the connector to participate in the new constraint"
  (if (not (member new-constraint (constraints c)))
    (push new-constraint (constraints c)))
  (if (has-value? c)
    (process-new-value new-constraint)))

(defun for-each-except (exception proc seq)
  "applies a designated procedure to all items in a list except a given one"
  (dolist (obj seq)
    (unless (eq obj exception)
      (funcall proc obj))))

#| The with-connectors macro will help us write complex constraint blocks with 
less repetitive typing. |#
(defmacro with-connectors ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-connector)))
    ,@body))

(defgeneric process-new-value (constraint)
  (:documentation
    "Called when one of the connectors attached to
    this constraint has a new value"))

(defgeneric process-forget-value (constraint)
  (:documentation
    "Ask the attached connectors to forget their values"))

#| Now, we can write our constraint classes and methods that implement these 
generics. In contrast to C++ and Java, these classes don’t have to be siblings. 
In fact, they can be any classes at all as long as they implement these two 
methods. Such approach has been recently brought to fame by Ruby, and it’s 
called duck typing. Here are the adder and multipler constraints, which are 
actually quite similar: |#

(defclass adder-constraint ()
  ( (a1
      :accessor a1
      :initarg :a1
      :documentation "First addend")
    (a2
      :accessor a2
      :initarg :a2
      :documentation "Second addend")
    (sum
      :accessor sum
      :initarg :sum
      :documentation "Sum of addends")))

(defun adder (a1 a2 sum)
  "Creates adder constraint with summand connectors a1 and a2 and sum connector"
  (let ((cs (make-instance
              'adder-constraint
              :a1 a1 :a2 a2 :sum sum)))
    (connect a1 cs)
    (connect a2 cs)
    (connect sum cs)
    cs))

#| The adder first checks to see if both a1 and a2 have values. If so, it tells 
sum to set its value to the sum of the two addends. The informant argument to 
set-value! is me, which is the adder object itself. If a1 and a2 do not both have
values, then the adder checks to see if perhaps a1 and sum have values. If so, it
sets a2 to the difference of these two. Finally, if a2 and sum have values, this 
gives the adder enough information to set a1. If the adder is told that one of 
its connectors has lost a value, it requests that all of its connectors now lose 
their values. (Only those values that were set by this adder are actually 
lost.) |#
(defmethod process-new-value ((cs adder-constraint))
  (with-accessors ((a1 a1) (a2 a2) (sum sum)) cs
    (cond ((and (has-value? a1) (has-value? a2))
            (set-value! sum
                        (+ (value a1) (value a2))
                        cs))
          ((and (has-value? a1) (has-value? sum))
            (set-value! a2
                        (- (value sum) (value a1))
                        cs))
          ((and (has-value? a2) (has-value? sum))
            (set-value! a1
                        (- (value sum) (value a2))
                        cs)))))

(defmethod process-forget-value ((cs adder-constraint))
  (forget-value! (a1 cs) cs)
  (forget-value! (a2 cs) cs)
  (forget-value! (sum cs) cs)
  (process-new-value cs))

(defclass multiplier-constraint ()
  ( (m1
      :accessor m1
      :initarg :m1
      :documentation "First multiplicand")
    (m2
      :accessor m2
      :initarg :m2
      :documentation "Second multiplicand")
    (product
      :accessor product
      :initarg :product
      :documentation "Product of multiplicands")))

(defun multiplier (m1 m2 product)
  (let ((cs (make-instance
              'multiplier-constraint
              :m1 m1 :m2 m2 :product product)))
    (connect m1 cs)
    (connect m2 cs)
    (connect product cs)
    cs))

#| A multiplier is very similar to an adder. It will set its product to 0 if 
either of the factors is 0, even if the other factor is not known. |#
(defmethod process-new-value ((cs multiplier-constraint))
  (with-accessors ((m1 m1) (m2 m2) (product product)) cs
    (cond ((or  (and (has-value? m1) (= (value m1) 0))
                (and (has-value? m2) (= (value m2) 0)))
            (set-value! product 0 cs))
          ((and (has-value? m1) (has-value? m2))
            (set-value! product
                        (* (value m1) (value m2)) cs))
          ((and (has-value? product) (has-value? m1))
            (set-value! m2
                        (/ (value product) (value m1) cs)))
          ((and (has-value? product) (has-value? m2))
            (set-value! m1
                        (/ (value product) (value m2)) cs)))))

(defmethod process-forget-value ((cs multiplier-constraint))
  (forget-value! (m1 cs) cs)
  (forget-value! (m2 cs) cs)
  (forget-value! (product cs) cs)
  (process-new-value cs))

#| The following two constrains are, in fact, quite different. But they still 
implement the two methods, so they are lawful constraints as far as we’re 
concerned: |#

(defclass constant-constraint () ())

(defun constant (value connector)
  (let ((cs (make-instance 'constant-constraint)))
    (connect connector cs)
    (set-value! connector value cs)
    cs))

(defmethod process-new-value ((cs constant-constraint))
  (error "Unable to set new value for CONSTANT"))

(defmethod process-forget-value ((cs constant-constraint))
  (error "Unable to forget a value for CONSTANT"))

(defclass probe-constraint ()
  ( (name :initarg :name :accessor name)
    (connector :initarg :connector :accessor connector)))

(defun probe (name connector)
  (let ((cs (make-instance
              'probe-constraint
              :name name
              :connector connector)))
    (connect connector cs)
    cs))

(defmethod print-probe ((cs probe-constraint) value)
  (format t "~%Probe: ~a = ~a" (name cs) value))

(defmethod process-new-value ((cs probe-constraint))
  (print-probe cs (value (connector cs))))

(defmethod process-forget-value ((cs probe-constraint))
  (print-probe cs "?"))

(defmacro deflex (var val &optional (doc nil docp))
  "Define a top level (global) lexical VAR with
  initial value VAL, which is assigned
  unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the
  name |VAR| and the name *STORAGE-FOR-DEFLEX-VAR-|VAR|*
  as a documentation string of kind 'VARIABLE.
  The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its
  dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))
         (backing-var
          (intern (concatenate 'string s0 s1 s2) s3)))
    ; Note: The DEFINE-SYMBOL-MACRO must be the last
    ; thing we do so that the value of the form is the
    ; symbol VAR.
    ; (print (concatenate 'string s0 s1 s2))
    (if docp
      `(progn
         (defparameter ,backing-var ,val ,doc)
         (setf (documentation ',var 'variable) ,doc)
         (define-symbol-macro ,var ,backing-var))
      `(progn
         (defparameter ,backing-var ,val)
         (define-symbol-macro ,var ,backing-var)))))

; = Usage =
; The procedure that creates the network is defined as follows:
(defun celsius-fahrenheit-converter (c f)
  (with-connectors (u v w x y)
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
#| This procedure creates the internal connectors u, v, w, x, and y, and links 
them as shown in figure 3.28 using the primitive constraint constructors adder, 
multiplier, and constant. Just as with the digital-circuit simulator, expressing 
these combinations of primitive elements in terms of procedures automatically 
provides our language with a means of abstraction for compound objects. |#

(deflex c (make-connector))
(deflex f (make-connector))
(celsius-fahrenheit-converter c f)

#| To watch the network in action, we can place probes on the connectors C and F,
using a probe procedure similar to the one we used to monitor wires. Placing a 
probe on a connector will cause a message to be printed whenever the connector is
given a value: |#
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

#| Next we set the value of C to 25. (The third argument to set-value! tells C 
that this directive comes from the user.) |#
(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)
#|-->|#
Probe: Celsius temp = 25
Probe: Fahrenheit temp = 77
Probe: Celsius temp = ?
Probe: Fahrenheit temp = ?
Probe: Fahrenheit temp = 212
Probe: Celsius temp = 100