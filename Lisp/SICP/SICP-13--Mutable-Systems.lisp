#|
SICP-13--Mutable-Systems.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Modeling with Mutable Data
|#

;; == A Simulator for Digital Circuits ==

#| Computer simulation of proposed circuit designs is an important tool used by
digital systems engineers. In this section we design a system for performing 
digital logic simulations. This system typifies a kind of program called an 
event-driven simulation, in which actions ("events") trigger further events that
happen at a later time, which in turn trigger more events, and so on. 

Our computational model of a circuit will be composed of objects that correspond
to the elementary components from which the circuit is constructed. 

There are wires, which carry digital signals. A digital signal may at any moment
have only one of two possible values, 0 and 1. 

There are also various types of digital function boxes, which connect wires 
carrying input signals to other output wires. Such boxes produce output signals 
computed from their input signals. The output signal is delayed by a time that 
depends on the type of the function box.

For example, an inverter is a primitive function box that inverts its input. If 
the input signal to an inverter changes to 0, then one inverter-delay later the 
inverter will change its output signal to 1.  

An and-gate is a primitive function box with two inputs and one output. It 
drives its output signal to a value that is the logical and of the inputs. That 
is, if both of its input signals become 1, then one and-gate-delay time later 
the and-gate will force its output signal to be 1; otherwise the output will be 
0. 

An or-gate is a similar two-input primitive function box that drives its output 
signal to a value that is the logical or of the inputs. That is, the output will
become 1 if at least one of the input signals is 1; otherwise the output will 
become 0. 

We can connect primitive functions together to construct more complex functions.
To accomplish this we wire the outputs of some function boxes to the inputs of 
other function boxes. For example, the half-adder circuit consists of an 
or-gate, two and-gates, and an inverter. It takes two input signals, A and B, 
and has two output signals, S and C. S will become 1 whenever precisely one of A
and B is 1, and C will become 1 whenever A and B are both 1. We can see that, 
because of the delays involved, the outputs may be generated at different times.
Many of the difficulties in the design of digital circuits arise from this 
fact. 
                                                  Half-Adder
A ----@--->~~~~, D,--------------->~~~~~,         ==========
      |   | OR >--'               | AND >-- S          |
    +-|--->~~~~'      ,~~~~~, E,-->~~~~~'            --|--  Crossed Wires
    | |            ,--> INV >--'                       |    (No Connection)
    | +--->~~~~~,  |  '~~~~~'
    |     | AND >--@----------------------- C          |
B --@----->~~~~~'                                    --@--  Wire Junction
                                                       |    (Connection)     

We will now build a program for modeling the digital logic circuits we wish to 
study. The program will construct computational objects modeling the wires, 
which will "hold" the signals. Function boxes will be modeled by procedures that
enforce the correct relationships among the signals. 

  = Representing Wires =

A wire in our simulation will be a computational object with two local state 
variables: a signal-value (initially taken to be 0) and a collection of 
action-procedures to be run when the signal changes value. We implement the 
wire, using message-passing style, as a collection of local procedures

|#   

(defclass wire ()
  ( (signal-value
      :initform 0
      :accessor signal-value ; The slot signal-value has an accessor of a 
      :documentation         ; similar name.
        "The signal associated with the wire")
    (action-procedures
      :initform '()
      :accessor action-procedures
      :documentation
        "Procedures that are executed when there's
        an event on the wire")))

(defun make-wire ()
  (make-instance 'wire))

#| = Primitive Function Boxes = 
The primitive function boxes implement the "forces" by which a change in the 
signal on one wire influences the signals on other wires. To build function 
boxes, we use the following operations on wires:

SIGNAL-VALUE - Returns the current value of the signal on the wire. The accessor
defined in DEFCLASS WIRE, above.

The definition of the setter for signal-value. It must check for a change in 
the value and notify the relevant action procedures. So I used the :around 
feature of CLOS to add custom code on calls of the writer. |#
(defmethod (setf signal-value) :around (new-value (w wire))
  "changes the value of the signal on the wire to the new value"
  (let ((old-value (signal-value w)))
    (prog1 (call-next-method)
      (unless (= old-value new-value)
        (call-each (action-procedures w))))))

#| ADD-ACTION! asserts that the designated procedure should be run whenever the 
signal on the wire changes value. Such procedures are the vehicles by which 
changes in the signal value on the wire are communicated to other wires. |#
(defgeneric add-action! (w proc)
  (:documentation "Add a new action procedure"))

(defmethod add-action! ((w wire) proc)
  (push proc (action-procedures w))
  (funcall proc))

(defmethod print-object ((w wire) stream)
  (print-unreadable-object (w stream :type t)
    (with-slots ( (sv signal-value)
                  (ap action-procedures)) w
      (format stream "Sig: ~a, N procs: ~a"
        sv (length ap)))))

(defun call-each (procs)
  (dolist (proc procs)
    (funcall proc)))

(defvar *inverter-delay* 2)

#| In addition, we will make use of a procedure after-delay that takes a time 
delay and a procedure to be run and executes the given procedure after the given
delay.

Using these procedures, we can define the primitive digital logic functions. To 
connect an input to an output through an inverter, we use add-action! to 
associate with the input wire a procedure that will be run whenever the signal 
on the input wire changes value. The procedure computes the logical-not of the 
input signal, and then, after one inverter-delay, sets the output signal to be 
this new value:

The arguments to the constructor procedure are the wires to be attached to 
the function box representing the component. |#
(defun inverter (input output)
  (flet ((invert-input ()
            (let ((new-value
                    (logical-not
                      (signal-value input))))
              (after-delay
                *inverter-delay*
                (lambda ()
                  (setf
                    (signal-value output)
                    new-value))))))
    (add-action! input #'invert-input)
    'ok))

(defun logical-not (s)
  (case s
    (0 1)
    (1 0)
    (otherwise (error "Invalid signal" s))))

(defvar *and-gate-delay* 3)

#| An and-gate is a little more complex. The action procedure must be run if 
either of the inputs to the gate changes. It computes the logical-and (using a 
procedure analogous to logical-not) of the values of the signals on the input 
wires and sets up a change to the new value to occur on the output wire after 
one and-gate-delay. |#

(defun and-gate (a1 a2 output)
  (flet ((and-action ()
            (let ((new-value
                    (logand
                      (signal-value a1)
                      (signal-value a2))))
              (after-delay
                *and-gate-delay*
                (lambda ()
                  (setf
                    (signal-value output)
                    new-value))))))
    (add-action! a1 #'and-action)
    (add-action! a2 #'and-action)
    'ok))

(defvar *or-gate-delay* 3)

(defun or-gate (a1 a2 output)
  (flet ((or-action ()
            (let ((new-value
                    (logior
                      (signal-value a1)
                      (signal-value a2))))
              (after-delay
                *or-gate-delay*
                (lambda ()
                  (setf
                    (signal-value output)
                    new-value))))))
    (add-action! a1 #'or-action)
    (add-action! a2 #'or-action)
    'ok))

(defmacro with-wires ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-wire)))
    ,@body))

#| This macro makes our code less repetitive, and hence much more pleasant to 
write. For example, half-adder is defined as follows, given the four external 
wires to be attached to the half-adder: |#
(defun half-adder (a b s c)
  (with-wires (d e)
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

#| The advantage of making this definition is that we can use half-adder itself 
as a building block in creating more complex circuits. Below is a diagram of a 
full adder with its constructor following it. 
                                                     Full-Adder
                     ,~~~~~~~,                       ==========
   A ----------------> HALF- >------------- SUM
                  +--> ADDER >--+
       ,~~~~~~~,  |  '~~~~~~~'  +-->~~~~,  
   B --> HALF- >--+                | OR >-- C_out
C_in --> ADDER >------------------->~~~~'
       '~~~~~~~'                        |#

(defun full-adder (a b c-in sum c-out)
  (with-wires (s c1 c2)
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

#| Wires, which have time-varying signals and may be incrementally attached to 
devices, are typical of mutable objects. We have modeled them as procedures with
local state variables that are modified by assignment. When a new wire is 
created, a new set of state variables is allocated and a new dispatch procedure 
is constructed and returned, capturing the environment with the new state 
variables.

The wires are shared among the various devices that have been connected to them.
Thus, a change made by an interaction with one device will affect all the other 
devices attached to the wire. The wire communicates the change to its neighbors 
by calling the action procedures provided to it when the connections were 
established. |#


;; == The Agenda ==

#| The only thing needed to complete the simulator is after-delay. The idea here
is that we maintain a data structure, called an agenda, that contains a schedule
of things to do. |#

;;;;;;;;;;;;;;;;;;;;; Agenda ;;;;;;;;;;;;;;;;;;;;;

(load "ex_3_21") ; queue

#| The agenda is made up of time segments. Each time segment is a pair 
consisting of a number (the time) and a queue that holds the procedures that are
scheduled to be run during that time segment. We will operate on the 
time-segment queues using the queue operations.|#
(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments)
  (setf (cdr agenda) segments))
(defun first-segment (agenda)
  (car (segments agenda)))
(defun rest-segments (agenda)
  (cdr (segments agenda)))

#| The agenda itself is a one-dimensional table of time segments. The segments 
will be sorted in order of increasing time. In addition, we store the current 
time (i.e., the time of the last action that was processed) at the head of the 
agenda. A newly constructed agenda has no time segments and has a current time 
of 0: |#

(defun make-agenda () 
  "returns a new empty agenda."
  (list 0))

(defun cur-time (agenda) 
  "returns the current simulation time"
  (car agenda))

(defun set-cur-time! (agenda time)
  (setf (car agenda) time))

; An agenda is empty if it has no time segments:
(defun empty-agenda? (agenda)
  "is true if the specified agenda is empty."
  (null (segments agenda)))

#| To add an action to an agenda, we first check if the agenda is empty. If so, 
we create a time segment for the action and install this in the agenda. 
Otherwise, we scan the agenda, examining the time of each segment. If we find a 
segment for our appointed time, we add the action to the associated queue. If we
reach a time later than the one to which we are appointed, we insert a new time 
segment into the agenda just before it. If we reach the end of the agenda, we 
must create a new time segment at the end. |#
(defun add-to-agenda! (time action agenda)
  "modifies agenda by adding given action procedure to be run at specified time"
  (labels (
    (belongs-before? (segments)
      (or (null segments)
          (< time (segment-time (car segments)))))
    (make-new-time-segment (time action)
      (let ((q (make-queue)))
        (insert-queue! q action)
        (make-time-segment time q)))
    (add-to-segments! (segments)
      (if (= (segment-time (car segments)) time)
        (insert-queue!
          (segment-queue (car segments))
          action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
            (setf
              (cdr segments)
              (cons
                (make-new-time-segment time action)
                (cdr segments)))
            (add-to-segments! rest))))))

    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons
            (make-new-time-segment time action)
            segments))
        (add-to-segments! segments)))))

#| The procedure that removes the first item from the agenda deletes the item at
the front of the queue in the first time segment. If this deletion makes the 
time segment empty, we remove it from the list of segments: |#
(defun remove-first-agenda-item! (agenda)
  "modifies the agenda by removing the first item"
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

#| The first agenda item is found at the head of the queue in the first time 
segment. Whenever we extract an item, we also update the current time: |#
(defun first-agenda-item (agenda)
  "returns the first item on the agenda."
  (if (empty-agenda? agenda)
    (error "Agenda is empty")
    (let ((first-seg (first-segment agenda)))
      (set-cur-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(defun print-agenda-details (agenda)
  (format t "--------~%Agenda:~%Cur time: ~a~%"
    (cur-time agenda))
  (dolist (seg (segments agenda))
    (format t "Seg. Time: ~a" (segment-time seg))
    (format t ", Queue: ~a~%" (segment-queue seg))
  (format t "--------~%")))

;;;;;;;;;;;;;;;;;;; Simulation ;;;;;;;;;;;;;;;;;;;

; The particular agenda that we use is denoted by *the-agenda*. 
(defvar *the-agenda* (make-agenda))

; The procedure after-delay adds new elements to the-agenda:
(defun after-delay (delay action)
  (add-to-agenda!
    (+ delay (cur-time *the-agenda*))
    action
    *the-agenda*))

#| The simulation is driven by the procedure propagate, which operates on 
the-agenda, executing each procedure on the agenda in sequence. In general, as 
the simulation runs, new items will be added to the agenda, and propagate will 
continue the simulation as long as there are items on the agenda: |#
(defun propagate ()
  (if (empty-agenda? *the-agenda*)
    'done
    (let ((first-item (first-agenda-item *the-agenda*)))
      (funcall first-item)
      (remove-first-agenda-item! *the-agenda*)
      (propagate))))

#| The following procedure, which places a "probe" on a wire, shows the 
simulator in action. The probe tells the wire that, whenever its signal changes 
value, it should print the new signal value, together with the current time and 
a name that identifies the wire: |#
(defun probe (name wire)
  (add-action! wire
    (lambda ()
      (format t "~a ~a new-value = ~a~%"
        name
        (cur-time *the-agenda*)
        (signal-value wire)))))

; = Usage =
(setq x (make-wire))
(setq y (make-wire))
(setq z (make-wire))  
(and-gate y x z)
(setf (signal-value x) 1)
(setf (signal-value y) 0)
(propagate)
(setf (signal-value y) 1)
(setf (signal-value x) 0)
(propagate)
#|-->|# Z 0 new-value = 0
#|-->|# Z 6 new-value = 1
#|-->|# Z 6 new-value = 0