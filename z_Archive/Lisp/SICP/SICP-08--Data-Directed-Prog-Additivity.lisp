#|
SICP-07--Data-Directed-Prog-Additivity.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Data Directed Programming paradigm and Additivity
|#

#| The general strategy of checking the type of a datum and calling an 
appropriate procedure is called dispatching on type. This is a powerful strategy
for obtaining modularity in system design. On the other hand, implementing the 
dispatch has two significant weaknesses. One weakness is that the generic 
interface procedures (real-part, imag-part, magnitude, and angle) must know 
about all the different representations. Another weakness of the technique is 
that even though the individual representations can be designed separately, we 
must guarantee that no two procedures in the entire system have the same name.

The issue underlying both of these weaknesses is that the technique for 
implementing generic interfaces is not additive. The person implementing the 
generic selector procedures must modify those procedures each time a new 
representation is installed, and the people interfacing the individual 
representations must modify their code to avoid name conflicts. In each of these
cases, the changes that must be made to the code are sources of inconvenience 
and error. 

What we need is a means for modularizing the system design even further. This is
provided by the programming technique known as data-directed programming. To 
understand how data-directed programming works, begin with the observation that 
whenever we deal with a set of generic operations that are common to a set of 
different types we are, in effect, dealing with a two-dimensional table that 
contains the possible operations on one axis and the possible types on the other
axis. The entries in the table are the procedures that implement each operation 
for each type of argument presented.

Data-directed programming is the technique of designing programs to work with 
such a table directly. Previously, we implemented the mechanism that interfaces 
the complex-arithmetic code with the two representation packages as a set of 
procedures that each perform an explicit dispatch on type. Here we will 
implement the interface as a single procedure that looks up the combination of 
the operation name and argument type in the table to find the correct procedure 
to apply, and then applies it to the contents of the argument. If we do this, 
then to add a new representation package to the system we need not change any 
existing procedures; we need only add new entries to the table. |#