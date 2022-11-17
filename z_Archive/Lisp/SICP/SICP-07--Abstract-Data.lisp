#|
SICP-06--Trees.lisp
James Watson, 2012 April

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Multiple representations for abstract data
|#

#| The key idea was to erect an abstraction barrier -- in this case, the 
selectors and constructors for rational numbers (make-rat, numer, denom) -- that
isolates the way rational numbers are used from their underlying representation 
in terms of list structure. But this kind of data abstraction is not yet 
powerful enough, because it may not always make sense to speak of 
"the underlying representation" for a data object. 

For one thing, there might be more than one useful representation for a data 
object, and we might like to design systems that can deal with multiple 
representations. To take a simple example, complex numbers may be represented in
two almost equivalent ways: in rectangular form (real and imaginary parts) and 
in polar form (magnitude and angle). Sometimes rectangular form is more 
appropriate and sometimes polar form is more appropriate. Indeed, it is 
perfectly plausible to imagine a system in which complex numbers are represented
in both ways, and in which the procedures for manipulating complex numbers work 
with either representation.

It is simply not possible for everyone to agree in advance on choices of data 
representation. So in addition to the data-abstraction barriers that isolate 
representation from use, we need abstraction barriers that isolate different 
design choices from each other and permit different choices to coexist in a 
single program. Furthermore, since large programs are often created by combining
pre-existing modules that were designed in isolation, we need conventions that 
permit programmers to incorporate modules into larger systems additively, that 
is, without having to redesign or reimplement these modules.

Our main technique for building generic procedures will be to work in terms of 
data objects that have type tags, that is, data objects that include explicit 
information about how they are to be processed.

We begin with the simple complex-number example. We will see how type tags and 
data-directed style enable us to design separate rectangular and polar 
representations for complex numbers while maintaining the notion of an abstract 
"complex-number" data object. |#

; = Representations of Complex Numbers =
#| We begin by discussing two plausible representations for complex numbers as 
ordered pairs: rectangular form (real part and imaginary part) and polar form 
 (magnitude and angle). Complex numbers are naturally represented as ordered 
pairs. The set of complex numbers can be thought of as a two-dimensional space 
with two orthogonal axes, the "real" axis and the "imaginary" axis. From this 
point of view, the complex number z = x + iy (where i**2 = - 1) can be thought 
of as the point in the plane whose real coordinate is x and whose imaginary 
coordinate is y. Addition of complex numbers reduces in this representation to 
addition of coordinates: 

Real-Part(z1 + z2) = Real-Part(z1) + Real-Part(z2)
Imaginary-Part(z1 + z2) = Imaginary-Part(z1) + Imginary-Part(z2)

When multiplying complex numbers, it is more natural to think in terms of 
representing a complex number in polar form, as a magnitude and an angle. The 
product of two complex numbers is the vector obtained by stretching one complex 
number by the length of the other and then rotating it through the angle of the 
other:

Magnitude(z1 * z2) = Magnitude(z1) * Magnitude(z2)
Angle(z1 * z2) = Angle(z1) + Angle(z2)

Thus, there are two different representations for complex numbers, which are 
appropriate for different operations. Yet, from the viewpoint of someone writing
a program that uses complex numbers, the principle of data abstraction suggests 
that all the operations for manipulating complex numbers should be available 
regardless of which representation is used by the computer. For example, it is 
often useful to be able to find the magnitude of a complex number that is 
specified by rectangular coordinates. Similarly, it is often useful to be able 
to determine the real part of a complex number that is specified by polar 
coordinates. 

Assume that the operations on complex numbers are implemented in terms of four 
selectors: real-part, imag-part, magnitude, and angle. Also assume that we have 
two procedures for constructing complex numbers: make-from-real-imag returns a 
complex number with specified real and imaginary parts, and make-from-mag-ang 
returns a complex number with specified magnitude and angle. |#

; Both produce complex numbers equal to z:
(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

#| Add and subtract complex numbers in terms of real and imaginary parts while 
multiplying and dividing complex numbers in terms of magnitudes and angles: |#
(defun add-complex (z1 z2)
  "Add complex numbers Z1 and Z2 using the rectangular form"
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  "Subtract complex number Z2 from Z1 using the rectangular form"
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
  "Multiply complex numbers Z1 and Z2 using the polar form"
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  "Divide complex number Z1 by Z2 using the polar form"
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

#| To complete the complex-number package, we must choose a representation and 
we must implement the constructors and selectors in terms of primitive numbers 
and primitive list structure. The rectangular and polar representations are 
related in the following way:

x = (* r (cos A))
y = (* r (sin A))
r = (sqrt (+ (expt x 2) (expt y 2)))
A = (atan y x)

The final form of constructors and selectors depend on the chosen 
representation. |#


; = Tagged Data =
#| One way to view data abstraction is as an application of the "principle of 
least commitment". In implementing the complex-number system, we can use either 
representation. The abstraction barrier formed by the selectors and constructors
permits us to defer to the last possible moment the choice of a concrete 
representation for our data objects and thus retain maximum flexibility in our 
system design. The principle of least commitment can be carried to even further 
extremes. If we desire, we can maintain the ambiguity of representation even 
after we have designed the selectors and constructors, and elect to use both 
representation. If both representations are included in a single system, 
however, we will need some way to distinguish data in polar form from data in 
rectangular form. 

A straightforward way to accomplish this distinction is to include a type tag 
--the symbol rectangular or polar -- as part of each complex number. Then when 
we need to manipulate a complex number we can use the tag to decide which 
selector to apply.

In order to manipulate tagged data, we will assume that we have procedures 
type-tag and contents that extract from a data object the tag and the actual 
contents (the polar or rectangular coordinates, in the case of a complex 
number). We will also postulate a procedure attach-tag that takes a tag and 
contents and produces a tagged data object. A straightforward way to implement 
this is to use ordinary list structure: |#

(defun attach-tag (type-tag contents)
  "Create tagged data"
  (cons type-tag contents))

(defun type-tag (datum)
  "Return the tag portion of a tagged data pair"
  (if (consp datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(defun contents (datum)
  "Return the data portion of a tagged data pair"
  (if (consp datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

#| Using these procedures, we can define predicates rectangular? and polar?, 
which recognize polar and rectangular numbers, respectively: |#

(defun rectangular? (z)
  "Return true if Z has a RECTANGULAR tag"
  (eql (type-tag z) 'rectangular))

(defun polar? (z)
  "Return true if Z has a POLAR tag"
  (eql (type-tag z) 'polar))

#| With the implementation of tagged data, you can now modify the code so that 
the two different representations can coexist in the same system.  You must make
sure that the names of their procedures do not conflict. One way to do this is 
to add a suffix to each function to denote the representation that it operates
on. |#

; - Rectangular -

(defun real-part-rectangular (z) 
  "Returns the real part of a tagged rectangular complex number"
  (car z))

(defun imag-part-rectangular (z) 
  "Return the imaginary part of a tagged rectangular complex number"
  (cdr z))

(defun magnitude-rectangular (z)
  "Return the magnitude of a tagged rectangular complex number"
  (sqrt (+ (expt (real-part-rectangular z) 2)
           (expt (imag-part-rectangular z) 2))))

(defun angle-rectangular (z)
  "Return the angle of a tagged rectangular complex number"
  (atan (imag-part-rectangular z) ; ATAN similar in Scheme and CL
        (real-part-rectangular z)))

(defun make-from-real-imag-rectangular (x y)
  "Return a tagged rectangular complex number given real and imag parts X and Y"
  (attach-tag 'rectangular (cons x y)))

(defun make-from-mag-ang-rectangular (r a)
  "Return a tagged rectangular complex number given a radius and angle R and A"
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; - Polar -

(defun real-part-polar (z)
  "Return the real part of a tagged polar complex number"
  (* (magnitude-polar z) (cos (angle-polar z))))

(defun imag-part-polar (z)
  "Return the imaginary part of a tagged polar complex number"
  (* (magnitude-polar z) (sin (angle-polar z))))

(defun magnitude-polar (z) 
  "Return the magnitude of a tagged polar complex number"
  (car z))

(defun angle-polar (z) 
  "Return the angle of a tagged polar complex number"
  (cdr z))

(defun make-from-real-imag-polar (x y)
  "Return a tagged polar complex number given real and imag components X and Y"
  (attach-tag 'polar
               (cons (sqrt (+ (expt x 2) (expt y 2)))
                     (atan y x))))

(defun make-from-mag-ang-polar (r a)
  "Return a tagged polar complex number given the radius and angle R and A"
  (attach-tag 'polar (cons r a)))

#| Each generic selector is implemented as a procedure that checks the tag of 
its argument and calls the appropriate procedure for handling data of that 
type. |#

(defun real-part (z)
  "Return the real part of a tagged complex number, rectangular or polar"
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (t (error "Unknown type -- REAL-PART" z))))

(defun imag-part (z)
  "Return the imag part of a tagged complex number, rectangular or polar"
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (t (error "Unknown type -- IMAG-PART" z))))

(defun magnitude (z)
  "Return the radius of a taggeg complex number, rectangular or polar"
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (t (error "Unknown type -- MAGNITUDE" z))))

(defun angle (z)
  "Return the angle of a tagged complex number, rectangular or polar"
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (t (error "Unknown type -- ANGLE" z))))

#| To implement the complex-number arithmetic operations, we can use the same 
procedures add-complex, sub-complex, mul-complex, and div-complex from above, 
because the selectors they call are generic, and so will work with either 
representation. 

The system has been decomposed into three relatively independent parts: the 
complex-number-arithmetic operations, polar complex number implementation, and 
rectangular complex number implementation. The polar and rectangular 
implementations could have been written by programmers working separately, and 
both of these can be used as underlying representations by a third programmer 
implementing the complex-arithmetic procedures in terms of the abstract 
constructor/selector interface. |#