#|
PCL-01--Structure-and-Syntax.lisp
James Watson, 2011 December
General comments on Lisp syntax, usage, and style
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

#|
NOTE: DO NOT EXECUTE!  CODE SEGMENTS FOR INSTRUCTION ONLY!
If run, this file will crash at best.  No functionality guaranteed or implied.
|#

; This is a comment.
;;; so is this.

; It is common Lisp practice that the number of ';' increase with the generality
;of the comments or to the amount of code to which the comments apply

#|
	This is a block comment
|#

;; == Emacs Guide ==
; = Keyboard Commands =
C-[char] ; Control + [key]
M-[char] ; Alt (Meta) + [key]

; = Tools =
M-x check-parens ; Emacs checks that parentheses and quotes all match
#| This command is especially useful for "READ failure" type errors |#

;; == REPL Interactive Environment ==
#| Like Python, Lisp-Box has a command-line interactive environment, called
the REPL, the Read-Eval-Print Loop. |#

#| The variable *query-io* is a global variable that contains the input stream 
connected to the terminal. |#

; = REPL/SLIME Functions & Commands =
M-x slime-pwd ; display the present working direcotry
M-x slime-cd ; Change the directory of Lisp process and REPL buffer
(load "hello.lisp") ; Load and compile a file 
                    ; Should set the working directory first
M-p ; Last entered command

; == Lisp Syntax and Structure ==
#| There are two levels of syntax defined by Common Lisp: 
the syntax of s-expressions understood by the reader and 
the syntax of Lisp forms understood by the evaluator. The evaluator is given an
expression and generates code that will compute the appropriate value when it's
run. 
   The reader is responsible for translating text into Lisp objects, and the 
Lisp evaluator then deals only with those objects. For a given number of a given
type, there can be many different textual representations, all of which will be 
translated to the same object representation by the Lisp reader. For instance, 
you can write the integer 10 as 10, 20/2, #xA, or any of a number of other ways,
but the reader will translate all these to the same object.
   This split has a couple of consequences. One is that you can use 
s-expressions as an externalizable data format for data other than source code,
using READ to read it and PRINT to print it.  The other consequence is that 
since the semantics of the language are defined in terms of trees of objects 
rather than strings of characters, it's easier to generate code within the 
language than it would be if you had to generate code as text.  The real win
is that you can generate code by manipulating existing data. This is the basis 
for Lisp's macros|#

; = S-expressions =
#| The basic elements of s-expressions are lists and atoms. Lists are delimited
by parentheses and can contain any number of whitespace-separated elements. 
Atoms are everything else. The elements of lists are themselves s-expressions 
 (in other words, atoms or nested lists).  Not all valid s-expressions are valid
Lisp forms.  Below are some examples of s-sxpressions: |#
x             ; the symbol X
()            ; the empty list
(1 2 3)       ; a list of three numbers
("foo" "bar") ; a list of two strings
(x y z)       ; a list of three symbols
(x 1 "foo")   ; a list of a symbol, a number, and a string
(+ (* 2 3) 4) ; a list of a symbol, a list, and a number.

; = S-expressions as Lisp Forms =
#| After the reader has translated a bunch of text into s-expressions, the 
s-expressions can then be evaluated as Lisp code, if they are valid forms. The 
following are proper forms:
* Any Atom (non-list atoms, or the empty list)
   - Atoms are self-evaluating objects
* Any list that has a symbol as its first element
   - Functions
   - Macros
   - Special (Operator) Forms

If the symbol hasn't been defined yet it's assumed to be a function name. If you
later bind something other than a function to that name, you will likely have to
compile the block with the previously un-defined name again.|#

;; == Atoms and Data Types ==
#| An atom is any object that is not a cons.  In other words, an indevisible
object that is not nested, branched, or otherwise composite. |#

; = Legal Number Atoms, Examples =
123         ; the integer one hundred twenty-three
3/7         ; the ratio three-sevenths
1.0         ; the floating-point number one in default precision
1.0e0       ; another way to write the same floating-point number
1.0d0       ; the floating-point number one in "double" precision
1.0e-4      ; the floating-point equivalent to one-ten-thousandth
+42         ; the integer forty-two
-42         ; the integer negative forty-two
-1/4        ; the ratio negative one-quarter
#b10101     #| --> |# 21    ; base 10
#b1010/1011 #| --> |# 10/11 ; base 10
#o777       #| --> |# 511   ; base 10
#xDADA      #| --> |# 56026 ; base 10

; = Integer and Rational Literals =
#| The syntax for integer values is an optional sign (+ or -) followed by one or
more digits. Ratios are written as an optional sign and a sequence of digits, 
representing the numerator, a slash (/), and another sequence of digits 
representing the denominator. All rational numbers are "canonicalized" as 
they're read--that's why 10 and 20/2 are both read as the same number, as are 
3/4 and 6/8. Rationals are printed in "reduced" form--integer values are printed
in integer syntax and ratios with the numerator and denominator reduced to 
lowest terms. 

It's also possible to write rationals in bases other than 10. If preceded by #B 
or #b, a rational literal is read as a binary number with 0 and 1 as the only 
legal digits. An #O or #o indicates an octal number (legal digits 0-7), and #X 
or #x indicates hexadecimal (legal digits 0-F or 0-f). You can write rationals 
in other bases from 2 to 36 with #nR where n is the base (always written in 
decimal). Additional "digits" beyond 9 are taken from the letters A-Z or a-z. 
Note that these radix indicators apply to the whole rational--it's not possible 
to write a ratio with the numerator in one base and denominator in another. 

; = Floating Point Number Literals =  
You can also write floating-point numbers in a variety of ways. Unlike rational 
numbers, the syntax used to notate a floating-point number can affect the actual
type of number read. Common Lisp defines four subtypes of floating-point number:
short, single, double, and long. Each subtype can use a different number of bits
in its representation, which means each subtype can represent values spanning a 
different range and with different precision.

The basic format for floating-point numbers is an optional sign followed by a 
nonempty sequence of decimal digits possibly with an embedded decimal point. 
This sequence can be followed by an exponent marker for "computerized scientific
notation." The letter does double duty: it marks the beginning of the exponent 
and indicates what floating- point representation should be used for the number.
The exponent markers s, f, d, l (and their uppercase equivalents) indicate 
short, single, double, and long floats, respectively. The letter e indicates 
that the default representation (initially single-float) should be used.

Numbers with no exponent marker are read in the default representation and must 
contain a decimal point followed by at least one digit to distinguish them from 
integers. The digits in a floating-point number are always treated as base 10 
digits. |#

; = Complex Number Literals =
#| Finally, complex numbers are written in their own syntax, namely, #C or #c 
followed by a list of two real numbers representing the real and imaginary part 
of the complex number. There are actually five kinds of complex numbers because 
the real and imaginary parts must either both be rational or both be the same 
kind of floating-point number. 

But you can write them however you want--if a complex is written with one 
rational and one floating-point part, the rational is converted to a float of 
the appropriate representation. Similarly, if the real and imaginary parts are 
both floats of different representations, the one in the smaller representation 
will be upgraded. |#
#c(2      1)   #| --> |# #c(2 1)
#c(2/3  3/4)   #| --> |# #c(2/3 3/4)
#c(2    1.0)   #| --> |# #c(2.0 1.0)
#c(2.0  1.0d0) #| --> |# #c(2.0d0 1.0d0)
#c(1/2  1.0)   #| --> |# #c(0.5 1.0)
#c(3      0)   #| --> |# 3
#c(3.0  0.0)   #| --> |# #c(3.0 0.0)
#c(1/2    0)   #| --> |# 1/2 ; no imaginary component, evaluates to rational
#c(-6/3   0)   #| --> |# -2

; = Character Literals =
#\x ;the character 'x'
#| Any character can be used after the #\, including otherwise special 
characters such as ", (, and whitespace. However, writing whitespace characters 
this way isn't very (human) readable; an alternative syntax for certain 
characters is #\ followed by the character's name. Exactly what names are 
supported depends on the character set and on the Lisp implementation, but all 
implementations support the names Space and Newline. Thus, you should write 
#\Space instead of #\ , though the latter is technically legal. Other 
semistandard names are Tab, Page, Rubout, Linefeed, Return, and Backspace. |#


; = String Literals, Examples =
"foo"     ; the string containing the characters f, o, and o.
"fo\o"    ; the same string
"fo\\o"   ; the string containing the characters f, o, \, and o.
"fo\"o"   ; the string containing the characters f, o, ", and o.
#| Strings in Common Lisp are really a composite data type, namely, a 
one-dimensional array of characters. You can include any character supported by 
the character set in a literal string except double quote (") and backslash (\).
And you can include these two as well if you escape them with a backslash. In 
fact, backslash always escapes the next character, whatever it is, though this 
isn't necessary for any character except for " and itself. |#

; = Symbols =
#| Names used in Lisp programs, such as FORMAT and hello-world, and *db* are 
represented by objects called symbols. The reader knows nothing about how a 
given name is going to be used--whether it's the name of a variable, a function,
or something else. It just reads a sequence of characters and builds an object 
to represent the name. Almost any character can appear in a name. Whitespace 
characters can't, though, because the elements of lists are separated by 
whitespace. Digits can appear in names as long as the name as a whole can't be 
interpreted as a number. Similarly, names can contain periods, but the reader 
can't read a name that consists only of periods. |#
; Ten characters that serve other syntactic purposes can't appear in names:
; : ( ) " ' ` , \ |
#| While reading names, the reader converts all unescaped characters in a name 
to their uppercase equivalents. Thus, the reader will read foo, Foo, and FOO as
the same symbol: FOO. However, \f\o\o and |foo| will both be read as foo, which
is a different object than the symbol FOO. |#



; = Mathematical Operations =
#| Math operation forms follow prefix notation, obviating the need to have an
operator symbol in between each and every operand. Calling any of these 
functions with more than two arguments is equivalent to calling the same 
function on the first two arguments and then calling it again on the resulting 
value and the rest of the arguments. For example, (+ 1 2 3) is equivalent to
 (+ (+ 1 2) 3). With only one argument, + and * return the value; - returns its 
negation and / its reciprocal. |#
(+ 137 349)         #| --> |# 486     ; Addition (Integer)
(+ 2.7 10)          #| --> |# 12.7    ; Addition (Float)
(+ #c(1 2) #c(3 4)) #| --> |# #c(4 6) ; Addition (Complex)
(- 1000 335)        #| --> |# 665     ; Subtraction
(* 5 99)            #| --> |# 495     ; Multiplication
(/ 10 5)            #| --> |# 2       ; Division
(/ 4)               #| --> |# 1/4     ; Reciprocal
; Nested computations
(+ (* 3 5) (- 10 6)) #| --> |# 19
#| The order of operations is determined solely by the basis of paren nesting.

+ and * can also be called with no arguments, in which case they return the 
appropriate identity: 0 for + and 1 for *.

The functions 1+ and 1- provide a shorthand way to express adding and 
subtracting one from a number. Note that these are different from the macros 
INCF and DECF. 1+ and 1- are just functions that return a new value, but INCF 
and DECF modify a place.

Floating-point and complex numbers are contagious--if all the arguments are 
reals but one or more are floating-point numbers, the other arguments are 
converted to the nearest floating-point value in a "largest" floating-point 
representation of the actual floating-point arguments. Floating-point numbers in
a "smaller" representation are also converted to the larger representation. 
Similarly, if any of the arguments are complex, any real arguments are converted
to the complex equivalents.

Because / doesn't truncate, Common Lisp provides four flavors of truncating and 
rounding for converting a real number (rational or floating point) to an 
integer: FLOOR truncates toward negative infinity, returning the largest integer
less than or equal to the argument. CEILING truncates toward positive infinity, 
returning the smallest integer greater than or equal to the argument. TRUNCATE 
truncates toward zero, making it equivalent to FLOOR for positive arguments and 
to CEILING for negative arguments. And ROUND rounds to the nearest integer. If 
the argument is exactly halfway between two integers, it rounds to the nearest 
even integer.|#


; == Lists ==
(list 1 2 3) ; The 'list' function returns a list composed of its arguments
; = Associative (Property) List =
; Designate property names (keys) with the keyword symbol ':' preceding the
;propert name, followed by the property value (separated by a space)
(list :a 1 :b 2 :c3)
; Retrieve a value of a property with 'getf'
(getf(list :a 1 :b 2 :c 3) :a) ; Result: 3
; (getf [list] [key])

; == Variables ==
(defvar *db* nil) ; Create var *db* with initial value 'nil'
; *varName* with "*" on each side is a Lisp naming convention for global vars
; Note that "*" is perfectly legal in a var name


; == Truth & Falsehood ==
#| The symbol 'NIL' is the only false value, and everything else is true.  
The symbol T is the canonical true value and can be used when you need to 
return a non-NIL value and don't have anything else handy.

NIL is the only object that's both an atom and a list: in addition to falsehood,
it's also used to represent the empty list. [If the reader sees (), it reads it 
as the symbol 'NIL'.]|#


; == Equality ==
; - = -
#| The function = is the numeric equality predicate. It compares numbers by 
mathematical value, ignoring differences in type. Thus, = will consider 
mathematically equivalent values of different types equivalent while the generic
equality predicate EQL would consider them inequivalent because of the 
difference in type.  If it's called with more than two arguments, it returns 
true only if they all have the same value. Thus: |#
(= 1 1)                       #| --> |# T
(= 10 20/2)                   #| --> |# T
(= 1 1.0 #c(1.0 0.0) #c(1 0)) #| --> |# T

; = eq =
#| EQ tests for "object identity"--two objects are EQ if they're identical. 
Unfortunately, the object identity of numbers and characters depends on how 
those data types are implemented in a particular Lisp. Thus, you should never 
use EQ to compare values that may be numbers or characters. |#

; = eql =
#| Common Lisp defines EQL to behave like EQ except that it also is guaranteed 
to consider two objects of the same class representing the same numeric or 
character value to be equivalent. Thus,|# (eql 1 1) #|is guaranteed to be 
true. And|# (eql 1 1.0) #|is guaranteed to be false since the integer value "1" 
and the floating-point value "1.0" are instances of different classes. |#

; = equal =
#| EQUAL loosens the discrimination of EQL to consider lists equivalent if they
have the same structure and contents, recursively, according to EQUAL. EQUAL 
also considers strings equivalent if they contain the same characters. It also 
defines a looser definition of equivalence than EQL for bit vectors and 
pathnames. For all other types, it falls back on EQL. |#

; = equalp =
#| EQUALP is similar to EQUAL except it's even less discriminating. It considers
two strings equivalent if they contain the same characters, ignoring differences
in case. It also considers two characters equivalent if they differ only in 
case. Numbers are equivalent under EQUALP if they represent the same 
mathematical value. EQUALP uses = to compare numbers. Thus,|# (equalp 1 1.0) 
#|is true. Lists with EQUALP elements are EQUALP; likewise, arrays with EQUALP 
elements are EQUALP. |#

; - /= -
; The /= function returns true only if all its arguments are different values:
(/= 1 1)       #| --> |# NIL
(/= 1 2)       #| --> |# T
(/= 1 2 3)     #| --> |# T
(/= 1 2 3 1)   #| --> |# NIL
(/= 1 2 3 1.0) #| --> |# NIL

; == Interpreter Flags & Options ==
#| Special flags can be used to selectively exempt forms and symbols from 
evaluation. The desired result is a form or series of forms meant to be 
evaluated at a different level or time. Another useful way to think about the 
this syntax is as a particularly concise way of writing code that generates 
lists. The techniques below are also commonly used in the definition of macros,
which generate and assemble expressions to be evaluated at runtime. |#

; = ' =
#| A single quote stops the Lisp from evaluating the form, allowing it to
pass verbatim to wherever it is being passed. |#
(some-function '(some-form arg1 arg2))
(some-form arg1 arg2) ; will not be evaluated before being sent to some-function

; = ` =
#| A single back quote before an expression keeps it from being evaluated.
Any sub-expression inside the back-quoted expression that is preceded by a 
comma >will< be evaluated. |#
`(1 2 (+ 1 2))  #| --> |# (1 2 (+ 1 2)); Nothing is evaluated
`(1 2 ,(+ 1 2)) #| --> |# (1 2 3)      ; Inner expression was evaluated 

; = @ =
#| "Splices" the value of the follwing expression into the enclosing list.
The expression follwing '@' must evaluate to a list. |#
`(and ,(list 1 2 3))   #| --> |# (AND (1 2 3))
`(and ,@(list 1 2 3))  #| --> |# (AND 1 2 3)
; Splicing into the middle of a list
`(and ,@(list 1 2 3) 4) #| --> |# (AND 1 2 3 4)

; - Some Flags and Their Equivalents -
; Backquote Syntax    Equivalent List-Building Code           Result
  `(a (+ 1 2) c)      (list 'a '(+ 1 2) 'c)                   (a (+ 1 2) c)
  `(a ,(+ 1 2) c)     (list 'a (+ 1 2) 'c)	              (a 3 c)
  `(a (list 1 2) c)   (list 'a '(list 1 2) 'c)                (a (list 1 2) c)
  `(a ,(list 1 2) c)  (list 'a (list 1 2) 'c)                 (a (1 2) c)
  `(a ,@(list 1 2) c) (append (list 'a) (list 1 2) (list 'c)) (a 1 2 c)