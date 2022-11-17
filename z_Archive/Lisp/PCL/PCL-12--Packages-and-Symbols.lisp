#|
PCL-12--Packages-and-Symbols.lisp
James Watson, 2012 March
Packaging code into modules, Creating/controlling name-spaces
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

;; == How the Reader Uses Packages ==
#| You can think of a package as a table that maps strings to symbols. The 
actual mapping is slightly more flexible than a simple lookup table but not in 
ways that matter much to the reader. Each package also has a name, which can be 
used to find the package using the function FIND-PACKAGE. 

The two key functions that the reader uses to access the name-to-symbol mappings
in a package are FIND-SYMBOL and INTERN. Both these functions take a string and,
optionally, a package. If not supplied, the package argument defaults to the 
value of the global variable *PACKAGE*, also called the current package.

FIND-SYMBOL looks in the package for a symbol with the given string for a name 
and returns it, or NIL if no symbol is found. INTERN also will return an 
existing symbol; otherwise it creates a new symbol with the string as its name 
and adds it to the package.

Most names you use are unqualified, names that contain no colons. When the 
reader reads such a name, it translates it to a symbol by converting any 
unescaped letters to uppercase and passing the resulting string to INTERN. Thus,
each time the reader reads the same name in the same package, it'll get the same
symbol object. This is important because the evaluator uses the object identity 
of symbols to determine which function, variable, or other program element a 
given symbol refers to. Thus, the reason an expression such as (hello-world) 
results in calling a particular hello-world function is because the reader 
returns the same symbol when it reads the function call as it did when it read 
the DEFUN form that defined the function.

A name containing either a single colon or a double colon is a package-qualified
name. When the reader reads a package-qualified name, it splits the name on the 
colon(s) and uses the first part as the name of a package and the second part as
the name of the symbol. The reader looks up the appropriate package and uses it 
to translate the symbol name to a symbol object. 

A name containing only a single colon must refer to an external symbol--one the 
package exports for public use. If the named package doesn't contain a symbol 
with a given name, or if it does but it hasn't been exported, the reader signals
an error. A double-colon name can refer to any symbol from the named package, 
though it's usually a bad idea--the set of exported symbols defines a package's 
public interface. 

Two other bits of symbol syntax the reader understands are those for keyword 
symbols and uninterned symbols. Keyword symbols are written with names starting 
with a colon. Such symbols are interned in the package named KEYWORD and 
automatically exported. Additionally, when the reader interns a symbol in the 
KEYWORD, it also defines a constant variable with the symbol as both its name 
and value. This is why you can use keywords in argument lists without quoting 
them--when they appear in a value position, they evaluate to themselves. 
Thus: |# (eql ':foo :foo) #|-->|# T

#| The names of keyword symbols, like all symbols, are converted to all 
uppercase by the reader before they're interned. The name doesn't include the 
leading colon. |# (symbol-name :foo) #|-->|# "FOO"

#| Uninterned symbols are written with a leading #:. These names (minus the #:) 
are converted to uppercase as normal and then translated into symbols, but the 
symbols aren't interned in any package; each time the reader reads a #: name, it
creates a new symbol. Thus: |# (eql '#:foo '#:foo) #|-->|# NIL

;; == Package and Symbol Terms ==
#| At its core, every package contains a name-to-symbol lookup table, but a 
symbol can be made accessible via an unqualified name in a given package in 
other ways. |#

#| To start with, all the symbols that can be found in a given package using 
FIND-SYMBOL are said to be "accessible" in that package. In other words, the 
accessible symbols in a package are those that can be referred to with 
unqualified names when the package is current. 

A symbol can be accessible in two ways. The first is for the package's 
name-to-symbol table to contain an entry for the symbol, in which case the 
symbol is said to be "present" in the package. When the reader interns a new 
symbol in a package, it's added to the package's name-to-symbol table. The 
package in which a symbol is first interned is called the symbol's "home 
package". 

The other way a symbol can be accessible in a package is if the package 
"inherits" it. A package inherits symbols from other packages by "using" the 
other packages. Only "external" symbols in the used packages are inherited. A 
symbol is made external in a package by "exporting" it. In addition to causing 
it to be inherited by using packages, exporting a symbol also--as you saw in the
previous section--makes it possible to refer to the symbol using a single-colon 
qualified name. 

To keep the mappings from names to symbols deterministic, the package system 
allows only one symbol to be accessible in a given package for each name. 
However, you can resolve conflicts by making one of the accessible symbols a 
"shadowing" symbol, which makes the other symbols of the same name inaccessible.
In addition to its name-to-symbol table, each package maintains a list of 
shadowing symbols. 

An existing symbol can be "imported" into another package by adding it to the 
package's name-to-symbol table. Thus, the same symbol can be present in multiple
packages. Sometimes you'll import symbols simply because you want them to be 
accessible in the importing package without using their home package. Other 
times you'll import a symbol because only present symbols can be exported or be 
shadowing symbols. For instance, if a package needs to use two packages that 
have external symbols of the same name, one of the symbols must be imported into
the using package in order to be added to its shadowing list and make the other 
symbol inaccessible. 

A present symbol can be "uninterned" from a package, which causes it to be 
removed from the name-to-symbol table and, if it's a shadowing symbol, from the 
shadowing list. You might unintern a symbol from a package to resolve a conflict
between the symbol and an external symbol from a package you want to use. A 
symbol that isn't present in any package is called an uninterned symbol, can no 
longer be read by the reader, and will be printed using the #:foo syntax. |#


;; == Three Standard Packages ==
; = COMMON-LISP and COMMON-LISP-USER =
#| When you first start Lisp, the value of *PACKAGE* is typically the 
COMMON-LISP-USER package, also known as CL-USER. CL-USER uses the package 
COMMON-LISP, which exports all the names defined by the language standard. Thus,
when you type an expression at the REPL, all the names of standard functions, 
macros, variables, and so on, will be translated to the symbols exported from 
COMMON-LISP, and all other names will be interned in the COMMON-LISP-USER 
package. |#
*package* #|-->|# #<PACKAGE "COMMON-LISP-USER">

#| The name *PACKAGE* is exported from COMMON-LISP, and because 
COMMON-LISP-USER uses COMMON-LISP, you can use a package-qualified name. |#
common-lisp:*package* #|-->|# #<PACKAGE "COMMON-LISP-USER">
; which can also be accessed by the package nickname
cl:*package* #|-->|#  #<PACKAGE "COMMON-LISP-USER">

; When you define a variable
(defvar *x* 10) #|-->|# *X*
#| the reader reads DEFVAR as the symbol from the COMMON-LISP package and *X* as
a symbol in COMMON-LISP-USER. |#

#| The REPL can't start in the COMMON-LISP package because you're not allowed to
intern new symbols in it; COMMON-LISP-USER serves as a "scratch" package where 
you can create your own names while still having easy access to all the symbols 
in COMMON-LISP.3 Typically, all packages you'll define will also use 
COMMON-LISP, so you don't have to write things like this: |#
(cl:defun (x) (cl:+ x 2)) ; unnecessary

#| To see what packages COMMON-LISP-USER inherits symbols from in a particular 
implementation, evaluate this expression at the REPL: |#
(mapcar #'package-name (package-use-list :cl-user))
; And to find out what package a symbol came from originally, evaluate this:
(package-name (symbol-package 'some-symbol))
; with some-symbol replaced by the symbol in question.
#| Symbols inherited from implementation-defined packages will return some value
other than COMMON-LISP or COMMON-LISP-USER. |#

; = KEYWORD =
#| The third standard package is the KEYWORD package, the package the Lisp 
reader uses to intern names starting with colon. Thus, you can also refer to any
keyword symbol with an explicit package qualification of keyword like this: |#
:a #|-->|# :A
keyword:a #|-->|# :A
(eql :a keyword:a) #|-->|# T


;; == Defining Your Own Packages ==
#| When you write libraries that you intend to use in different contexts, you'll
want to define separate packages and then export the symbols that make up the 
libraries' public APIs. 

However, it's important to understand one thing about what packages do not do. 
Packages don't provide direct control over who can call what function or access 
what variable. They provide you with basic control over namespaces by 
controlling how the reader translates textual names into symbol objects. Thus, 
it doesn't make sense to talk about exporting a function or a variable from a 
package. You can export symbols to make certain names easier to refer to, but 
the package system doesn't allow you to restrict how those names are used. 

You define new packages with the macro DEFPACKAGE, which allows you to not only 
create the package but to specify what packages it uses, what symbols it 
exports, and what symbols it imports from other packages and to resolve 
conflicts by creating shadowing symbols.

If the application is simple enough to be written with no libraries beyond the 
facilities provided by the language itself, you could define a simple package 
like this: |#
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp))
#| This defines a package, named COM.GIGAMONKEYS.EMAIL-DB, that inherits all the
symbols exported by the COMMON-LISP package. 

You actually have several choices of how to represent the names of packages and 
the names of symbols in a DEFPACKAGE. Packages and symbols are named with 
strings. However, in a DEFPACKAGE form, you can specify the names of packages 
and symbols with string designators.  Using keyword symbols, as in the previous 
DEFPACKAGE, is a common style that allows you to write the names in lowercase--
the reader will convert the names to uppercase for you. You could also write the
DEFPACKAGE with strings, but then you have to write them in all uppercase.

You could also use nonkeyword symbols- but then the very act of reading the 
DEFPACKAGE form would cause those symbols to be interned in the current 
package. 

To read code in this package, you need to make it the current package with the 
IN-PACKAGE macro: |#
(in-package :com.gigamonkeys.email-db)
#| If you type this expression at the REPL, it will change the value of 
*PACKAGE*, affecting how the REPL reads subsequent expressions, until you change
it with another call to IN-PACKAGE. Similarly, if you include an IN-PACKAGE in a
file that's loaded with LOAD or compiled with COMPILE-FILE, it will change the 
package, affecting the way subsequent expressions in the file are read. 
IN-PACKAGE expands into code that will run when the file is compiled by 
COMPILE-FILE as well as when the file is loaded, changing the way the reader 
reads the rest of the file during compilation. 

With the current package set to the COM.GIGAMONKEYS.EMAIL-DB package, other than
names inherited from the COMMON-LISP package, you can use any name you want for 
whatever purpose you want. Thus, you could define a new hello-world function 
that could coexist with the hello-world function previously defined in 
COMMON-LISP-USER. |#


; = Packaging Reusable Libraries =
#| You might realize that functions you have written could be useful in other 
programs and decide to repackage them as a library. You should define a new 
package, and export certain names to make them available to other packages. |#
(defpackage :com.gigamonkeys.text-db
  (:use :common-lisp)
  (:export :open-db   
           :save
           :store))
#| The :export clause specifies names that will be external in 
COM.GIGAMONKEYS.TEXT-DB and thus accessible in packages that :use it. Therefore,
if you wanted the above COM.GIGAMONKEYS.EMAIL-DB package to use 
COM.GIGAMONKEYS.TEXT-DB, you can change the email package definition to the 
following: |#
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db))
#| Now code written in COM.GIGAMONKEYS.EMAIL-DB can use unqualified names to 
refer to the exported symbols from both COMMON-LISP and 
COM.GIGAMONKEYS.TEXT-DB. |#

; = Importing Individual Names =
#| Now suppose you find a third-party library of functions for manipulating 
e-mail messages. The names used in the library's API are exported from the 
package COM.ACME.EMAIL, so you could :use that package to get easy access to 
those names. But suppose you need to use only one function from this library, 
and other exported symbols conflict with names you already use (or plan to use) 
in our own code. In this case, you can import the one symbol you need with an 
:import-from clause in the DEFPACKAGE. For instance, if the name of the function
you want to use is parse-email-address, you can change the DEFPACKAGE to this:|#
(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp :com.gigamonkeys.text-db)
  (:import-from :com.acme.email :parse-email-address))
#| Now anywhere the name parse-email-address appears in code read in the 
COM.GIGAMONKEYS.EMAIL-DB package, it will be read as the symbol from 
COM.ACME.EMAIL. If you need to import more than one symbol from a single 
package, you can include multiple names after the package name in a single 
:import-from clause. A DEFPACKAGE can also include multiple :import-from clauses
in order to import symbols from different packages. 

Occasionally you'll run into the opposite situation--a package may export a 
bunch of names you want to use and a few you don't. Rather than listing all the 
symbols you do want to use in an :import-from clause, you can instead :use the 
package and then list the names you don't want to inherit in a :shadow clause. 
For instance, suppose the COM.ACME.TEXT package exports a bunch of names of 
functions and classes used in text processing. Further suppose that most of 
these functions and classes are ones you'll want to use in your code, but one of
the names, build-index, conflicts with a name you've already used. You can make 
the build-index from COM.ACME.TEXT inaccessible by shadowing it. |#
(defpackage :com.gigamonkeys.email-db
  (:use
   :common-lisp
   :com.gigamonkeys.text-db
   :com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :build-index))

; = Packaging Mechanics =
#| Because packages are used by the reader, a package must be defined before you
can LOAD or COMPILE-FILE a file that contains an IN-PACKAGE expression switching
to that package. Packages also must be defined before other DEFPACKAGE forms can
refer to them. For instance, if you're going to :use COM.GIGAMONKEYS.TEXT-DB in 
COM.GIGAMONKEYS.EMAIL-DB, then COM.GIGAMONKEYS.TEXT-DB's DEFPACKAGE must be 
evaluated before the DEFPACKAGE of COM.GIGAMONKEYS.EMAIL-DB. 

The best first step toward making sure packages exist when they need to is to 
put all your DEFPACKAGEs in files separate from the code that needs to be read 
in those packages. Some folks like to create a foo-package.lisp file for each 
individual package, and others create a single packages.lisp that contains all 
the DEFPACKAGE forms for a group of related packages. Either approach is 
reasonable, though the one-file-per-package approach also requires that you 
arrange to load the individual files in the right order according to the 
interpackage dependencies.

Either way, once all the DEFPACKAGE forms have been separated from the code that
will be read in the packages they define, you can arrange to LOAD the files 
containing the DEFPACKAGEs before you compile or load any of the other files. 
For simple programs you can do this by hand: simply LOAD the file or files 
containing the DEFPACKAGE forms, possibly compiling them with COMPILE-FILE 
first. Then LOAD the files that use those packages, again optionally compiling 
them first with COMPILE-FILE. Note, however, that the packages don't exist until
you LOAD the package definitions, either the source or the files produced by 
COMPILE-FILE. Thus, if you're compiling everything, you must still LOAD all the 
package definitions before you can COMPILE-FILE any files to be read in the 
packages. 

Doing these steps by hand will get tedious after a while. For simple programs 
you can automate the steps by writing a file, load.lisp, that contains the 
appropriate LOAD and COMPILE-FILE calls in the right order. Then you can just 
LOAD that file.  

The other key rule of thumb is that each file should contain exactly one 
IN-PACKAGE form, and it should be the first form in the file other than 
comments. Files containing DEFPACKAGE forms should start with (in-package 
"COMMON-LISP-USER"), and all other files should contain an IN-PACKAGE of one of 
your packages.

The other bit of packaging mechanics has to do with how to name packages. 
Package names live in a flat namespace--package names are just strings, and 
different packages must have textually distinct names. Thus, you have to 
consider the possibility of conflicts between package names. If you're using 
only packages you developed yourself, then you can probably get away with using 
short names for your packages. But if you're planning to use third-party 
libraries or to publish your code for use by other programmers, then you need to
follow a naming convention that will minimize the possibility of name collisions
between different packages. |#


; = Package Gotchas =
#|
* Forgetting to USE-PACKAGE before trying to invoke that package's functions
* After trying to invoke an un-used package, that name will have been interned,
  and trying to then use it after this mistake will cause a conflict.  The 
  debugger may have an option to un-intern the conflicting name
* Unintentionally defining a new function for name that was already exported.
  This will not cause an error.
|#