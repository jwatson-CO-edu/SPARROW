#|
PCL-08--File-IO.lisp
James Watson, 2012 January
Reading from and writing to files in Common Lisp, Streams
Borrowed heavily from Practical Common Lisp by Peter Seibel
URL: http://gigamonkeys.com/book/
|#

;; == Reading File Data ==
; The most basic file I/O task is to read the contents of a file.

; = open =
#| obtain a stream from which you can read a file's contents. By default OPEN 
returns a character-based input stream you can pass to a variety of functions 
that read one or more characters of text. The only required argument to OPEN is 
the name of the file to read. Common Lisp provides a couple of ways to 
represent a filename, but the simplest is to use a string containing the name in
the local file-naming syntax. So assuming that /some/file/name.txt is a file, 
you can open it like this:|#
(open "/some/file/name.txt")

; = read-char =
; reads a single character

; = read-line =
#| reads a line of text, returning it as a string with the end-of-line 
character(s) removed |#

; = close =
; Close the stream

; to print the first line of the file and then close it
(let ((in (open "/some/file/name.txt")))
  (format t "~a~%" (read-line in))
  (close in))

#| By default OPEN and the READ-* functions will signal an error in the 
situation that the requested file does not exist or when the file unexpectedly 
end while reading. Each of these functions accepts arguments that modify its 
behavior in these exceptional situations. 

If you want to open a possibly nonexistent file without OPEN signaling an error,
you can use the keyword argument :if-does-not-exist to specify a different 
behavior. The three possible values are :error, the default; :create, which 
tells it to go ahead and create the file and then proceed as if it had already 
existed; and NIL, which tells it to return NIL instead of a stream. Thus, you 
can change the previous example to deal with the possibility that the file may 
not exist. |#
(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

#| The reading functions all take an optional argument, which defaults to true, 
that specifies whether they should signal an error if they're called at the end 
of the file. If that argument is NIL, they instead return the value of their 
third argument, which defaults to NIL. Thus, you could print all the lines in a 
file like this: |#
(let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))

; = read =
#| Reads a single s-expression, returning a Lisp object.

Of the three text-reading functions, READ is unique to Lisp. This is the same 
function that provides the R in the REPL and that's used to read Lisp source 
code. Each time it's called, it reads a single s-expression, skipping whitespace
and comments, and returns the Lisp object denoted by the s-expression. For 
instance, suppose /some/file/name.txt has the following contents:  |#
(1 2 3)
456
"a string" ; this is a comment
((a b)
 (c d))

; You can read those expressions like this:
(defparameter *s* (open "/some/file/name.txt"))
(read *s*)  #|-->|# (1 2 3)
(read *s*)  #|-->|# 456
(read *s*)  #|-->|# "a string"   ; The comment is ignored, as expected
(read *s*)  #|-->|# ((A B) (C D)) ; reads entire statment even though it spans 
(close *s*) #|-->|# T             ;two lines


;; == Reading Binary Data ==
#| By default OPEN returns character streams, which translate the underlying 
bytes to characters according to a particular character-encoding scheme. To read
the raw bytes, you need to pass OPEN an :element-type argument of 
'(unsigned-byte 8).3 You can pass the resulting stream to the function 
READ-BYTE, which will return an integer between 0 and 255 each time it's called.
READ-BYTE, like the character-reading functions, also accepts optional arguments
to specify whether it should signal an error if called at the end of the file 
and what value to return if not. |#

; = read-sequence =
#| a bulk reading function that works with both character and binary streams. 
You pass it a sequence (typically a vector) and a stream, and it attempts to 
fill the sequence with data from the stream. It returns the index of the first 
element of the sequence that wasn't filled or the length of the sequence if it 
was able to completely fill it. You can also pass :start and :end keyword 
arguments to specify a subsequence that should be filled instead. The sequence 
argument must be a type that can hold elements of the stream's element type. 
Since most operating systems support some form of block I/O, READ-SEQUENCE is 
likely to be quite a bit more efficient than filling a sequence by repeatedly 
calling READ-BYTE or READ-CHAR. |#


;; == File Output ==
#| To write data to a file, you need an output stream, which you obtain by 
calling OPEN with a :direction keyword argument of :output. When opening a file 
for output, OPEN assumes the file shouldn't already exist and will signal an 
error if it does. However, you can change that behavior with the :if-exists 
keyword argument. Passing the value :supersede tells OPEN to replace the 
existing file. Passing :append causes OPEN to open the existing file such that 
new data will be written at the end of the file, while :overwrite returns a 
stream that will overwrite existing data starting from the beginning of the 
file. And passing NIL will cause OPEN to return NIL instead of a stream if the 
file already exists. A typical use of OPEN for output looks like this: |#
(open "/some/file/name.txt" :direction :output :if-exists :supersede)

; = write-char =
; writes a single character to the stream

; = write-line =
#| writes a string followed by a newline, which will be output as the 
appropriate end-of-line character or characters for the platform. |#

; = write-string =
#| writes a string without adding any end-of-line characters. |#

; = Newline Functions =
; - terpri -
#| short for "terminate print"--unconditionally prints a newline character |#

; - fresh-line -
#| prints a newline character unless the stream is at the beginning of a line. 
FRESH-LINE is handy when you want to avoid spurious blank lines in textual 
output generated by different functions called in sequence. |#

; = print =
#| prints an s-expression preceded by an end-of-line and followed by a space. |#

; = prin1 =
; prints just the s-expression

; = pprint =
#| prints s-expressions like PRINT and PRIN1 but using the "pretty printer," 
which tries to print its output in an aesthetically pleasing way. |#

#| However, not all objects can be printed in a form that READ will understand. 
The variable *PRINT-READABLY* controls what happens if you try to print such an 
object with PRINT, PRIN1, or PPRINT. When it's NIL, these functions will print 
the object in a special syntax that's guaranteed to cause READ to signal an 
error if it tries to read it; otherwise they will signal an error rather than 
print the object. |#

; = princ =
#| also prints Lisp objects, but in a way designed for human consumption. For 
instance, PRINC prints strings without quotation marks. |#

; = Writing Binary Data =
#| To write binary data to a file, you have to OPEN the file with the same 
:element-type argument as you did to read it: '(unsigned-byte 8). |#

; - write-byte -
; write individual bytes to the stream

; - write-sequence -
#| Accepts both binary and character streams as long as all the elements of the 
sequence are of an appropriate type for the stream, either characters or bytes. 
As with READ-SEQUENCE, this function is likely to be quite a bit more efficient 
than writing the elements of the sequence one at a time. |#

; = Closing Files =
#| It's important to close files when you're done with them. 
You could always structure your file using code like this: |#
(let ((stream (open "/some/file/name.txt")))
  ;; do stuff with stream
  (close stream))
#| However, this approach suffers from two problems. One is simply that it's 
error prone--if you forget the CLOSE, the code will leak a file handle every 
time it runs. The other--and more significant--problem is that there's no 
guarantee you'll get to the CLOSE. For instance, if the code prior to the CLOSE 
contains a RETURN or RETURN-FROM, you could leave the LET without closing the 
stream. Or, if any of the code before the CLOSE signals an error, control may 
jump out of the LET to an error handler and never come back to close the 
stream. Common Lisp provides a general solution to the problem of how to ensure 
that certain code always runs: the special operator UNWIND-PROTECT. |#

; = with-open-file =
#| WITH-OPEN-FILE, built on top of UNWIND-PROTECT, to encapsulate the pattern of
opening a file, doing something with the resulting stream, and then closing the 
stream |#
(with-open-file (stream-var open-argument*)
  body-form*)
#| The forms in body-forms are evaluated with stream-var bound to a file stream 
opened by a call to OPEN with open-arguments as its arguments. WITH-OPEN-FILE 
then ensures the stream in stream-var is closed before the WITH-OPEN-FILE form 
returns. Thus, you can write this to read a line from a file: |#
(with-open-file (stream "/some/file/name.txt")
  (format t "~a~%" (read-line stream)))
; To create a new file, you can write something like this:
(with-open-file (stream "/some/file/name.txt" :direction :output)
  (format stream "Some text."))

#| The only time you need to use raw OPEN and CLOSE calls is if you need to open
a file in a function and keep the stream around after the function returns. In 
that case, you must take care to eventually close the stream yourself, or you'll
leak file descriptors and may eventually end up unable to open any more files.|#


;; == Filenames ==
#| Using strings as filenames ties your code to a particular operating system 
and file system. Likewise, if you programmatically construct names according to 
the rules of a particular naming scheme (separating directories with /, say), 
you also tie your code to a particular file system. 

To avoid this kind of nonportability, Common Lisp provides another 
representation of filenames: pathname objects. Pathnames represent filenames in 
a structured way that makes them easy to manipulate without tying them to a 
particular filename syntax. And the burden of translating back and forth between
strings in the local syntax--called namestrings--and pathnames is placed on the 
Lisp implementation.

Unfortunately, the pathname abstraction introduces its own complications. When 
pathnames were designed, the set of file systems in general use was quite a bit 
more variegated than those in common use today. Consequently, some nooks and 
crannies of the pathname abstraction make little sense if all you're concerned 
about is representing Unix or Windows filenames. However, once you understand 
which parts of the pathname abstraction you can ignore as artifacts of 
pathnames' evolutionary history, they do provide a convenient way to manipulate 
filenames.

Together namestrings, pathnames, and streams returned by OPEN are collectively 
referred to as pathname designators. All the built-in functions that expect a 
filename argument accept all three types of pathname designator. For instance, 
all the places in the previous section where you used a string to represent a 
filename, you could also have passed a pathname object or a stream. |#

; = How Pathnames Represent Filenames =
#| A pathname is a structured object that represents a filename using six 
components: host, device, directory, name, type, and version. Most of these 
components take on atomic values, usually strings; only the directory component 
is further structured, containing a list of directory names (as strings) 
prefaced with the keyword :absolute or :relative. However, not all pathname 
components are needed on all platforms. |#

; - pathname -
#| translate a namestring to a pathname. It takes a pathname designator and 
returns an equivalent pathname object. When the designator is already a 
pathname, it's simply returned. When it's a stream, the original filename is 
extracted and returned. When the designator is a namestring, however, it's 
parsed according to the local filename syntax. The language standard, as a 
platform-neutral document, doesn't specify any particular mapping from 
namestring to pathname, but most implementations follow the same conventions on 
a given operating system. 

On Unix file systems, only the directory, name, and type components are 
typically used. On Windows, one more component--usually the device or host--
holds the drive letter. On these platforms, a namestring is parsed by first 
splitting it into elements on the path separator--a slash on Unix and a slash or
backslash on Windows. The drive letter on Windows will be placed into either the
device or the host component. All but the last of the other name elements are 
placed in a list starting with :absolute or :relative depending on whether the 
name (ignoring the drive letter, if any) began with a path separator. This list 
becomes the directory component of the pathname. The last element is then split 
on the rightmost dot, if any, and the two parts put into the name and type 
components of the pathname. |#

; - pathname-directory, pathname-name, & pathname-type -
(pathname-directory (pathname "/foo/bar/baz.txt")) #|>|# (:ABSOLUTE "foo" "bar")
(pathname-name (pathname "/foo/bar/baz.txt"))      #|>|# "baz"
(pathname-type (pathname "/foo/bar/baz.txt"))      #|>|# "txt"

; - pathname-host, pathname-device, and pathname-version - 
#| access the other three pathname components, though they're unlikely to have 
interesting values on Unix. On Windows either PATHNAME-HOST or PATHNAME-DEVICE 
will return the drive letter. |#  

#| Pathnames have their own read syntax, #p followed by a double-quoted string. 
This allows you to print and read back s-expressions containing pathname 
objects, but because the syntax depends on the namestring parsing algorithm, 
such data isn't necessarily portable between operating systems. |#
(pathname "/foo/bar/baz.txt") #|-->|# #p"/foo/bar/baz.txt"


; - namestring, directory-namestring, & file-namestring -
#| NAMESTRING, which takes a pathname designator and returns a namestring. Two 
other functions, DIRECTORY-NAMESTRING and FILE-NAMESTRING, return a partial 
namestring. DIRECTORY-NAMESTRING combines the elements of the directory 
component into a local directory name, and FILE-NAMESTRING combines the name and
type components. |#
(namestring #p"/foo/bar/baz.txt")           #|-->|# "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") #|-->|# "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")      #|-->|#  "baz.txt"

; = Constructing New Pathnames =
; - make-pathname -
#| construct arbitrary pathnames. It takes one keyword argument for each 
pathname component and returns a pathname with any supplied components filled in
and the rest NIL. |#
make-pathname
  :directory '(:absolute "foo" "bar")
  :name "baz"
  :type "txt") ; --> 
#p"/foo/bar/baz.txt"

#| However, if you want your programs to be portable, you probably don't want to
make pathnames completely from scratch: even though the pathname abstraction 
protects you from unportable filename syntax, filenames can be unportable in 
other ways. For instance, the filename /home/peter/foo.txt is no good on an OS X
box where /home/ is called /Users/ 

If you write code like this for Lisp for Windows: |#
(make-pathname :device "c" :directory '(:absolute "foo" "bar") :name "baz")
; it will be correct on some implementations but not on others.

#| Rather than making names from scratch, you can build a new pathname based on 
an existing pathname with MAKE-PATHNAME's keyword parameter :defaults. With this
parameter you can provide a pathname designator, which will supply the values 
for any components not specified by other arguments. For example, the following 
expression creates a pathname with an .html extension and all other components 
the same as the pathname in the variable input-file: |#
(make-pathname :type "html" :defaults input-file)
#| Assuming the value in input-file was a user-provided name, this code will be 
robust in the face of operating system and implementation differences such as 
whether filenames have drive letters in them and where they're stored in a 
pathname if they do. 

You can use the same technique to create a pathname with a different directory 
component. |#
(make-pathname :directory '(:relative "backups") :defaults input-file)

#| Sometimes, though, you want to combine two pathnames, at least one of which 
has a relative directory component, by combining their directory components. For
instance, suppose you have a relative pathname such as #p"foo/bar.html" that you
want to combine with an absolute pathname such as #p"/www/html/" to get 
#p"/www/html/foo/bar.html". In that case, MAKE-PATHNAME won't do; instead, you 
want MERGE-PATHNAMES. |#

; - merge-pathnames -
#| MERGE-PATHNAMES takes two pathnames and merges them, filling in any NIL 
components in the first pathname with the corresponding value from the second 
pathname, much like MAKE-PATHNAME fills in any unspecified components with 
components from the :defaults argument. However, MERGE-PATHNAMES treats the 
directory component specially: if the first pathname's directory is relative, 
the directory component of the resulting pathname will be the first pathname's 
directory relative to the second pathname's directory. Thus: |#
(merge-pathnames #p"foo/bar.html" #p"/www/html/") ; --> 
#p"/www/html/foo/bar.html"
#| The second pathname can also be relative, in which case the resulting 
pathname will also be relative. |#
(merge-pathnames #p"foo/bar.html" #p"html/") #|-->|# #p"html/foo/bar.html"

; - enough-namestring -
; obtain a filename relative to a particular root directory
(enough-namestring #p"/www/html/foo/bar.html" #p"/www/") ; --> 
"html/foo/bar.html"

#| You can then combine ENOUGH-NAMESTRING with MERGE-PATHNAMES to create a 
pathname representing the same name but in a different root. |#
(merge-pathnames
  (enough-namestring #p"/www/html/foo/bar/baz.html" #p"/www/")
  #p"/www-backups/") ; --> 
#p"/www-backups/html/foo/bar/baz.html"


; = Interacting with the File System =
; - probe-file -
#| test whether a file exists in the file system corresponding to a pathname 
designator--a pathname, namestring, or file stream. If the file named by the 
pathname designator exists, PROBE-FILE returns the file's truename, a pathname 
with any file system-level translations such as resolving symbolic links 
performed. Otherwise, it returns NIL. However, not all implementations support 
using this function to test whether a directory exists. Also, Common Lisp 
doesn't provide a portable way to test whether a given file that exists is a 
regular file or a directory. |#

; - directory -
#| standard function for listing files in the file system. works fine for simple
cases, but the differences between implementations make it tricky to use 
portably. |#

; - delete-file -
#| takes a pathname designator and deletes the named file, returning true if it 
succeeds. Otherwise it signals a FILE-ERROR. |#

; - rename-file -
#| takes two pathname designators and renames the file named by the first name 
to the second name.  |#

; - ensure-directories-exist -
#| takes a pathname designator and ensures that all the elements of the 
directory component exist and are directories, creating them as necessary. It 
returns the pathname it was passed, which makes it convenient to use inline. |#
(with-open-file (out (ensure-directories-exist name) :direction :output)
   ...
   )

; - file-write-date & file-author -
#| FILE-WRITE-DATE and FILE-AUTHOR both take a pathname designator. 
FILE-WRITE-DATE returns the time in number of seconds since midnight January 1, 
1900, Greenwich mean time (GMT), that the file was last written, and FILE-AUTHOR
returns, on Unix and Windows, the file owner. |#

; - file-length -
#| For historical reasons FILE-LENGTH takes a stream as an argument rather than 
a pathname. In theory this allows FILE-LENGTH to return the length in terms of 
the element type of the stream. For predictable results, the best way to get the
length of a file is to use a binary stream. |#
(with-open-file (in filename :element-type '(unsigned-byte 8))
  (file-length in))

; - file-position -
#| When called with just a stream, this function returns the current position in
the file--the number of elements that have been read from or written to the 
stream. When called with two arguments, the stream and a position designator, it
sets the position of the stream to the designated position. The position 
designator must be the keyword :start, the keyword :end, or a non-negative 
integer. The two keywords set the position of the stream to the start or end of 
the file while an integer moves to the indicated position in the file. With a 
binary stream the position is simply a byte offset into the file. However, for 
character streams things are a bit more complicated because of character-
encoding issues. Your best bet, if you need to jump around within a file of 
textual data, is to only ever pass, as a second argument to the two-argument 
version of FILE-POSITION, a value previously returned by the one-argument 
version of FILE-POSITION with the same stream argument.  |#

; == Other Kinds of I/O ==
#| In addition to file streams, Common Lisp supports other kinds of streams, 
which can also be used with the various reading, writing, and printing I/O 
functions. 

Add information at a later date...
URL: http://www.gigamonkeys.com/book/files-and-file-io.html|#