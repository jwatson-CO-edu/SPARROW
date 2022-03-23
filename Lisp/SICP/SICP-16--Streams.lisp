#|
SICP-16--Streams.lisp
James Watson, 2012 May

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Working with data objects whose contents arrive a piece at a time
|#

#| Streams can mitigate some of the complexity of modeling state. 

If time is measured in discrete steps, then we can model a time function as a 
 (possibly infinite) sequence. We will see how to model change in terms of 
sequences that represent the time histories of the systems being modeled. To 
accomplish this, we introduce new data structures called streams. From an 
abstract point of view, a stream is simply a sequence. However, we will find 
that the straightforward implementation of streams as lists doesn't fully reveal
the power of stream processing. 

Stream processing lets us model systems that have state without ever using 
assignment or mutable data. |#


; = Streams are Delayed Lists =

#| Unfortunately, if we represent sequences as lists, simplicity is bought at 
the price of severe inefficiency with respect to both the time and space 
required by our computations. When we represent manipulations on sequences as 
transformations of lists, our programs must construct and copy data structures 
 (which may be huge) at every step of a process. 

With streams we can achieve the best of both worlds: We can formulate programs 
elegantly as sequence manipulations, while attaining the efficiency of 
incremental computation. The basic idea is to arrange to construct a stream only
partially, and to pass the partial construction to the program that consumes the
stream. If the consumer attempts to access a part of the stream that has not yet
been constructed, the stream will automatically construct just enough more of 
itself to produce the required part. |#

#| There is a distinguishable object, the-empty-stream, which cannot be the 
result of any cons-stream operation, and which can be identified with the 
predicate stream-null?. |#
(defvar the-empty-stream '())

(defun stream-null? (s)
  (null s))

(defun stream-car (s)
  (car s))

#| To make the stream implementation automatically and transparently interleave 
the construction of a stream with its use, we will arrange for the cdr of a 
stream to be evaluated when it is accessed by the stream-cdr procedure rather 
than when the stream is constructed by cons-stream. |#

(defun stream-cdr (s)
  (force (cdr s)))

#| As a data abstraction, streams are the same as lists. The difference is the 
time at which the elements are evaluated. With ordinary lists, both the car and 
the cdr are evaluated at construction time. With streams, the cdr is evaluated 
at selection time. What this means is that we will construct streams using 
pairs. However, rather than placing the value of the rest of the stream into the
cdr of the pair we will put there a promise to compute the rest if it is ever 
requested. |#

#| Our implementation of streams will be based on a special form called delay. 
Evaluating (delay <exp>) does not evaluate the expression <exp>, but rather 
returns a so-called delayed object, which we can think of as a "promise" to 
evaluate <exp> at some future time. 

Delay must package an expression so that it can be evaluated later on demand, 
and we can accomplish this simply by treating the expression as the body of a 
procedure. |#

(defmacro delay (expr)
  `(memo-proc (lambda () ,expr)))

#| As a companion to delay, there is a procedure called force that takes a 
delayed object as argument and performs the evaluation -- in effect, forcing the
delay to fulfill its promise. 

Force simply calls the procedure (of no arguments) produced by delay, so we can 
implement force as a procedure: |#

(defun force (delayed-object)
  (funcall delayed-object))

#| This implementation suffices for delay and force to work as advertised, but 
there is an important optimization that we can include. In many applications, we
end up forcing the same delayed object many times. This can lead to serious 
inefficiency in recursive programs involving streams. The solution is to build 
delayed objects so that the first time they are forced, they store the value 
that is computed. Subsequent forcings will simply return the stored value 
without repeating the computation. In other words, we implement delay as a 
special-purpose memoized procedure. One way to accomplish this is to use the 
following procedure, which takes as argument a procedure (of no arguments) and 
returns a memoized version of the procedure. The first time the memoized 
procedure is run, it saves the computed result. On subsequent evaluations, it 
simply returns the result. |#

(defun memo-proc (proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
        (progn
          (setf result (funcall proc))
          (setf already-run? t)
          result)
        result))))

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

; Using the above we can implement some of the basic stream operations:

(defun stream-ref (s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(defun stream-enumerate-interval (low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(defun stream-filter (pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((funcall pred (stream-car s))
          (cons-stream
            (stream-car s)
            (stream-filter pred (stream-cdr s))))
        (t (stream-filter pred (stream-cdr s)))))

; = Helper Macro: DEFLEX =
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

; - Usage: Basic Stream Functions 
(deflex ss
  (stream-filter
    #'prime?
    (stream-enumerate-interval 10 100)))

(stream-car ss) #|-->|# 11
(stream-car (stream-cdr ss)) #|-->|# 13
(stream-car (stream-cdr (stream-cdr ss))) #|-->|# 17


#| generalizes stream-map to allow procedures that take multiple arguments, 
analogous to map |#
(defun stream-map (proc &rest argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (mapcar #'stream-car argstreams))
      (apply #'stream-map
        (cons proc (mapcar #'stream-cdr argstreams))))))

; - Usage: New STREAM-MAP -
(deflex s1 (stream-enumerate-interval 10 100))
(deflex s2 (stream-enumerate-interval 20 200))
(deflex s3 (stream-enumerate-interval 30 300))
(deflex ss (stream-map #'+ s1 s2 s3))

(stream-ref ss 0) #|-->|# 60
(stream-ref ss 1) #|-->|# 63
(stream-ref ss 2) #|-->|# 66


; = Infinite Streams =

#| we can use streams to represent sequences that are infinitely long. For 
instance, consider the following definition of the stream of positive 
integers: |#

(defun integers-starting-from (n)
   (cons-stream n (integers-starting-from (+ n 1))))

(deflex integers (integers-starting-from 1))

#| This makes sense because integers will be a pair whose car is 1 and whose cdr
is a promise to produce the integers beginning with 2. This is an infinitely 
long stream, but in any given time we can examine only a finite portion of it. 
Thus, our programs will never know that the entire infinite stream is not 
there. 

we can define the infinite stream of Fibonacci numbers: |#
(defun fibgen (a b)
  (cons-stream a (fibgen b (+ a b))))

(deflex fibs (fibgen 0 1))
#| Fibs is a pair whose car is 0 and whose cdr is a promise to evaluate 
 (fibgen 1 1). When we evaluate this delayed (fibgen 1 1), it will produce a 
pair whose car is 1 and whose cdr is a promise to evaluate (fibgen 1 2), and so 
on. |#

; Next Section: Defining Streams Implicitly