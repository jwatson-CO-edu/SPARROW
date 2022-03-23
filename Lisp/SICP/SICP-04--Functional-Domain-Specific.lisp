#|
SICP-04--Functional-Domain-Specific.lisp
James Watson, 2012 March

Structure and Interpretation of Computer Programs
By Harold Abelson and Gerald Jay Sussman with Julie Sussman  
http://www-mitpress.mit.edu/sicp/

Functional programming and the creation of domain-specific languages
|#

;; == Example: A Picture Language ==
#| This section presents a simple language for drawing pictures that illustrates
the power of data abstraction and closure, and also exploits higher-order 
procedures in an essential way.Part of the elegance of this picture language is 
that there is only one kind of element, called a painter. A painter draws an 
image that is shifted and scaled to fit within a designated parallelogram-shaped
frame. A frame can be described by three vectors -- an origin vector and two 
edge vectors. The origin vector specifies the offset of the frame's origin from 
some absolute origin in the plane, and the edge vectors specify the offsets of 
the frame's corners from its origin. If the edges are perpendicular, the frame 
will be rectangular. Otherwise the frame will be a more general parallelogram. A
frame is created with a constructor make-frame, which takes three vectors and 
produces a frame, and three corresponding selectors origin-frame, edge1-frame, 
and edge2-frame. We will use coordinates in the unit square to specify images. 
With each frame, we associate a frame coordinate map, which will be used to 
shift and scale images to fit the frame. The map transforms the unit square into
the frame by mapping the vector v = (x,y) to the vector sum
Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame) 

In order to create and operate on such vectors, the following functions have 
been provided by Eli Bendersky
URL: http://eli.thegreenplace.net/2007/08/24/sicp-section-224/
and then converted from PLT Scheme (Racket). |#

(defun make-vect(x y)
  "Create a 2-dimensional painter vector represented as a cons"
  (cons x y))

(defun xcor-vect(v)
  "Return the x coordinate of a painter vector"
  (car v))

(defun ycor-vect(v)
  "Return the y coordinate of a painter vector"
  (cdr v))

(defun add-vect(v1 v2)
  "Return the sum of two painter vectors"
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(defun sub-vect(v1 v2)
  "Return the difference of two painter vectors"
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(defun scale-vect(s v)
  "Return vector in which vector V has been scaled by scalar S"
  (make-vect
   ; The bitmap library used requires that coords be expresses as integers
   (floor (* s (xcor-vect v)))   ; calc scaled x and round down
   (floor (* s (ycor-vect v))))) ; calc scaled y and round down

(defun make-frame(origin edge1 edge2)
  "Create a frame as defined in SICP Section 2.2.4"
  (list origin edge1 edge2))

(defun origin-frame(f)
  "Access the origin vector of a SICP frame"
  (car f))

(defun edge1-frame(f)
  "Access the Edge1 vector of a SICP frame"
  (cadr f))

(defun edge2-frame(f)
  "Access the Edge2 vector of a SICP frame"
  (caddr f))

(defun make-segment(v-start v-end)
  "Create a representation of a line segment as a cons of two painter vectors"
  (cons v-start v-end))

(defun start-segment(seg)
  "Access the start point vector of a line segment"
  (car seg))

(defun end-segment(seg)
  "Access the end point vector of a line segment"
  (cdr seg))

#| We will use coordinates in the unit square (0 <= x, y <= 1) to specify 
images. With each frame, we associate a frame coordinate map, which will be used
to shift and scale images to fit the frame. The map transforms the unit square 
into the frame by mapping the vector v = (x,y) to the vector sum
Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame)

We can create a frame's coordinate map with the following procedure: |#
(defun frame-coord-map(frame)
  "Return a function that will map a vector V to FRAME"
  ; #' is short for FUNCTION, letting interpreter know that this belongs in 
  ;function namespace
  #'(lambda (v) ; Returned function will take a vector as an arg
      (add-vect ; Add the passed frame origin to the vectors in a new basis
       (origin-frame frame) 
       ; Construct a vector, V oriented along the basis vectors of the frame
       (add-vect (scale-vect (xcor-vect v) ;v's x coord along the frame's x vec
			     (edge1-frame frame))
		 (scale-vect (ycor-vect v) ;v's y coord along the frame's y vec
			     (edge2-frame frame))))))


#| Assume a function DRAW-SEGMENT that will use whatever line-drawing facility 
that is available in order to display output. |#
(defun draw-segment(segment)
  "Draw a segment to the *buffer*"
  (px-line *buffer* (car segment) (cdr segment))) 
; PX-LINE is a function exported by the PX-PAINT package


#| A painter is represented as a procedure that, given a frame as argument, 
draws a particular image shifted and scaled to fit the frame. That is to say, if
p is a painter and f is a frame, then we produce p's image in f by calling p 
with f as argument. |#

(defun segments->painter(segment-list) ; painter assigned segments when created
  "Return procedure that, given a frame, draws segments shifted/scaled to frame"
  #'(lambda (frame) ; returned function takes a frame as an arg
      (dolist (segment segment-list) ; for each SEGMENT in SEGMENTS-LIST
	; Draw a segment that - 
	(draw-segment (make-segment ; is oriented along FRAME
		       (funcall (frame-coord-map frame) 
				(start-segment segment))
		       #| FUNCALL signals to the interpreter that the result
		       of the first form will be a function, and then invokes
		       it, with each form after the first as arguments.|#
		       (funcall (frame-coord-map frame) 
				(end-segment segment)))))))
                       #| A form must start with a function name, not another
                       form, making the above calls to FUNCALL necessary. |#

; For the purposes of illustration the following diagram is available
(defparameter krest-segments ; Segments for the Eli Bendersky design
  ; http://eli.thegreenplace.net/2007/08/24/sicp-section-224/
  ; Coordinates expressed as fractions of the frame vector amplitudes
  (list
    (make-segment
      (make-vect 0.4 0.2)
      (make-vect 0.6 0.2))
    (make-segment
      (make-vect 0.5 0.0)
      (make-vect 0.5 1.0))
    (make-segment
      (make-vect 0.3 1.0)
      (make-vect 0.7 0.7))
    (make-segment
      (make-vect 0.0 0.4)
      (make-vect 1.0 0.4))
    (make-segment
      (make-vect 0.2 0.4)
      (make-vect 0.2 0.6))
    (make-segment
      (make-vect 1.0 0.4)
      (make-vect 1.0 0.6))
    (make-segment
      (make-vect 0.0 0.6)
      (make-vect 0.2 0.6))))

#| By changing the basis vectors for the frames, we can see how the original
figure is transformed. |#
(defun quad-and-flipped()
  "Testing application of a painter to different frames"
  (init-buffer 400 400)
  ; Note that frame side vectors are expressed relative to the origin of frame
  ; UL: Normal
  (funcall (segments->painter krest-segments) (make-frame (cons 0 0)
							  (cons 199 0)
							  (cons 0 199)))
  ; UR: Flipped horizontal
  (funcall (segments->painter krest-segments) (make-frame (cons 399 0)
							  (cons -199 0)
							  (cons 0 199)))
  ; LL: Flipped vertical
  (funcall (segments->painter krest-segments) (make-frame (cons 0 399)
							  (cons 199 0)
							  (cons 0 -199)))
  ; LR: Flipped horizontal, vertical
  (funcall (segments->painter krest-segments) (make-frame (cons 399 399)
							  (cons -199 0)
							  (cons 0 -199)))
  (rgb-buffer->ppm "quad-and-flipped.ppm" *buffer*))

#| The segments are given using coordinates with respect to the unit square. For
each segment in the list, the painter transforms the segment endpoints with the 
frame coordinate map and draws a line between the transformed points.

Representing painters as procedures erects a powerful abstraction barrier in the
picture language. We can create and intermix all sorts of primitive painters, 
based on a variety of graphics capabilities. The details of their implementation
do not matter. Any procedure can serve as a painter, provided that it takes a 
frame as argument and draws something scaled to fit the frame. 

 = Transforming Painters =

An operation on painters (such as those below) works by creating a painter that
invokes the original painters with respect to frames derived from the argument 
frame. Thus, for example, flip-vert doesn't have to know how a painter works in 
order to flip it -- it just has to know how to turn a frame upside down: The 
flipped painter just uses the original painter, but in the inverted frame.

Painter operations are based on the procedure TRANSFORM-PAINTER, which takes as 
arguments a painter and information on how to transform a frame and produces a 
new painter. The transformed painter, when called on a frame, transforms the 
frame and calls the original painter on the transformed frame. The arguments to 
transform-painter are points (represented as vectors) that specify the corners 
of the new frame: When mapped into the frame, the first point specifies the new 
frame's origin and the other two specify the ends of its edge vectors. Thus, 
arguments within the unit square specify a frame contained within the original 
frame. |#

(defun transform-painter(painter origin corner1 corner2)
  "Returns a new painter based on a given painter and frame vectors"
  ; Returns a function that transforms the frame before passing it to the
  ;original painter
  #'(lambda (frame) ; Returned function takes a frame to transform to as an arg
      ;Create mapping function, assign to m
      (let ((m (frame-coord-map frame)))
	(let ((new-origin (funcall m origin))) ; Transform origing vector with m
	  (funcall painter
	   (make-frame new-origin ; create a new frame based on the passed args
		       (sub-vect (funcall m corner1) new-origin)
		       (sub-vect (funcall m corner2) new-origin)))))))

(defun rotate90(painter)
  "Return a version of PAINTER that will paint segments 90deg from original"
  ;(funcall #'transform-painter painter
  (transform-painter painter
	   (make-vect 0.0 1.0)     ; new origin
	   (make-vect 0.0 0.0)     ; new end of edge1
	   (make-vect 1.0 1.0)))   ; new end of edge2

(defun rotate180(painter)
  "Return a version of PAINTER that will paint segments 180deg from original"
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(defun rotate270(painter)
  "Return a version of PAINTER that will paint segments 270deg from original"
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(defun flip-horiz(painter)
  "Returned function invokes PAINTER on a frame flipped about the 2nd axis"
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

#| Frame transformation is also the key to defining means of combining two or 
more painters. The beside procedure, for example, takes two painters, transforms
them to paint in the left and right halves of an argument frame respectively, 
and produces a new, compound painter. When the compound painter is given a 
frame, it calls the first transformed painter to paint in the left half of the 
frame and calls the second transformed painter to paint in the right half of the
frame: |#

(defun beside(painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    #| LABELS and its relatives FLET/FLET* are used to define lexical functions
    for use within a block. They are of use for local functions or instances
    when code will be repeated in one region, and are not needed at the top 
    level. |#
    (labels ((paint-left(fr)
	       (funcall (transform-painter painter1
					   (make-vect 0.0 0.0)
					   split-point
					   (make-vect 0.0 1.0)) fr))
	     #| Needed to make sure that the function returned by 
	     TRANSFORM-PAINTER is actually called with FUNCALL and passed FR 
	     when the labels PAINT-UP and PAINT-DOWN were invoked. |#
	     (paint-right(fr)
	       (funcall (transform-painter painter2
					   split-point
					   (make-vect 1.0 0.0)
					   (make-vect 0.5 1.0)) fr)))
      #'(lambda (frame)                                       ; ^-argument
	  (paint-left frame)
	  (paint-right frame)))))

#| The beside procedure need not know anything about the details of the 
component painters other than that each painter will draw something in its 
designated frame. |#

(defun below(painter1 painter2)
  "Divides frame along 2nd axis and invokes a painter on each region"
  (let ((split-point (make-vect 0.0 0.5)))
    (labels ((paint-up(fr)
	     (funcall (transform-painter painter2
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 0.0)
					 split-point) fr))
	   (paint-down(fr)
	     (funcall (transform-painter painter1
					 split-point
					 (make-vect 1.0 0.5)
					 (make-vect 0.0 1.0)) fr)))
    #'(lambda(frame) ; Returned value is a function, belongs in function space
	(paint-up frame)
	(paint-down frame)))))

(defun below-rot(painter1 painter2)
  (rotate90 (below
              (rotate90 painter1)
              (rotate270 painter2))))

(defun up-split(painter n)
  (if (zerop n)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(defun right-split(painter n)
   (if (zerop n)
       painter
       (let ((smaller (right-split painter (- n 1))))
         (beside painter (below smaller smaller)))))

(defun corner-split(painter n)
  (if (zerop n)
    painter
    (let* ( (up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (top-left up)
            (bottom-right right)
            (corner (corner-split painter (- n 1))))
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defun flip-vert(painter)
   (transform-painter painter
                      (make-vect 0.0 1.0)   ; new origin
                      (make-vect 1.0 1.0)   ; new end of edge1
                      (make-vect 0.0 0.0))) ; new end of edge2

(defun square-limit(painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(defun squash-inwards(painter)
   (transform-painter painter
                      (make-vect 0.0 0.0)
                      (make-vect 0.65 0.35)
                      (make-vect 0.35 0.65)))

#| The fundamental data abstractions, painters, are implemented using procedural
representations, which enables the language to handle different basic drawing 
capabilities in a uniform way, which permits us to easily build up complex 
designs. Finally, all the tools for abstracting procedures are available to us 
for abstracting means of combination for painters.

We have also obtained a glimpse of another crucial idea about languages and 
program design. This is the approach of stratified design, the notion that a 
complex system should be structured as a sequence of levels that are described 
using a sequence of languages. Each level is constructed by combining parts that
are regarded as primitive at that level, and the parts constructed at each level
are used as primitives at the next level. The language used at each level of a 
stratified design has primitives, means of combination, and means of abstraction
appropriate to that level of detail. 

Stratified design helps make programs robust, that is, it makes it likely that 
small changes in a specification will require correspondingly small changes in 
the program. In general, each level of a stratified design provides a different 
vocabulary for expressing the characteristics of the system, and a different 
kind of ability to change it. |#


; = Demo Functions =
#| The following functions are used to demonstrate the above so that the results
can be viewed. |#

(defun test-painter-1()
  "Test that SEGMENTS->PAINTER returns a good function"
  (init-buffer 400 400)
  (funcall (segments->painter krest-segments) (make-frame (cons 0 0)
							  (cons 399 0)
							  (cons 0 399)))
  (rgb-buffer->ppm (get-file-name) *buffer*))

(defun standard-demo-frame()
  "Frame fit to a 400x400px bitmap"
  (make-frame (cons 0 0)
	      (cons 399 0)
	      (cons 0 399)))

(defun krest-rot-90()
  "Tests ROTATE90 with the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (rotate90 (segments->painter krest-segments)) 
	   (make-frame (cons 0 0)
		       (cons 399 0)
		       (cons 0 399)))
  (rgb-buffer->ppm "krest-rot-90.ppm" *buffer*))

(defun krest-rot-180()
  "Tests ROTATE180 with the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (rotate180 (segments->painter krest-segments)) 
	   (make-frame (cons 0 0)
		       (cons 399 0)
		       (cons 0 399)))
  (rgb-buffer->ppm "krest-rot-180.ppm" *buffer*))

(defun krest-rot-270()
  "Tests ROTATE270 with the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (rotate270 (segments->painter krest-segments)) 
	   (make-frame (cons 0 0)
		       (cons 399 0)
		       (cons 0 399)))
  (rgb-buffer->ppm "krest-rot-270.ppm" *buffer*))

(defun krest-flip-h()
  "Tests FLIP-HORIZ on the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (flip-horiz (segments->painter krest-segments)) 
	   (make-frame (cons 0 0)
		       (cons 399 0)
		       (cons 0 399)))
  (rgb-buffer->ppm "krest-flip-h.ppm" *buffer*))

(defun krest-below()
  "Tests BELOW on the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (below (segments->painter krest-segments)
		  (segments->painter krest-segments))
	   (make-frame (cons 0 0)
		       (cons 399 0)
		       (cons 0 399)))
  (rgb-buffer->ppm "krest-below.ppm" *buffer*))

(defun krest-below-rot()
  "Tests BELOW-ROT on the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (below-rot (segments->painter krest-segments)
		      (segments->painter krest-segments))
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-below-rot.ppm" *buffer*))

(defun krest-beside()
  "Tests BESIDE on the KREST-SEGMENTS"
  (init-buffer 400 400)
  (funcall (beside (segments->painter krest-segments)
		   (segments->painter krest-segments))
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-beside.ppm" *buffer*))

(defun krest-right-split()
  (init-buffer 400 400)
  (funcall (right-split (segments->painter krest-segments) 4)
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-right-split.ppm" *buffer*))

(defun krest-up-split()
  (init-buffer 400 400)
  (funcall (up-split (segments->painter krest-segments) 4)
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-up-split.ppm" *buffer*))

(defun krest-corner-split()
  (init-buffer 400 400)
  (funcall (corner-split (segments->painter krest-segments) 4)
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-corner-split.ppm" *buffer*))

(defun krest-square-limit()
  (init-buffer 400 400)
  (funcall (square-limit (segments->painter krest-segments) 4)
	   (standard-demo-frame))
  (rgb-buffer->ppm "krest-square-limit.ppm" *buffer*))