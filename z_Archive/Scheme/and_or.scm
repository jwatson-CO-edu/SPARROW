#|
and_or.scm
James Watson, 2013 December
Demonstrating behavior of 'and' and 'or' in Scheme
|#

(define displayln
  (lambda (exp) (display exp) (newline)))

(displayln (or       )) ; #f
(displayln (or   1  2)) ; 1
(displayln (or  #f  2)) ; 2
(displayln (or   1 #f)) ; 1
(displayln (and      )) ; #t
(displayln (and  1  2)) ; 2
(displayln (and #f  2)) ; #f
(displayln (and  1 #f)) ; #f