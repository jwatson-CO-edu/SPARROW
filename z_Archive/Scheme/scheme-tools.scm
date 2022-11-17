#| scheme-tools.scm
James Watson, 2014 December
A collection of useful little things 

  == LOG ==
2014-12-07: Copied 'displayln' from "little-schemer-01.scm"
|#

(define displayln
  (lambda (exp) (display exp) (newline)))