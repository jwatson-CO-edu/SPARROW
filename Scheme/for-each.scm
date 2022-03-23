#| for-each.scm
James Watson 2014 December
Learning about the 'for-each' procedure. |#

#| (for-each FUNCFORITEM THELIST) 
'for-each' takes two arguments: 
    FUNCFORITEM : A function that takes one argument, the list item. 'for-each' will call this
                  function on each list item in turn.
    THELIST     : A list of items to be individually processed by FUNCFORITEM |#
(for-each
  (lambda (i) (display i) (newline))
  '(frogs bats snakes)) ; prints:
#|
frogs
bats
snakes
|#

;The Lispy way is to process a list recursively, but here is 'for-each' if you just don't wanna