#|
macro-tricks.lisp
James Watson, 2013 July
Experiments with macros, symbols, and quotes
|#

; URL: http://cl-cookbook.sourceforge.net/strings.html#symbols
(defvar foo (intern (string-upcase "plus-three"))) ; create symbol 'PLUS-THREE
(eval `(defun ,foo (num) (+ num 3))) ; create function PLUS-THREE, adds 3 to num

(defun symbol-prefix (sym-list prefix)
  "Return list of symbols that is SYM-LIST with the string PREFIX on each item"
  (let ((rtn-syms nil))
    (dolist (item sym-list)
      (push (intern (string-upcase (concatenate 'string
						prefix
						(string item))))
	    rtn-syms))
    rtn-syms))

; http://stackoverflow.com/questions/5661875/why-is-the-sign-needed-in-this-macro-definition
; http://stackoverflow.com/questions/17429521/common-lisp-double-backquote-unquote-quote-unquote-sequence

#|(defmacro generate-accessor (key-symbol prefix)
    `(defmacro ,(intern (string-upcase (concatenate 'string
						    prefix "-"
						    key-symbol))) 
	 (alis) `(assoc ',',key-symbol ,alis)))|#

;(defvar protolist (pairlis (list 'a 'b 'c) (list 1 2 3)))
;(defvar sym 'a)
;(generate-accessor 'a "alist")

#| 2013-07-26: Given up on trying to automatically generate macro names due to
http://stackoverflow.com/questions/17871121/common-lisp-passing-symbol-to-macro
have not found a good way to do this, since macros do not evaluate their 
arguments, thus one cannot pass a macro name to a macro in an argument
|#