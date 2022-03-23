REPL-README.txt
James Watson, 2013 November
Manual, Documentation, and Implementation notes for the REPLacement language

== Introduction ==

REPLacement is a simple implementation of Scheme written in JavaScript that originates directly from "The Little JavaScripter" by Douglas Crockford.
It's purpose is to serve as a fun but usable REPL in the browser or a Windows Hypertext Application.
REPLacement consists of the evaluator code copied directly from "JavaScripter" with some changes.  Features have been added to make the language more 
complete.

== End Intro ==

== Implementation Notes ==

* JS 'null' used to represent the empty list (quote ())
* Strings are used to represent symbols
* Names that begin with "$" are generally internal-use variables and functions
* For the purposes of documentation
  - item: refers to the element of a list under scrutiny for the current iteration (stack frame)
  - sublist: usually refers to the balance of the list after item (e.g. cdr(list)), but can refer to the next level of a nested list generally

== End Implementation ==

== Evaluation Description ==

= value = 
Evaluation begins with the 'value' function, which takes 'e', an s-expression, as its argument.  It calls 'meaning' on the expression 'e' and the 
'$global' context.  '$global' is the lookup table that associates reserved words with JavaScript actions.
'value' is the function that external code should call in order to evaluate REPLacement expressions.

-- List Processing --
Note that 'meaning', 'expressionToAction', 'listToAction', and 'evlis' together form the main engine that recursively transforms a tokenized list into
executable actions.
-- End List --

= meaning =
Takes an s-expression 'e' and 'context' as arguments.  Calls the function returned by 'expressionToAction(e)' with the arguments 'e, context'

= expressionToAction =
Takes an s-expression 'e' as an argument. It attempts to return an appropriate function or value based on the expression 'e'
  - is atom --> return '$identifier'
    ~ boolean or number literal - return expression
    ~ assigned symbol ----------- return result of symbol lookup
    ~ JavaScript name ----------- return result of lookup of JavaScript global namespace
    ~ not recognized ------------ return LS pair (error <unrecognizedName>) // NOTE: This will not halt eval and error pair will be passed to calling code
  - is list --> 'listToAction'
When 'expressionToAction' is evaluating atoms, it does not reach into reserved words.  These are handled by 'listToAction'.

= listToAction =
* Lookup of function name in special forms
  - OR -
* Return a function with the arguments 'e, context' that applies ('apply') the meaning of the first item in the list to the evaluation of the sublist

= evlis =
Takes a list of expressions 'args' and a 'context', and returns a list that is the 'meaning' of each expression

= apply =
Determines whether the function passed is a primitive (JS) function or a user-defined (closure) function, and call corresponding 'applyPrimitive' or
'applyClosure', based on the label-name pair, where the label is 'primitive' or 'nonPrimitive'

= applyPrimitive =
Flatten Lisp list of arguments into a JS array using 'flatten'.  Then use the function object member function 'fun.apply' to call the function stored in
'fun' with the array members as arguments

= Evaluation Examples =

value(s('2'))
	'2' --> 2                         1. Parser converts digit string into a JS number
	meaning(2, $global)               2. 'value' calls 'meaning', passing the expression and the '$global' context
	expressionToAction(2)(2, $global) 3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned 
	                                     function
	$identifier(2, $global)           4. The function returned by 'expressionToAction' was '$identifier', because it was passed an atom
	2                                 5. '$identifier' recognizes the atom as a number, and returns it
	
value(s(' (cons 1 2) '))

	1. Parser converts string to Lisp list, JS implementation
	' (cons 1 2) ' --> ['cons', [1, [2, null]]]
	
	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['cons',[1,[2,null]]], $global)
	
	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction(['cons',[1,[2,null]]])(['cons',[1,[2,null]]], $global)
	
	4. Expression was not atom, call 'listToAction', which does not find special form, so return a function that applies
	return apply(meaning(functionOf(['cons',[1,[2,null]]]), $global), evlis(argumentsOf(['cons',[1,[2,null]]]), $global));
	return apply(meaning(            'cons'               , $global), evlis(                    [1,[2,null]]] , $global));
	
	5. Meaning of "cons" is an atom (string), but not a literal, not a resrved word, is JS func, identify as func and label as primitive func
	return apply(        ['primitive,[cons, null]]                  , evlis(                    [1,[2,null]]] , $global));
	
	6. 'evlis' builds a list of the 'meaning' of each item in arg sublist.  Each item turned out to be a number literal (see above for literals)
	return apply(        ['primitive,[cons, null]]                  ,                           [1,[2,null]]]           );
	
	7. Now we may 'apply', tagged with "primitive" symbol, 'applyPrimitive'
	return                            cons.apply(null,                                 flatten( [1,[2,null]]] ));
	return                            cons.apply(null,                                          [1, 2] ); // funcName.apply takes args as a list
        return [1, 2] // END RESULT

value(s(' ((lambda (x) x) 2) '))

	1. Parser converts string to Lisp list, JS implementation
	' ((lambda (x) x) 2) ' --> [ ['lambda',[['x',null], ['x', null]]],[2,null]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( [['lambda',[['x',null], ['x', null]]],[2,null]], $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction([['lambda',[['x',null], ['x', null]]],[2,null]])([['lambda',[['x',null], ['x', null]]],[2,null]], $global)

	4. Expression was not atom, call 'listToAction', which does not find special form, since there is a function in place of the function name
	return apply(meaning(functionOf([['lambda',[['x',null], ['x', null]]],[2,null]]), $global), 
					evlis(argumentsOf([['lambda',[['x',null], ['x', null]]],[2,null]]), $global));
	return apply(meaning(            ['lambda',[['x',null], ['x', null]]], $global), evlis( [2,null], $global));

	5. Now it is time to find the 'meaning' of the 'functionOf' the expression, which is of course a lambda expression. 
	   It is not an atom, but this time 'listToAction' discovers that it is a special form, 
	   and the lookup returns the following, to which 'meaning' passes the expression and context:
	function (e, context){ return build('nonPrimitive', cons(context, cdr(e))); }
	                       return build('nonPrimitive', cons($global, [['x',null], ['x', null]]));
	                       return      ['nonPrimitive', [   [$global, [['x',null], ['x', null]]], null ] ]

	6. Evaluate the list of arguments, the 'cdr' of the original expression, consists of a list of one item: number literal 2
	   For the interpretation of literals, see above
	cons(meaning(car([2,null]), $global), evlis(cdr([2,null]), $global));
	[2,null]

	7. Having determined 'meaning' of both function and arguments, can now 'apply'.  Encountering the 'nonPrimitive' function tag, 'apply' chooses
	   to 'applyClosure' in order to evaluate a user-defined function created with the "lambda" keyword
	return apply( ['nonPrimitive', [ [$global, [['x',null], ['x', null]]], null ] ] , [2,null] );
	return applyClosure(second(['nonPrimitive', [ [$global, [['x',null], ['x', null]]], null ] ]), [2,null]) 
	return applyClosure(                          [$global, [['x',null], ['x', null]]],            [2,null])

	8. 'applyClosure' decomposes the closure and evaluates it
	return meaning(bodyOf([$global, [['x',null], ['x', null]]]), 
		newContext(formalsOf([$global, [['x',null], ['x', null]]]), [2,null], tableOf([$global, [['x',null], ['x', null]]])));
	return meaning(                               'x'          , 
		newContext(                     ['x',null]                , [2,null],          $global                             ));

	9. 'newContext' uses the 'begetObject' function of the original context (in this case '$global') to create a copy.  It then iterates through the
            list of 'names' and associates each with the corresponding in the list of 'vals' in a new associative array. Now, when determining the 
	    'meaning' of 'x', it becomes a variable lookup in the newly-created context.
	return meaning( 'x', <theNewContext> )
	return expressionToAction('x')( 'x', <theNewContext> )

	10. 'expressionToAction' identifies "x" as an atom, returns function '$identifier', which inspects expression, not finding literal, 
	    attempts lookup. Lookup in the newly-created context succeeds since during the application of the closure it was loaded with 
	    a value of 2 for key 'x'
	lookupInContext( 'x', <theNewContext> )
	<theNewContext>['x']
	2

value(s(' (define foo 2) '))

	1. Parser converts string to Lisp list, JS implementation
	' (define foo 2) ' --> ['define', ['foo', [2, null]]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['define', ['foo', [2, null]]] , $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction(['define', ['foo', [2, null]]])(['define', ['foo', [2, null]]], $global)

	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function (['define', ['foo', [2, null]]] , $global)
		{ return ($global[second(['define', ['foo', [2, null]]])] = meaning(third(['define', ['foo', [2, null]]]), $global)); }
		  return ($global[                   'foo'              ] = meaning(                          2          , $global));
	   See above for the evaluation of a number literal

	5. The key 'foo' has been added to the '$global' associative array with the value 2.  Returning the assignment operation results in the value
	2

value(s(' (cond (#t (quote was-true)) (else (quote was-false))) '))

	1. Parser converts string to Lisp list, JS implementation
	' (cond (#t (quote was-true)) (else (quote was-false))) ' --> 
	['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]] , $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction(['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]])
		(['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]] , $global)
	
	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function (['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]] , $global)
		{ return evcon(condLinesOf(['cond',[['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]]), $global); }
		  return evcon(                    [['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]]  , $global);
		  
	5. 'evcon' will now evaluate the sublist of conditions expression-by-expression
	isElse(questionOf(car([['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]])))
	isElse(questionOf(     ['#t',[['quote',['was-true',null]],null]]                                                      ))
	isElse(                 '#t' ) // This was not an 'else' expression, therefore determine if conditional is true
	meaning(questionOf(car([['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]])), $global) ? 
		meaning(answerOf(car([['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]])), $global)
	meaning(                '#t'                                                                                              , $global) ? 
		meaning(answerOf(car([['#t',[['quote',['was-true',null]],null]],[['else',[['quote',['was-false',null]],null]],null]])), $global)
	// conditional evaluates true as a result of the '$global' lookup, therefore find 'meaning' of consequent
		meaning(answerOf(     ['#t',[['quote',['was-true',null]],null]]                                                       , $global)
		meaning(                     ['quote',['was-true',null]]         , $global)
	// 'meaning' calls 'expressionToAction', calls 'listToAction', lookup in '$specialform' succeeds
		return textOf(['quote',['was-true',null]])
		return                  'was-true'
	// Therefore the result of 'evcon' returned by the function returned by 'listToAction'
	'was-true'

value(s(' (and  1  2) '))

	1. Parser converts string to Lisp list, JS implementation
	' (and  1  2) ' --> ['and',[1,[2,null]]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['and',[1,[2,null]]] , $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( ['and',[1,[2,null]]] )( ['and',[1,[2,null]]] , $global )

	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function ( ['and',[1,[2,null]]] , $global ){ return isNull(cdr(['and',[1,[2,null]]])) ? true: $and(cdr(['and',[1,[2,null]]]), $global); }

	5. Returned function tests for null-arguments case, fails. Calls '$and' on sublist. 
	$and( [1,[2,null]] )

	6. The test expressions (1 2) are evaluated from left to right, and the value of the first expression that evaluates to a false value is 
	returned. Any remaining expressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is 
	returned.
	// no false-like values found, therefore the value of the last expression is returned
	2

value(s(' (or 1 2) '))

	1. Parser converts string to Lisp list, JS implementation
	' (or  1  2) ' --> ['or',[1,[2,null]]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['or',[1,[2,null]]] , $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( ['or',[1,[2,null]]] )( ['or',[1,[2,null]]] , $global )
	
	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function ( ['or',[1,[2,null]]] , $global ){ return isNull(cdr(['or',[1,[2,null]]])) ? false: $or(cdr(['or',[1,[2,null]]]), $global); }

	5. Returned function tests for null-arguments case, fails. Calls '$or' on sublist. 
	$or( [1,[2,null]] )

	6. The test expressions are evaluated from left to right, and the value of the first expression that evaluates to a true value is returned. Any 
	remaining expressions are not evaluated. If all expressions evaluate to false values, the value of the last expression is returned.
	// the first expression is a truth-like expression, therefore the value of the first expression is returned
	1

value(s(' (quote horse) '))

	1. Parser converts string to Lisp list, JS implementation
	' (quote horse) ' --> ['quote',['horse',null]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['quote',['horse',null]] , $global)

	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( ['quote',['horse',null]] )( ['quote',['horse',null]] , $global )

	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function ( ['quote',['horse',null]] , $global ){ return textOf( ['quote',['horse',null]] ); }

	5. 'textOf' is an alias for 'second', retrieve the contents of the quoted expression
	'horse'

- Recursion Understanding -
	
value(s(' (define fact (lambda (n) (cond ((zero? n) 1) (else (* n (fact (- n 1))))))) '))

	1. Parser converts string to Lisp list, JS implementation
	['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]]

	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( 
		['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]] , 
		$global
	)
	
	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( 
		['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]]
	)( 
		['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]] ,
		$global 
	)
	
	4. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform' for "define", succeeds. Returns function:
	function ( ['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]] , $global)
		{ return (
			$global[second(
				['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]]
			)]
			= 
			meaning(
				third(
					['define',['fact',[['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]],null]]]
				), 
				$global
			)
		); }
	// First evaluating the accessors
		{ return (
			$global[
				'fact'
			]
			= 
			meaning(
				['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], 
				$global
			)
		); }
		
	5. Now the meaning of the lambda expression must be determined. The function returned by 'listToAction' calls 'meaning', 
	passing the expression and the '$global' context. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context 
	to returned function
	expressionToAction( 
		['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]]
	)( 
		['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]] ,
		$global 
	)
	
	6. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function (
		['lambda',[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]] ,
		$global 
	)
		{ return build('nonPrimitive', cons(context, cdr(e))); } 
		{ return build('nonPrimitive', cons(context, [['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]])); }
		{ return build('nonPrimitive', [$global,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]]); }
		['nonPrimitive', [[$global,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], null]]
		
	7. Therefore, assign the returned epxression to the new name and return the expression
	{ return ( $global['fact'] = 
			['nonPrimitive', [[$global,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], null]]
	); }
	
// given the above
value(s(' (fact 4) '))

	1. Parser converts string to Lisp list, JS implementation
	' (fact 4) ' --> ['fact', [4, null]]
	
	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['fact', [4, null]] , $global )
	
	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( ['fact', [4, null]] )( ['fact', [4, null]] , $global )
	
	4. Expression was not atom, call 'listToAction', which performs lookup in '$specialform', fails, assume it is a function application
	function ( ['fact', [4, null]] , $global ){ return 
		apply(
			meaning(functionOf( ['fact', [4, null]] ), $global ), 
			evlis(argumentsOf( ['fact', [4, null]] ), $global )
		); 
	};
	// accessors are aliases for...
		apply(
			meaning(car( ['fact', [4, null]] ), $global ), 
			evlis(  cdr( ['fact', [4, null]] ), $global )
		);
	// eval accessors
		apply(
			meaning( 'fact'     , $global ), 
			evlis(   [4, null]] , $global )
		);
	// eval func name and arg list
		apply(
			['nonPrimitive', [[$global,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], null]], 
			[4,null]
		);
		
	5. Apply: func was not atom, tagged as 'nonPrimitive', call 'applyClosure'
	apply(fun, vals){
		return ...
			applyClosure(
				second(fun), 
				vals
			)
	}
	// eval accessor 
			applyClosure(
				[$global,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], 
				[4,null]
			)
	// 'applyClosure' decomposes the lambda expression so that it can be evaluated
	applyClosure(closure, vals){ return 
		meaning(
			bodyOf(closure), // third()
			newContext(
				formalsOf(closure), // second()
				vals, 
				tableOf(closure) // first()
			)
		); 
	}
	// eval accessors
	applyClosure(closure, vals){ return 
		meaning(
			[cond,[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]]], 
			newContext(
				[n,null], 
				[4,null], 
				$global
			)
		); 
	}
	
	6. 'newContext' uses the 'begetObject' function of the original context (in this case '$global') to create a copy.  It then iterates through the
    list of 'names' and associates each with the corresponding in the list of 'vals' in a new associative array <theNewContext>
    applyClosure(closure, vals){ return 
		meaning(
			[cond,[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]]], 
			<theNewContext>
		); 
	}
	
	7. Expression was not atom, call 'listToAction', it in turn performs lookup in '$specialform', succeeds. Returns function:
	function ([cond,[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]]] , <theNewContext>)
		{ return evcon(
			condLinesOf([cond,[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]]]), // cdr
			<theNewContext>
		); }
	// eval accessor 
	evcon(
		[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]],
		<theNewContext>
	);

	8. 'evcon' will now evaluate the sublist of conditions expression-by-expression
	isElse(questionOf(car([[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]])))
	// not an 'else' clause, eval the first question to see if it is true
	meaning( [zero?,[n,null]]
		questionOf(car([[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]])), 
		<theNewContext>
	) 
	// eval accessors
	meaning( [zero?,[n,null]]
		['zero?',['n',null]], 
		<theNewContext>
	) 
	// -->
	false
	// first clause was false, evaluate the next clause of 'cond' (recur on sublist), 'else' in this case, therefore
	meaning(answerOf(car(lines)))
	/* At this point it was discovered that 'evcon's call to 'meaning' for the 'else' case was missing the second argument 'context'.  This error prevented 
	access of this call to the defined environment and prevented recursion in the definition of 'fact'.  Corrected 2014-01-22. */
	
	
// Now use the new implementation of contexts as Lisp lists of assoc arrays, each assoc array holds the vars defined in that context, the next item is the
//containing context.  Note that below the name '$global' is used to represent the assoc array that was previously '$global', while under the new
//implementation it is a list consisting of the former bare assoc array: $global = [{assoc array previously $global}, null]
value(s(' (fact 4) '))

	1. Parser converts string to Lisp list, JS implementation
	' (fact 4) ' --> ['fact', [4, null]]
	
	2. 'value' calls 'meaning', passing the expression and the '$global' context
	meaning( ['fact', [4, null]] , $global )
	
	3. 'meaning' calls 'expressionToAction', passing exp, then passes the expression and the context to returned function
	expressionToAction( ['fact', [4, null]] )( ['fact', [4, null]] , $global )
	
	4. Expression was not atom, call 'listToAction', which performs lookup in '$specialform', fails, assume it is a function application
	function ( ['fact', [4, null]] , $global ){ return 
		apply(
			meaning(functionOf( ['fact', [4, null]] ), $global ), 
			evlis(argumentsOf( ['fact', [4, null]] ), $global )
		); 
	};
	// accessors are aliases for...
		apply(
			meaning(car( ['fact', [4, null]] ), $global ), 
			evlis(  cdr( ['fact', [4, null]] ), $global )
		);
	// eval accessors
		apply(
			meaning( 'fact'     , $global ), 
			evlis(   [4, null]] , $global )
		);
	// eval func name and arg list
		apply(
			['nonPrimitive', [[ [$global, null] ,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], null]], 
			[4,null]
		);

	5. Apply: func was not atom, tagged as 'nonPrimitive', call 'applyClosure'
	apply(fun, vals){
		return ...
			applyClosure(
				second(fun), // grab the closure itself
				vals
			)
	}
	// eval accessor 
			applyClosure( // expanded args shown below
				[ [$global, null] ,[['n',null],[['cond',[[['zero?',['n',null]],[1,null]],[['else',[['*',['n',[['fact',[['-',['n',[1,null]]],null]],null]]],null]],null]]],null]]], 
				[4,null]
			)
	// 'applyClosure' decomposes the closure so that it can be evaluated
	applyClosure(closure, vals){ return 
		meaning(
			bodyOf(closure), // third()
			newContext(
				formalsOf(closure), // second()
				vals, 
				tableOf(closure) // first()
			)
		); 
	}
	// eval accessors
	applyClosure(closure, vals){ return 
		meaning(
			[cond,[[[zero?,[n,null]],[1,null]],[[else,[[*,[n,[[fact,[[-,[n,[1,null]]],null]],null]]],null]],null]]], 
			newContext(
				[n,null], 
				[4,null], 
				[$global, null]
			)
		); 
	}
	/* At this point it was discovered that after'lookupInContext' had been re-written, it was now evaluating the truthiness of an attempted variable
        lookup with 'isNull' as a test for whether it is defined.  The obvious flaw in this approach is that if zero or 'false' is stored under that name,
        the test will be false and the lookup will fail.  In the above example, this resulted in infinite recursion because the evaluator became unable
        to retrieve 'n' after it had been set to zero.  Using the 'isUndefined' test resolved the issue.  This highlights the still unresolved problem of 
        a failed name lookup evaluating as an error pair (error <UNKNOWNNAME>) that not only passes any truthiness test on the return result, 
        but also fails to halt execution at all. */

- End Recursion -
	
= End Examples =


== End Evaluation ==

== Parsing Description ==

'EXPPARSER' holds a regular expression that breaks a passed string into tokens.  'rgx_next_match' calls 'EXPPARSER.exec' which returns the next match in
the string after the last match found.  Matches are non-whitespace characters or parentheses.  A parenthesis and an adjacent non-whitespace substring will
return as two different matches.  If no match is found, an empty string is returned.

's' is the wrapper function that begins the parsing process.  It passes its text expression argument 'texp' to recursive helper function 's_build', which
is the guts of the parser.  's_build' fetches tokens one at a time from the 'texp' string, using 'rgx_next_match'.  If the token is not an open paren, 
's_build' attempts to convert it to a number with 'attempt_num' and returns the result.  If the token cannot be converted, then 'attempt_num' returns the 
unchanged string.  If instead an open paren is encountered, then 's_build' will begin a new Lisp list.  As tokens are read, it will process each with a 
recursive call to 's_build', then append each to the list one at a time using 'push_onto_L', which ensures the list is still terminated correctly after 
each append.  When a closing paren is encountered, execution will break out of the list-building loop and return the resulting list.

== End Parsing ==

== Printing Description ==

Function 'p' is used to print output back to the REPL in human-readable form.  If the argument passed to it is a list, an open paren is printed.  'p' is 
called on every list item one at a time to get the string representation of the item, to which is added a trailing space to separate items. The trailing
space is removed after the last item in a list.  If the last item is a non-null atom, it is assumed a cons is being printed, and thus formatted 
accordingly.  A closing paren is printed at the end of the cons structure.  Otherwise if the argument was null and not part of a list, the empty list is
printed.  If the argument was not a list and not null, then the item is returned unchanged.  In this last case whatever automatic string conversion
performed by JS when the return value is concatenated to a string applies.  (In the future may want to coerce a type conversion?) 

== End Printing ==
