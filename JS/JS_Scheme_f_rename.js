/*
REPLacement.js
James Watson, 2013 November
A simple Scheme implementation in JavaScript; it's only a stand-in for the real thing!
Based on "The Little JavaScripter" by Douglas Crockford, with a good deal of modification and addition
*/

/* == LOG ==
2015-02-27: Removed 'load_by_lines', which was just an alias for 'parse_and_eval_lines'
2015-02-23: Rewrote 'load_by_lines', moved old code to 'load_by_lines_old'
2015-02-2N: Wrote: 'eval_consume_built', 'preparse_consume', 'process_remainder', 'parse_and_eval_lines'
2015-02-19: Wrote 'preparse'
2015-02-17: Disabled 's_build_chunk' and 's_chunk' in favor of the pre-parsing approach
2015-02-13: 'load_by_lines_from_cur_dir' now correctly loads a file on the first try. However, due to the asynchronous nature of file loading in Chrome,
            is unable to return the result of the last statement of the loaded file in this this implementation. 
2015-02-04: Tested 'load_by_lines' -  Success.
2015-02-03: Changed 'load_by_lines_from_cur_dir' to 'load_by_lines', now given an array of text lines and has no file loading responsibilities
2015-02-02: Disabled the 'load' entry in '$specialform'. Google Chrome file loading facilities do not appear to be working consistently
2014-02-05: Re-wrote table search / extension such that a deep copy of the '$global' context is not created each time a lambda expression is executed.
            This gets a little closer to the "Schemer" implementation, with only limited changes to the guts of the evaluator, see below:
            - 'newContext': Re-written, reads formals and associated values and creates an associative array from these, then conses this new object onto
                            the old context object
            - 'lookupInContext': Re-written, checks if the context passed is non-null, then proceeds to search each assoc array in the list, returning
                                 'undefined' if the end of the list is reached without a positive hit
            - 'Object.begetObject': Removed, 'newContext' was the sole user of this function. Due to the above, it is no longer needed.
2014-01-23: * Added: - Reserved symbol "atom?" calls primitive 'asAtom' that returns true if the argument is a string, number, or boolean, otherwise false
                     - Reserved symbol "number?" calls primitive 'isNumber' that returns the result of JS function 'isFinite' called on argument
2014-01-22: 'evcon' call to 'meaning' for the 'else' case was missing the second argument 'context'.  This error prevented access of this call to the
            defined environment and prevented recursion in some cases.  Corrected.
2013-12-07: * Rewrote the 'and' special form handler to more closely conform to R5RS specification, which returns the
              the characteristic evaluation result rather than a simple true/false
              - Wrote the '$and' function to recursively process arguments passed to 'and', invoked only in the case one or more arguments passed
            * Rewrote the 'or' special form handler to more closely conform to R5RS specification, which returns the
              the characteristic evaluation result rather than a simple true/false
              - Wrote the '$or' function to recursively process arguments passed to 'or', invoked only in the case one or more arguments passed
2013-12-04: Copied the following functions from "little.js": 'isList', 'p'
2013-12-03: All "primitive" function declaration lists in '$global' are now formed with 'build' rather than writing Lisp lists in JS long-hand
2013-12-01: * Changed regex 'EXPPARSER' so that spaces are not matched, and only the match is printed as the sole
              item returned by 'exec'. Previous version matched spaces and returned array: [match, match-without-spaces]
            * 'rgx_next_match' now either returns the string contained in the array or an empty string
2013-11-30: Rewrote 'newContext' such that it iterates through values and names as Lisp lists, not JS arrays
2013-11-26: In 'newContext', moved the declaration of 'for' counter variable 'i' inside the loop expression, for clarity and style reasons
2013-11-24: * Added: - Reserved symbol "*" calls primitive 'multiply' that returns the product of an arbitrary number of arguments,
                       returns 1 if no args given
                     - Reserved symbol "/" calls primitive 'divide' that returns the quotient of the first agument divided by every subsequent
                       argument, returns 1 if no args given
                     - Reserved symbol "1+" calls primitive 'add1' that returns the first argument plus 1, returns 1 if no argument given
                     - Reserved symbol "1-" calls primitive 'sub1' that returns the first argument minus 1, returns -1 if no argument given
                     - Reserved symbol "<" calls primitive 'lt' that returns true if two or more arguments that increase monotonically, else false,
                       if less than two arguments are passed, returns false
                     - Reserved symbol ">" calls primitive 'gt' that returns true if two or more arguments that decrease monotonically, else false,
                       if less than two arguments are passed, returns false
                     - Reserved symbol "<=" calls primitive 'le' that returns true if two or more arguments that increase (or are equal)
                       monotonically, else false, if less than two arguments are passed, returns false
                     - Reserved symbol ">=" calls primitive 'ge' that returns true if two or more arguments that decrease (or are equal)
                       monotonically, else false, if less than two arguments are passed, returns false
            * Nested math operations work as expected
            * 'add1' and 'sub1' now handle the case that no argument is passed, return 1 and -1, respectively
            * Wrote functions 'make_list_comparator', 'lt_help', 'gt_help', 'le_help', 'ge_help'
2013-11-23: * Moved 'flatten' outside of 'applyPrimitive' so that it is not created every time 'applyPrimitive' is called
            * Removed unary operator from 'add1' as numbers are now automatically converted by the parser
            * Added: - Reserved symbol "+" calls primitive 'plus' that sums an arbitrary number of arguments, returns 0 if no args given
                     - Reserved symbol "-" calls primitive 'minus' that returns the difference between the first arg and all subsequent args,
                       returns 0 if no args given
2013-11-19: For an investigation of 'this' at the top level of a JS file, see "Learning/globalThis.js"
2013-11-18: Processed with "notepad_safe.py", original file stored in "REPLacement.js.bak"
2013-11-17: * Copied the following functions/variables from "little.js": 'global', 'build', 'first', 'second', 'third', 'newEntry',
              'lookupInEntryHelp', 'lookupInEntry', 'extendTable', 'lookupInTable', 'lookupInContext', 'newContext', 'expressionToAction',
              'listToAction', '$specialform', '$global', 'value', 'evcon', 'apply', 'applyPrimitive', 'applyClosure', 'meaning', 'tableOf',
              'formalsOf', 'bodyOf', 'textOf', 'questionOf', 'answerOf', 'bodyOf', 'isElse', 'evcon', 'evlis', 'functionOf', 'argumentsOf',
              'EXPPARSER', 'rgx_next_match', 'push_onto_L', 'attempt_num', 's_build', 's'
            * Passed evaluator tests! (Taken from "Chaper 10 Tests" of "little_UT.js"), test on 'rember' removed, 'rember' not in this implementation
2013-11-16: Copied the following functions from "little.js": 'Object.prototype.begetObject', 'cons', 'car', 'cdr', 'isAtom', 'isNull', 'isEq',
            'isNumber', 'isBoolean', 'isUndefined', 'isFunction'

== TODO ==
* Add the facility to parse a line as a partial form, indicating to the client code that the form is incomplete. The client code can send the form to the
  evaluator when the parser has constructed a complete statement. Consider using the 'depth' method implemented elsewhere
* Add strings (differentiated from symbols, which are currently JS strings), the 'load' form should be passed a string, not a symbol
* Investigate the necessity of 'var global = this;' at the end of this file. See notes?
* Add exponentiation
* Add the facility to declare cons literals
* Implement some kind of error system - presently a failed name lookup evaluates as an error pair (error <UNKNOWNNAME>) that not only passes any
  truthiness test on the return result, but also fails to halt execution at all.
* Study the performance of object lookup (associative array) versus hash lookup --> At what point should '$global' be a hash?

== NOTES ==
* 2015-02-18: There are several approaches that can be taken to parsing and evaluating a for in which expressions can span multiple lines or share a line
              with other expressions
              - Each line can be read and parsed as a unit. When a line ends in an incomplete statement, an INCOMPLETE object is inserted at the location
                where the next token would have been placed. On the next line read, the code is informed that there is a partially formed statement
                already in storage. The parser crawls the partial statement until the INCOMPLETE marker is reached, the next token to be parsed replaces
                the marker, then parsing continues as usual. This might be accomplished with a version of 's' or 's_build' that has the ability to crawl
                an existing partial expression recursively until it recognizes the point where it can begin inserting. In this way, crawling becomes one
                of the recursive cases of the expression building process, and any depth of nexting is handled naturally without complex bookkeeping.
                The main drawback is that the entire expression built so far must be traversed each time a new line is read from the source file.
              - Lines can be read until a pre-parser function determines that nesting depth has resturned to 0 after reading a non-zero number of
                characters. The pre-parser appends these strings onto a single string for final parsing and evaluation. Conversely, if the pre-parser
                finds it has a complete expression candidate and has excess (non-comment) characters left in the line, then it would both package the
                completed string as described and treat the remainder as a line read. The pre-parser would not perform any tokenization, 
                but would be responsible for slicing and concatenating strings based on open and close parens. It would also be responsible for handling
                comments.
* 2015-02-13: * URL, asynchronous file loading: http://stackoverflow.com/questions/28485231/chrome-app-cannot-retrieve-file-load-status
              * Found that the asynchronous nature of Chrome file loading made it it difficult to return to the calling code with the result of the file
                load (error, or the result of the last form in the file). Briefly considered implementing a mechanism whereby a statement that contains
                the load statement would be saved, then exited to give the file a chance to load, then resumed from the saved state. Even in this scenario
                it would be difficult to determine the execution order within the JavaScript engine, and thus difficult to determine whether any efforts
                to "hang" execution, either for the user's or the evaluator's sake would be successful. Honestly, such an undertaking would not likely be
                worth the effort if REPLacement will be ported to C or some other environment where I can better control execution order.
* 2013-11-NN: Briefly considered removing the ability to reach into the global 'this' from 'expressionToAction', found this mechanism was unable to reach
              into built-in functions such as 'Math.pow'. Decided to keep this functionality in case any of the extensive list of "little.js" functions 
              are to be used, even though this mechanism requires a flattening operation for args
*/

// == Primitives ==

// = cons Structures =
function cons(a, d){ return [a, d]; } // Contruct a cons as a two-item JS list (a . d) --> [a,d]
function car(s){ return s[0]; }
function cdr(s){ return s[1]; }
function build(s1, s2){ return cons(s1, cons(s2, null)); } // return a two-item list with 's1' as the first item and 's2' as the second item
var first = car; // 'first' alias for 'car', return the first item of an LS pair
function second(l){ return car(cdr(l)); } // Return the second item in a list, (cadr l)
function third(l){ return car(cdr(cdr(l))); } // return the third item of 'l', (caddr l)
// = End Structures =

// = Type and Equivalency Predicates =
function isAtom(a){ return typeof a === 'string' || typeof a === 'number' || typeof a === 'boolean'; } // 'a' is any of String, Number, or Boolean
function isNull(a){ return typeof a === 'undefined' || (typeof a === 'object' && !a); } // is undefined or a false-like object
function isEq(s, t){ return s === t; } // Args are strictly equivalent
function isNumber(a){ return isFinite(a); } // URL: http://www.w3schools.com/jsref/jsref_isfinite.asp
function isBoolean(a){ return typeof a === 'boolean'; }
function isUndefined(a){ return typeof a === 'undefined'; }
function isFunction(a){ return typeof a === 'function'; } // Arg is a JS function
function isZero(s){ return s === 0; } // Arg is strictly equivalent to 0
function isList(a){ return a && typeof a === 'object' && a.constructor === Array; }
// = End Predicates =

// = Mathematics =
function add1(n){ return n ? n + 1 :  1; } // returns the first argument plus  1, returns  1 if no argument given
function sub1(n){ return n ? n - 1 : -1; } // returns the first argument minus 1, returns -1 if no argument given

// URL: http://stackoverflow.com/questions/2141520/javascript-variable-number-of-arguments-to-function
function plus(){ // sums an arbitrary number of arguments, returns 0 if no args given
	var sum = 0;
	for(var arg = 0; arg < arguments.length; arg++){ sum += arguments[arg]; }
	return sum;
}

function minus(){ // returns the difference between the first arg and all subsequent args, returns 0 if no args given
	var sum = arguments[0] || 0;
	for(var arg = 1; arg < arguments.length; arg++){ sum -= arguments[arg]; }
	return sum;
}

function multiply(){ // returns the product of an arbitrary number of arguments, returns 1 if no args given
	var product = arguments[0] || 1;
	for(var arg = 1; arg < arguments.length; arg++){ product *= arguments[arg]; }
	return product;
}

function divide(){ // returns the quotient of the first agument divided by every subsequent argument, returns 1 if no args given
	var quotient = arguments[0] || 1;
	for(var arg = 1; arg < arguments.length; arg++){ quotient /= arguments[arg]; }
	return quotient;
}

function make_list_comparator(func){ // return a function that returns true only if the comparison of on arg to the next is true monotonically
	return function(){           // also returns false if less than two arguments given
		var rtnBool = false, op = arguments[0];
		if(arguments[1]){
			rtnBool = true;
			for(var arg = 1; arg < arguments.length; arg++){
				if(!(func(op, arguments[arg]))){ rtnBool = false; break; }else{ op = arguments[arg]; }
			}
		}
		return rtnBool;
	}
}

function lt_help(op1, op2){ return op1 <  op2; }
function gt_help(op1, op2){ return op1 >  op2; }
function le_help(op1, op2){ return op1 <= op2; }
function ge_help(op1, op2){ return op1 >= op2; }

var lt = make_list_comparator(lt_help); // list comparator for "<" Less Than
var gt = make_list_comparator(gt_help); // list comparator for ">" Greater Than
var le = make_list_comparator(le_help); // list comparator for "<=" Less Than or Equal To
var ge = make_list_comparator(ge_help); // list comparator for ">=" Greater Than or Equal To

// = End Math =

// == End Primitives ==

// == Environment ==

var newEntry = build; // 'newEntry' an alias for 'build'

function lookupInEntryHelp(name, names, values, entryF){ // return value associated with name, by linear search, or eval (entry-f name)
	return isNull(names) ? entryF(name) : // list of names null, invoke the contingency func
		isEq(car(names), name) ? car(values) : // item name match, return item value
			lookupInEntryHelp(names, cdr(names), cdr(values), entryF); // else, recur on sublists
}

function lookupInEntry(name, entry, entryF){ // return value associated with name if entry, otherwise (entry-f name)
	return lookupInEntryHelp(name, first(entry), second(entry), entryF); //unpack entry into appropriate args for a helper function
}

// A table (also called an environment) is a list of entries.
var extendTable = cons; /* Define extend-table as an alias for cons. Takes an entry and a table (possibly an empty one) and creates a new table
by putting the new entry in the front of the old table. */

function lookupInTable(name, table, tableF){ // return value associated with name in table, if name exists
	return isNull(table) ? tableF(name) : // table null, eval (table-f name)
		lookupInEntry( // else, lookup in first entry
			name,
			car(table),
			function(name){ return lookupInTable(name, cdr(table), tableF); } // if not found, recur on subtable (next entry)
		);
}

//function lookupInContext(name, context){ return context[name]; } // return the value belonging to key 'name' in assoc array 'context'
function lookupInContext(name, context){
	var temp;
	return isNull(context) ? undefined : // if context is null, not possible to find, it is undefined
		!isUndefined(temp = car(context)[name]) ? temp: // context exists, attempt to assign lookup result to 'temp', if lookup succeeds, return 'temp'
			lookupInContext(name, cdr(context)); // lookup failed, recur on the next containing namespace
}

/* function newContext(names, vals, oldContext){
	var c = oldContext ? oldContext.begetObject() : {}; // if 'oldContext' exists, assign a copy to 'c', otherwise assign empty object
	//for(var i = 0; i < names.length; i += 1){ c[names[i]] = vals[i]; }
	while(!isNull(names)){
		c[car(names)] = car(vals);
		names = cdr(names);
		vals = cdr(vals);
	}
	return c;
} */

function newContext(names, vals, oldContext){ // create a new context of name-value pairs consed onto the 'oldContext'
	var c = {}; // 'c' is an object (associative array) to hold the new name-value pairs
	while(!isNull(names)){
		c[car(names)] = car(vals);
		names = cdr(names);
		vals = cdr(vals);
	}
	return cons(
		c,
		( oldContext ? oldContext : null ) // If 'oldContext' exists, then store a reference to it in the cdr, else null cdr
	);
}

// == End Environment ==

// == Evaluation ==

// = Reserved Words and Special Forms =

var textOf = second; // 'textOf' is an alias for 'second', retrieve the contents of the quoted expression

// -- lambda helpers --
var tableOf = first, // -- alias, table is the first item
    formalsOf = second, // alias, formals list is the second item
    bodyOf = third; // --- alias, body is the third item
// -- end lambda --

// -- cond helpers --
var questionOf = first, // alias, question - condition to eval is the first item
    answerOf = second, // alias, answer - action to take is the second item
    condLinesOf = cdr; // alias, lines - cond lines are in the sublist following func name
function isElse(x){ return isAtom(x) && isEq(x, 'else'); } // is the arg an 'else symbol?
// -- end cond --

function evcon(lines, context){ // evaluate cond form by form, this is the guts of cond
	return isElse(questionOf(car(lines))) ? meaning(answerOf(car(lines)), context) : // item question is 'else, eval item answer
		meaning(questionOf(car(lines)), context) ? meaning(answerOf(car(lines)), context) : //eval item question -> is true, eval item answer
			evcon(cdr(lines), context); // else, recur on sublist lines and table
} // note there was no action for the 'null?' case, one of the above conditions better be true!

var $global = [ // a one-item list that contains the global context
	{ // associative array of reserved names, the global context
	'true':    true,
	'false':   false,
	'#t':      true,
	'#f':      false,
	'atom?':   build('primitive', isAtom),
	'eq?':     build('primitive', isEq),
	'null?':   build('primitive', isNull),
	'zero?':   build('primitive', isZero),
	'number?': build('primitive', isNumber),
	'+':       build('primitive', plus),
	'-':       build('primitive', minus),
	'*':       build('primitive', multiply),
	'/':       build('primitive', divide),
	'1+':      build('primitive', add1),
	'1-':      build('primitive', sub1),
	'<':       build('primitive', lt),
	'>':       build('primitive', gt),
	'<=':      build('primitive', le),
	'>=':      build('primitive', ge)
	},
	null
];

// - Logical Operator Helpers -

/* ~ 'and', R5RS (PDF), pg 11 ~
The test expressions are evaluated from left to right, and the value of the first expression that evaluates to a false value is returned. Any remaining
expressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is returned. If there are no expressions
then '#t' is returned. */

function $and(e, context){ // the recursive portion of the evaluation of 'and', case that args list non-null
	var current; // var to hold meaning of current arg, '$and' assumes there is at least one
	return !(current = meaning(car(e), context)) ? current : // fetch first item 'meaning', if false, return
		isNull(cdr(e)) ? current : $and(cdr(e), context); // else if list end, return 'current', else recur sublist
}

/* ~ 'or' , R5RS (PDF), pg 11 ~
The test expressions are evaluated from left to right, and the value of the first expression that evaluates to a true value is returned. Any remaining
expressions are not evaluated. If all expressions evaluate to false values, the value of the last expression is returned. If there are no expressions
then #f is returned. */

function  $or(e, context){
	var current; // var to hold meaning of current arg, '$and' assumes there is at least one
	return (current = meaning(car(e), context)) ? current : // fetch first item 'meaning', if true, return
		isNull(cdr(e)) ? current : $or(cdr(e), context); // else if list end, return 'current', else recur sublist
}

// - End Logical Helpers -

var $specialform = { // associative array of special form names with associated actions
	quote:  function (e, context){ return textOf(e); },
	lambda: function (e, context){ return build('nonPrimitive', cons(context, cdr(e))); },
	cond:   function (e, context){ return evcon(condLinesOf(e), context); },
	define: function (e, context){ return (car(context)[second(e)] = meaning(third(e), context)); },
	and:    function (e, context){ return isNull(cdr(e)) ? true: $and(cdr(e), context); }, // handle null case or recur
	or:	    function (e, context){ return isNull(cdr(e)) ? false: $or(cdr(e), context); }, // handle null case or recur
	load:   function (e, context){ return load_by_lines_from_cur_dir( textOf(e), context ); }
};

// = End Words / Forms =

// = Evaluator =

function flatten(l) { // funcName.apply() is the magic that allows us access to JS functions!
	// push Lisp list items onto a JS array to be used by 'fun.apply', passed to 'fun'
	var a = [];
	while( !isNull(l) ){
		a.push(car(l));
		l = cdr(l);
	}
	return a;
}

// URL, call JS function with args in array with 'apply': http://msdn.microsoft.com/en-us/library/ie/4zc42wh1(v=vs.94).aspx
function applyPrimitive(fun, vals){ // takes function name and array of values, flattens arg list to array, and applies
	return fun.apply(null, flatten(vals));
}

// apply a non-primitive function
function applyClosure(closure, vals){ return meaning(bodyOf(closure), newContext(formalsOf(closure), vals, tableOf(closure))); }

function apply(fun, vals){ // evaluate primitives and closures (non-primitive functions)
	return isAtom(fun) ? fun :
		isEq(first(fun), 'primitive') ? applyPrimitive(second(fun), vals) :
			isEq(first(fun), 'nonPrimitive') ? applyClosure(second(fun), vals) :
				fun;
}

// -- application helpers --
var functionOf = car, // alias, the function name is the first item
    argumentsOf = cdr; // alias. the arguments are in the sublist after first
// -- end application --

function evlis(args, context){ // take list of representations of expressions and return list of meanings
	return isNull(args) ? null : // list end or list null, return null
		cons(meaning(car(args), context), evlis(cdr(args), context)); //else, cons the meaning of the args item onto recur sublist args and table
}

function listToAction(e){ // Return one of ...
	//     special form action  OR a function that returns the result of applying the form assuming the first item is a func name
	return $specialform[car(e)] || function (e, context){ return apply(meaning(functionOf(e), context), evlis(argumentsOf(e), context)); };
}

function expressionToAction(e){ // attempt to assign appropriate action to the given expression 'e'
	return isAtom(e) ? function $identifier(e, context) { // if 'e' is atom, return the inline '$identifier' function
		if( isNumber(e) || isBoolean(e) ){ return e; } // if 'e' number or boolean, return 'e'
                var i = lookupInContext(e, context); // else not number/boolean literal, attempt lookup of assumed symbol in reserved words
                if( !isUndefined(i) ){ return i; } // if lookup succeeded, return result
                i = global[e]; // else attempt lookup in defined JS names (global 'this')
                if( isFunction(i) ){ return build('primitive', i); } // if lookup result is function, return LS pair with 'primitive' first item
                return build('error', e); // else error, not recognized as literal, reserved word, defined symbol, or JS function
		} : // else 'e' not atom
			isNull(e) ? null : // if 'e' is empty list, return null
				listToAction(e); // else return result of 'listToAction'
} // Note that in the above, 'context' is not defined, but it is determined by the call to 'meaning' that called 'expressionToAction'

function meaning(e, context){ return expressionToAction(e)(e, context); } // lookup action for expression e, and apply to e and global table

function value(e){ return meaning(e, $global); } // this function, together with all the functions it uses, is an interpreter
// call meaning on expression in the '$global' context

// = End Evaluator =

// == End Evaluation ==

// == Parsing and Printing ==

// = Pre-parser Functions / Accounting =

/* 2015-02-18: There are several approaches that can be taken to parsing and evaluating a for in which expressions can span multiple lines or share a line
               with other expressions
               - Lines can be read until a pre-parser function determines that nesting depth has resturned to 0 after reading a non-zero number of
                 characters. The pre-parser appends these strings onto a single string for final parsing and evaluation. Conversely, if the pre-parser
                 finds it has a complete expression candidate and has excess (non-comment) characters left in the line, then it would both package the
                 completed string as described and treat the remainder as a line read. The pre-parser would not perform any tokenization, 
                 but would be responsible for slicing and concatenating strings based on open and close parens. It would also be responsible for handling
                 comments. */

// Pre-parse bookkeeping, resetting of these for the sake of 'preparse' is the responsibility of the code doing the reading
var depth = 0; // ------- paren depth, 0 is the base depth (not inside any parens), decreases with nesting. ( depth = -1 ( depth = -2 ))
var lastOpenDex = 0; // - index within the last full 'texp' (line) sent to 'preparse' where an open paren was found
var strRemainder = ''; // place-holder for partial lines/strings, client code should remember to erase this after it is pre-parsed
var builtExp = ''; // --- The text expression built so far
var expCmplt = false; //- flag fo whether 'builtExp' is a candidate for a complete expression

function preparse(texp, rmndr){ // Determine depth change within text expression. Strip comments
	//var parseArr = ['', '', -1]; 
	
	var xChar = ''; // character under scrutiny
	
	// FIXME: Correctly handle multiple statements, correctly handle atoms (no depth decrease), 
	//        Correctly handle intermediate line that does not begin or end a candidate s-expression
	for(var index = 0; index < texp.length; index++){ // for every character in the given 'texp'
		xChar = texp[index]; // read a char from the line

		if(expCmplt){ strRemainder += xChar; } //append excess character to remainder string, resetting 'expCmplt' responsibility of client code
		else{ // else we are in the middle of an expression
			if( xChar === '(' ){ // open paren, decrement 'depth' and determine index within containing string
				depth--;
				rmndr ? lastOpenDex = builtExp.length - 1 + index : lastOpenDex = index;
			}else if( xChar === ')' ){ depth++; } // close paren, increment depth and judge completeness
			builtExp += xChar; // Append expression character to candidate expression in all cases when the candidate is incomplete
			if( depth === 0 ){ expCmplt = true; } // expression was completed or we processed an atom
		}
	}

	//return parseArr;
}

// = End Pre-parser =


// = Incomplete Input =
// Handle partially complete statements in 'texp' 

function eval_consume_built(){ value( s( builtExp ) ); builtExp = ''; expCmplt = false; } // parse & evaluate 'builtExp', then erase it
function preparse_consume(texp){ preparse(texp); if( expCmplt ){ eval_consume_built(); } }

function process_remainder(){ // preparse an arbitrary amount of text left over from the last complete statement, may be fractional or multiple statements
	var temp = '';
	while( strRemainder.length > 0 ){
		temp = strRemainder; strRemainder = ''; // fetch and erase remainder
		preparse_consume(temp); // will produce a new remainder if needed
	}
}

function parse_and_eval_lines( lineArr ){ // determine the 'meaning' of an array of strings line by line
	
	for(var i = 0; i < lineArr.length; i++){

		preDebug.innerText += '\r\n' + lineArr[i]; // FIXME: Remove debug behavior
		preparse_consume(lineArr[i]); 
		process_remainder(); 

		//value( s( lineArr[i] ) ); // 'value' evaluates in the '$global' context
	}
	
}

// = End Incomplete =


// URL: http://stackoverflow.com/questions/19713284/javascript-regex-exec-returns-match-repeated-in-a-list-why
var EXPPARSER = /(?:\(|\)|[^\s()]+|$)/g;

function rgx_next_match(texp){ // Fetch the next REGEX match in 'texp', if no match return an empty string
	var r = EXPPARSER.exec(texp); // --- The result of the most recent execution of regex
	return r[0] || ''; // ------- Return matching string, otherwise return empty string
}

// Return a list that is 'L' with 'item' appended to the end.  If 'L' is null, return a list with only one 'item'
function push_onto_L(L,item){ if( isNull(L) ){ return cons(item, null); }else{ L[1] = push_onto_L(L[1], item); return L; } }

function attempt_num(canNum){ // Attempt to coerce type to number, return raw arg if unsuccessful
	rtnNum = Number(canNum);
	return isNaN(rtnNum) || canNum == '' ? canNum : rtnNum; // Number('') == 0, WTF JAVASCRIPT!
}

// = Complete Input =
// Assume that 'texp' contains a complete statement

function s_build(texp){ // parse texp into an s-expression with conses represented as two-member JS arrays // runs in n^2 time, ick!
	var token = rgx_next_match(texp); // fetch first regex match, store in 'token'
	var rtnL; // return value, an s-expression (atom or list)
	if( token == '(' ){ // open paren, reading a list
		//depth--; // entered paren, decrement depth
		do{
			token = s_build(texp); // fetch item, if an open paren is read, a new list will be returned
			if(token !== '' && token !== ')'){ rtnL = push_onto_L(rtnL, token); }
			else{ /*depth++;*/ break; } // if not close paren, push, else increment depth and end list
		}while(true)
	}else{ rtnL = attempt_num(token); } // was an atom, attempt number conversion and return it
	return rtnL;
}

function s(texp){ // parse expression text expression 'texp', reset regex index, and return s-expression 'rtnExp'
	var rtnExp = s_build(texp);
	EXPPARSER.lastIndex = 0; // reset the index of the parser
        // index not reset within 's_build' because we want to read the string linearly while correctly building nested structures
	//depth = 0; // reset depth gauge
	return rtnExp;
}

// = End Complete =


// = Formatting / Printing =

function p(x){ // Produce a printable presentation of an s-expression

	var r; // Local var to hold printed list

	if( isList(x) ){ // If x is a list
		r = '('; // opening paren
		do{
			r += p(car(x)) + ' '; // call p() on first item, add a space, and append the whole thing to the return string
			x = cdr(x);           // set x to the next sublist
		}while( isList(x) ); // If sublist is also list, then repeat

		if( r.charAt(r.length - 1) === ' ' ){ r = r.substr(0, r.length - 1); } // Remove trailing space if present
		if( isAtom(x) ){ r += ' . ' + x; } // Inside a cons, but cdr(x) was atom, not list, therefore inside plain cons, print as such
		return r + ')'; // closing paren
	}
	if( isNull(x) ){ return '()'; } // Else if x is null, return an empty list
	return x; // Else if x is neither list nor null, return the argument as-is
}

// = End Printing =

// == End Parsing / Printing ==

// == File Operations ==

function load_by_lines_old( lineArr ){ // determine the 'meaning' of an array of strings line by line
	
	for(var i = 0; i < lineArr.length; i++){
		preDebug.innerText += '\r\n' + lineArr[i]; // FIXME: Remove debug behavior
		value( s( lineArr[i] ) ); // 'value' evaluates in the '$global' context
	}
	
}

/* Assumes an 'External' object that handles file operations and possesses the following: 'read_file_in_load_path', 'curFile.lodd'
   Assumes that 'External' is already pointed a valid directory, Chrome: 'chrome.fileSystem.chooseEntry' (user action) 
   Assumes that each line contains one and only one complete form that can be parsed and evaluated

URL, asynchronous file loading: http://stackoverflow.com/questions/28485231/chrome-app-cannot-retrieve-file-load-status

Marc Rochkind: "You can't kick off an asynchronous activity and then busy-wait for it to complete, because JavaScript is single threaded. No matter 
what's happened, the asynchronous function won't be executed until the script completes its current processing. 
In other words, asynchronous does not imply concurrent.

Generally speaking, if task A is to be performed after asynchronous task B completes, you should execute A from the completion callback for B. 
That's the straightforward, safe way to do it. Any shortcut, to achieve better responsiveness or to simplify the code, is going to have dependency or 
race-condition problems, and will require lots of horsing around to get right. Even then, it will be hard to prove that the code operates correctly 
on all platforms in all circumstances."
*/


function load_by_lines_from_cur_dir( fileName, context ){ // determine the 'meaning' of a file line by line
	
	var curLineMeaning = 1;	
	External.read_file_in_load_path(fileName, context); // 'External' load 'fileName' and reads lines, REPLacement evaluates within call-back func
	return curLineMeaning; // return 1 for returning's sake
}

// == End File ==


// == JavaScript Extensions ==

// Allows us to call vanilla JS functions by name, if we so choose
var global = this; // assign the global context/object to a variable named 'global'

// == End Extensions ==


/* == Abandoned Components ==

Object.prototype.begetObject = function(){ // Returns a copy of the Object that contains all the key-value pairs of the original, including 'begetObject'
	var F = function(){};
	F.prototype = this;
	return new F();
};

// ~ Time Study ~
//var sysTime = new Date();
var lastClock = 0;
function nuClock(){ lastClock = Date.now(); }
function time_since_last(){
	var temp = Date.now();
	var interval = temp - lastClock;
	lastClock = temp;
	return interval;
}

// URL, sleep: http://stackoverflow.com/questions/16623852/how-to-pause-javascript-code-excution-for-2-seconds
function sleep(miliseconds){
	var currentTime = Date.now();
	while ( currentTime + miliseconds >= Date.now() ) { console.log( 'Asked for idle time at:' + Date.now() ); }
}

// ~ End Time ~

//function idle(){  }

function load_by_lines_from_cur_dir( fileName, context ){ // determine the 'meaning' of a file line by line, return last 'meaning', otherwise 'null' 
	
	var curLineMeaning = 1;
	//var lastLineValid = true;
	
	External.read_file_in_load_path(fileName, context); // 'External' load 'fileName' and reads lines, REPLacement evaluates within call-back func

	// This is a dirty workaround that accounts for the fact that 'DirectoryEntry.getFile' is asynchronous, thus pre-parsing checks fail intil loaded
	var counter = 0, maxLoops = 10;
	nuClock();
	do{ 
		sleep(500); 
		counter++; 
		preDebug.innerText += '\r\nLoop:' + counter + " , " + time_since_last();

	}while( !External.curFile.lodd && (counter < maxLoops) );  // idle and check if file loaded, 5000ms max
	
	preDebug.innerText += '\r\nLoaded?:' + External.curFile.lodd;
	preDebug.innerText += '\r\nLines?:' +  External.curFile.lins;
	
	if( External.curFile.lodd ){ // The last load operating was successful, attempt to parse and interpret each line
		// parse and interpret lines, storing each meaning in 'curLineMeaning', until last line is reached
		while(!External.curFile.rEOF){ 
			curLineMeaning = meaning( s( External.readln_from_current_file() ), context); 
			preDebug.innerText += '\r\nNext Line?: ' + External.curFile.lnnm;
			preDebug.innerText += '\r\nEOF?: ' + External.curFile.rEOF;
		}
	} // else, return 'null'
	return curLineMeaning; // return 1 for returning's sake
}

and add facility to resume s-expression on next read

2015-02-18: There are several approaches that can be taken to parsing and evaluating a for in which expressions can span multiple lines or share a line
               with other expressions
               - Each line can be read and parsed as a unit. When a line ends in an incomplete statement, an INCOMPLETE object is inserted at the location
                 where the next token would have been placed. On the next line read, the code is informed that there is a partially formed statement
                 already in storage. The parser crawls the partial statement until the INCOMPLETE marker is reached, the next token to be parsed replaces
                 the marker, then parsing continues as usual. This might be accomplished with a version of 's' or 's_build' that has the ability to crawl
                 an existing partial expression recursively until it recognizes the point where it can begin inserting. In this way, crawling becomes one
                 of the recursive cases of the expression building process, and any depth of nexting is handled naturally without complex bookkeeping.
                 The main drawback is that the entire expression built so far must be traversed each time a new line is read from the source file.


var conSeq = []; // sequence of 'car' and 'cdr' needed to navigate back to the INCOMPLETE marker

function INCOMPLETE(){ this.is_INCOMPLETE = true; } // marker for where to append remaining s-expression terms and branches

// Recursive search for incomplete object, insert at that point

function s_build_chunk(texp){ // parse texp into an s-expression with conses represented as two-member JS arrays // runs in n^2 time, ick!
	var token = rgx_next_match(texp); // fetch first regex match, store in 'token'
	var rtnL; // return value, an s-expression (atom or list)
	if( token == '(' ){ // open paren, reading a list
		depth--; // entered paren, decrement depth
		do{
			token = s_build(texp); // fetch item, if an open paren is read, a new list will be returned

			//if(token !== '' && token !== ')'){ rtnL = push_onto_L(rtnL, token); }
			//else{ depth++; break; } // if not close paren, push, else increment depth and end list
			if(token === ')'){ depth++; break; }
			else if( token === '' ){ if( depth < 0 ){ rtnL = push_onto_L(rtnL, new INCOMPLETE() ); break; }else{ break; } }
			else{ rtnL = push_onto_L(rtnL, token); }


		}while(true)
	}else{ rtnL = attempt_num(token); } // was an atom, attempt number conversion and return it
	return rtnL;
}

function s_chunk(texp, prevS){ // parse expression text expression 'texp', reset regex index, and append to 'prevS' if it exists
	
}

// == End Abandoned == */
