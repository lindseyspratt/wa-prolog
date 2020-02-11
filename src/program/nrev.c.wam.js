const util = require('./utilities.c.wam.js');
/*
nrev([],[]).

nrev([X|Rest],Ans):-
	nrev(Rest,L),
	append(L,[X],Ans).


	Var     Last Goal   Offset
	X       append      Y1
	Rest    nrev        X3 -- temporary var, only present in head and first goal.
	L       append      Y2
	Ans     append      Y3


	try_me_else L
	get_constant []
	get_constant []
	proceed
L:	trust_me ;; X4 -> Y1, A1 -> [X4|X3], A2 -> Y2. head. X3 -> A1, Y3 -> A2. call nrev/2. Y1 -> X4, Y3 -> A1, [X4] -> A2, Y2 -> A3. exec append/3.
	allocate			// nrev
	put_variable 1, 1, 4,//   Y1 <==> X4
	get_list 1			//     ([
	unify_value 4		//		 X
	unify_variable 3	//		  |Rest],
	get_variable 1, 2, 2//               Ans) :-
	put_value 0, 3, 1,  //    nrev(Rest,
	put_variable 1, 3, 2,//             L
	call nrev/2, 3,		 //	             ),
	put_unsafe_value 1, 4    //   append(...X4  <= Y1
	put_unsafe_value 3, 1,   //          L,
	put_list 2,  		//	           [
	set_value 4       	//              X...X4
	set_constant []   	//               ],
	put_value 1, 2, 3   //                 Ans
	deallocate          //
	execute append/3    //                    )
*/

function nrevProgram () {
	return util.process_labels([
		util.opCodes.try_me_else, {ref: 'L'},                   // try_me_else, L1
		util.opCodes.get_constant, util.lookup_atom("[]"), 1,
		util.opCodes.get_constant, util.lookup_atom("[]"), 2,
		util.opCodes.proceed,
		{label: 'L'},
		util.opCodes.trust_me,
		util.opCodes.allocate,
		util.opCodes.put_variable, 1, 1, 4,
		util.opCodes.get_list, 1,
		util.opCodes.unify_value, 4,
		util.opCodes.unify_variable, 3,
		util.opCodes.get_variable, 1, 2, 2,
		util.opCodes.put_value, 0, 3, 1,
		util.opCodes.put_variable, 1, 3, 2,
		util.opCodes.call, util.lookupIndicator("nrev",2), 3,
		util.opCodes.put_unsafe_value, 1, 4,
		util.opCodes.put_unsafe_value, 3, 1,
		util.opCodes.put_list, 2,
		util.opCodes.set_value, 4,
		util.opCodes.set_constant, util.lookup_atom("[]"),
		util.opCodes.put_value, 1, 2, 3,
		util.opCodes.deallocate,
		util.opCodes.execute, util.lookupIndicator("append", 3),
	]);
}

/*

append([], L, L).
append([H|T], R, [H|L]) :-
	append(T, R, L).


append(A1, A2, A3) :-
	A1 = [],
	A2 = A3.
append(A1, A2, A3) :-
	A1 => [X4|X5],
	;;A2 => X7,
	A3 => [X4|X6],
	X5 => A1,
	;;X7 => A2,
	X6 => A3,
	append(A1, A2, A3).

	try_me_else L
	get_constant [], 1
	get_variable 0, 4, 2
	get_value 4, 3
	proceed
L:	trust_me ;; A1 -> [X4|X5], (A2->A2), A3->[X4|X6]
    allocate
	get_list 1
	unify_variable 4
	unify_variable 5
	get_list 3
	unify_value 4
	unify_variable 6
	put_value 0, 5, 1
	put_value 0, 6, 3
	deallocate
	execute append/3
 */

function appendProgram () {
	return util.process_labels([
			util.opCodes.try_me_else, {ref: 'L'},
			util.opCodes.get_constant, util.lookup_atom('[]'), 1,
			util.opCodes.get_variable, 0, 4, 2,
			util.opCodes.get_value, 4, 3,
			util.opCodes.proceed,

			{label: 'L'},
			util.opCodes.trust_me,
			util.opCodes.allocate,
			util.opCodes.get_list, 1,
			util.opCodes.unify_variable, 4,
			util.opCodes.unify_variable, 5,
			util.opCodes.get_list, 3,
			util.opCodes.unify_value, 4,
			util.opCodes.unify_variable, 6,
			util.opCodes.put_value, 0, 5, 1,
			util.opCodes.put_value, 0, 6, 3,
			util.opCodes.deallocate,
			util.opCodes.execute, util.lookupIndicator("append", 3),
		]
	)
}

/*
?- nrev([1,2,...,30], X).

	put_list 1
	set_constant 1
	put_list 3
	set_constant 2
	put_list 4
	...
	set_constant 30
	set_constant []
	set_variable 2
	call nrev/2
	proceed
 */

function query3() {
	return [
		util.opCodes.put_list, 4,
		util.opCodes.set_constant, util.lookup_atom('3'),
		util.opCodes.set_constant, util.lookup_atom('[]'),
		util.opCodes.put_list, 3,
		util.opCodes.set_constant, util.lookup_atom('2'),
		util.opCodes.set_value, 4,
		util.opCodes.put_list, 1,
		util.opCodes.set_constant, util.lookup_atom('1'),
		util.opCodes.set_value, 3,
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2), 0,
		util.opCodes.proceed
	]
}

function queryNull() {
	return [
		util.opCodes.put_constant, util.lookup_atom('[]'), 1,
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2), 0,
		util.opCodes.proceed
	]
}

function query1() {
	return [
		util.opCodes.put_list, 1,
		util.opCodes.set_constant, util.lookup_atom('1'),
		util.opCodes.set_constant, util.lookup_atom('[]'),
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2), 0,
		util.opCodes.proceed
	]
}

function query2() {
	return [
		util.opCodes.put_list, 3,
		util.opCodes.set_constant, util.lookup_atom('2'),
		util.opCodes.set_constant, util.lookup_atom('[]'),
		util.opCodes.put_list, 1,
		util.opCodes.set_constant, util.lookup_atom('1'),
		util.opCodes.set_value, 3,
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2), 0,
		util.opCodes.proceed
	]
}

/*
put_list, 3,
set_constant, N,
set_constant, [],
put_list, 4,
set_constant, N-1,
set_value, 3,
put_list, 3,
set_constant, N-2,
set_value, 4,
...
put_list, 1,
set_constant, 1,
set_value, LAST
set_variable, 2
call nrev/2
proceed

 */

function gen(arg, head, tail, n) {
	let last = (n % 2 === 0) ? head : tail;

	return [util.opCodes.put_list, head,
		util.opCodes.set_constant, util.lookup_atom(n),
		util.opCodes.set_constant, util.lookup_atom("[]")]
		.concat(genBody(tail, head, n-1))
		.concat([util.opCodes.put_list, arg,
		util.opCodes.set_constant, util.lookup_atom(1),
		util.opCodes.set_value, last])
}
function genBody(head, tail, n) {
	if(n > 1) {
		return [util.opCodes.put_list, head,
			util.opCodes.set_constant, util.lookup_atom(n),
			util.opCodes.set_value, tail]
			.concat(genBody(tail, head, n - 1));
	} else {
		return [];
	}
}

function query4() {
	return gen(1, 3, 4, 4)
		.concat([util.opCodes.set_variable, 2,
			util.opCodes.call, util.lookupIndicator("nrev", 2), 0,
			util.opCodes.proceed
		])
}

function query15() {
	return gen(1, 3, 4, 15)
		.concat([util.opCodes.set_variable, 2,
			util.opCodes.call, util.lookupIndicator("nrev", 2), 0,
			util.opCodes.proceed
		])
}

function query30() {
	return gen(1, 3, 4, 30)
		.concat([util.opCodes.set_variable, 2,
			util.opCodes.call, util.lookupIndicator("nrev", 2), 0,
			util.opCodes.proceed
		])
}

module.exports.nrevProgram = nrevProgram;
module.exports.appendProgram = appendProgram;
module.exports.queryNull = queryNull;
module.exports.query1 = query1;
module.exports.query2 = query2;
module.exports.query3 = query3;
module.exports.query4 = query4;
module.exports.query15 = query15;
module.exports.query30 = query30;