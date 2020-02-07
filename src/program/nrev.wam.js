const util = require('./utilities.wam.js');
/*
nrev([],[]).

nrev([X|Rest],Ans):-
	nrev(Rest,L),
	append(L,[X],Ans).

	try_me_else L
	get_constant []
	get_constant []
	proceed
L:	trust_me ;; A1 -> [X4|X3], A2 -> Y2. X4 -> Y1, X3 -> A1, Y3 -> A2. Y1 -> X4, Y3 -> A1, [X4] -> A2, Y2 -> A3.
	allocate 3			// nrev
	put_variable 1, 1, 4,//   Y1 <==> X4
	get_list 1			//     ([
	unify_value 4		//		 X
	unify_variable 3	//		  |Rest],
	get_variable 1, 2, 2//               Ans) :-
	put_value 0, 3, 1,  //    nrev(Rest,
	put_variable 1, 3, 2,//             L
	call nrev/2,		 //	             ),
	put_value 1, 1, 4    //   append(...X4  <= Y1
	put_value 1, 3, 1,   //          L,
	put_list 2,  		//	           [
	set_value 4       	//              X...X4
	set_constant []   	//               ],
	put_value 1, 2, 3   //                 Ans
	call append/3       //                    )
	deallocate          //                     .
*/

function nrevProgram () {
	return util.process_labels([
		util.opCodes.try_me_else, {ref: 'L'},                   // try_me_else, L1
		util.opCodes.get_constant, util.lookup_atom("[]"), 1,
		util.opCodes.get_constant, util.lookup_atom("[]"), 2,
		util.opCodes.proceed,
		{label: 'L'},
		util.opCodes.trust_me,
		util.opCodes.allocate, 3,
		util.opCodes.put_variable, 1, 1, 4,
		util.opCodes.get_list, 1,
		util.opCodes.unify_value, 4,
		util.opCodes.unify_variable, 3,
		util.opCodes.get_variable, 1, 2, 2,
		util.opCodes.put_value, 0, 3, 1,
		util.opCodes.put_variable, 1, 3, 2,
		util.opCodes.call, util.lookupIndicator("nrev",2),
		util.opCodes.put_value, 1, 1, 4,
		util.opCodes.put_value, 1, 3, 1,
		util.opCodes.put_list, 2,
		util.opCodes.set_value, 4,
		util.opCodes.set_constant, util.lookup_atom("[]"),
		util.opCodes.put_value, 1, 2, 3,
		util.opCodes.call, util.lookupIndicator("append", 3),
		util.opCodes.deallocate
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
	get_list 1
	unify_variable 4
	unify_variable 5
	get_list 3
	unify_value 4
	unify_variable 6
	put_value 0, 5, 1
	put_value 0, 6, 3
	call append/3
	proceed

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
			util.opCodes.allocate, 0,
			util.opCodes.get_list, 1,
			util.opCodes.unify_variable, 4,
			util.opCodes.unify_variable, 5,
			util.opCodes.get_list, 3,
			util.opCodes.unify_value, 4,
			util.opCodes.unify_variable, 6,
			util.opCodes.put_value, 0, 5, 1,
			util.opCodes.put_value, 0, 6, 3,
			util.opCodes.call, util.lookupIndicator("append", 3),
			util.opCodes.deallocate
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
		util.opCodes.call, util.lookupIndicator('nrev', 2),
		util.opCodes.proceed
	]
}

function queryNull() {
	return [
		util.opCodes.put_constant, util.lookup_atom('[]'), 1,
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2),
		util.opCodes.proceed
	]
}

function query1() {
	return [
		util.opCodes.put_list, 1,
		util.opCodes.set_constant, util.lookup_atom('1'),
		util.opCodes.set_constant, util.lookup_atom('[]'),
		util.opCodes.set_variable, 2,
		util.opCodes.call, util.lookupIndicator('nrev', 2),
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
		util.opCodes.call, util.lookupIndicator('nrev', 2),
		util.opCodes.proceed
	]
}

module.exports.nrevProgram = nrevProgram;
module.exports.appendProgram = appendProgram;
module.exports.queryNull = queryNull;
module.exports.query1 = query1;
module.exports.query2 = query2;
module.exports.query3 = query3;
