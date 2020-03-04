nrev([],[]).

nrev([X|Rest],Ans):-
	nrev(Rest,L),
	append(L,[X],Ans).
