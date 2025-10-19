-language(colon).
% (2) Hamming's Problem
% hamming(Xs) :- Xs is the sorted list of numbers of
% the form (2**i)*(3**j)*(5**k)

hamming(Xs) :-
	multiply(2, [1|Xs?], X2),
	multiply(3, [1|Xs?], X3),
	multiply(5, [1|Xs?], X5),
	opmerge(X2?, X3?, X23),
	opmerge(X5?, X23?, Xs).

multiply(N, [U|X], [V?|Z?]) :-
V := N*U, multiply(N, X?, Z).

opmerge([U|X], [U|Y], [U|Z]) :- opmerge(X?, Y?, Z).
opmerge([U|X], [V|Y], [U|Z]) :- U < V | opmerge(X?, [V|Y], Z).
opmerge([U|X], [V|Y], [V|Z]) :- V < U | opmerge([U|X], Y?, Z).
