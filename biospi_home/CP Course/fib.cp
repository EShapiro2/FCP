-language(colon).

% fib(Xs) :- Xs is the Fibonacci sequence.
     
fib(Xs) :-
	fib(0,1,Xs).
     
fib(N1,N2,[N1|Ns]^) :-
	fib(N2,N3,Ns),
	N3:=N1+N2.
