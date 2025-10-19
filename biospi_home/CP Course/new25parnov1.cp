-mode(trust).
-language(compound).


id1(N1,Q1):-
	N1==0|Q1=empty;
	N1>0|N2=N1-1,id1(N2,E1),ty(dot,E1)=Q1.

/*
id(Q,N):-
	N==0|Q=empty:
	N>0|N:=N-1,id(E,N),plas(plas(E,zero),plas(zero,E))=Q.
	*/
