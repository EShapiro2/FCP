-mode(trust).
-language(compound).

test(B):-id(D,4),par(D,D,B).



par(zero,_,zero).
par(_,zero,zero).
par(one,one,one).
par(S,empty,empty):-S!=zero.
par(empty,S,empty):- S!=zero.

par(A,B,Z):-A=plas(A0,A1),Z=plas(Z0,Z1)| par1(A0,A1,B,Z0,Z1).
	
par1(A0,A1,B,Z0,Z1)	:-
	A0=zero | Z0=zero;
	A1=zero | Z1=zero;
	A0=plas(A00,A01),A1=plas(A01,A11) | 
		par(B,A00,Z00),
		par(B,A01,Z01),Z0=plas(Z00,Z01),
		par(B,A10,Z10),
		par(B,A11,Z11),Z1=plas(Z10,Z11).
		
id(Q,n):- plas(plas(E,zero),plas(zero,E))=Q^|id1(E,n).
id1(E,n):-
	n=0|E=empty;
	n>0|n1:=n-1,id(E,n1).
			









/*
or1(X,Y,Z):-
			X=zero | Z=Y;
			Y=zero | Z=X;
			X=one | Z=one;
			Y=one | Z=one;
			X=[X0|X1],Y=[Y0|Y1] | or1(X0,Y0,Z0),or1(X1,Y1,Z1),Z=[Z0|Z1].
			*/