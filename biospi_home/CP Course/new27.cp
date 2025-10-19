-mode(trust).
-language(compound).

or(X,Y,Z):-
			X=zero : Z=Y;
			Y=zero : Z=X;
			X=one : Z=one;
			Y=one : Z=one;
			X=cons(X0,X1),Y=cons(Y0,Y1) : or(X0,Y0,Z0),or(X1,Y1,Z1),Z=cons(Z0,Z1).
			
			.