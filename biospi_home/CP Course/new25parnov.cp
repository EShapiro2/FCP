-mode(trust).
-language(compound).
/*
test(B):-E=empty,plas(plas(E,zero),plas(zero,E))=B.



id(Q,n):-

	n==0|Q=empty;
	n>0|n:=n-1, plas(plas(E,zero),plas(zero,E))=Q,id(E,n1).

	*/