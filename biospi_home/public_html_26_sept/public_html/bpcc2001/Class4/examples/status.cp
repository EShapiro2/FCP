-language(compound).
-export([now/1]).

now(Now):- psi_monitor#status(Status),extract(Status, now, Now) .

extract(S,N,V):-
	S ? N(VV) :
	  S' = _ ,
        V = VV ;

 	S ? X ,
      X =\= N(_) | 
		self ; 

	S =?= [] :
	   N = _ ,
	   V = [] .