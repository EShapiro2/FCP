-language(psifcp).
global(inside,outside).
baserate(1).

System(N)::= Membrane | CREATE_H_plus(N) .

CREATE_H_plus(C)::= {C =< 0} , true ;
		    {C > 0} , {C--} | H_plus(outside) | self .

Membrane::= inside ! {outside} , Membrane ;
	    outside ! {inside} , Membrane .


H_plus(location)::= location ? {new_location} , H_plus(new_location) .

