% this piece of code serves to spawn off N1 "K processes" and N2 "F processes".
% Incorporate it into your program as one of the processes
System(N1,N2)::= << CREATE_K(N1) | CREATE_F(N2) .
		    CREATE_K(C)::= {C =< 0} , true ;
				       {C > 0} , {C--} | K | self .
		    CREATE_F(C)::= {C =< 0} , true ;
		 		       {C > 0} , {C--} | F | self 
		 >> .
