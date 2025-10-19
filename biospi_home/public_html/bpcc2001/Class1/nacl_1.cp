
% Na + Cl <--> Na+ + Cl-


-language(psifcp).
global(e1(100),e2(10)).

System(N1,N2)::= << CREATE_Na(N1) | CREATE_Cl(N2) .

		    CREATE_Na(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | Na | self .

		    CREATE_Cl(C)::= {C =< 0} , true ;
		 		    {C > 0} , {C--} | Cl | self 
		 >> .


Na::= e1 ! [] , Na_plus .

Na_plus::= e2 ? [] , Na .

Cl::= e1 ? [] , Cl_minus .

Cl_minus::= e2 ! [] , Cl .
	
