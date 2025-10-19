% H + Cl <--> HCl


-language(psifcp).
global(e1(100)).
export(System).

System(N1,N2)::= << CREATE_H(N1) | CREATE_Cl(N2) .

		    CREATE_H(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | H | self .

		    CREATE_Cl(C)::= {C =< 0} , true ;
		 		    {C > 0} , {C--} | Cl | self 
		 >> .


H+electron(10)::= 
	e1 ! {electron} , H_plus(electron). 

H_plus(e)::= e ? [] , H .

Cl::= e1 ? {electron} , Cl_minus(electron). 

Cl_minus(e)::= e ! [] , Cl .	 
	  





	
