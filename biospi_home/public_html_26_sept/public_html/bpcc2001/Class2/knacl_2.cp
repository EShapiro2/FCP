% K + Na + 2Cl <--> K+ + Na+ + 2Cl- (~)

-language(psifcp).
global(e1(100),e2(10),e3(30),e4(20)).

System(N1,N2,N3)::= << CREATE_Na(N1) | CREATE_Cl(N2) | CREATE_K(N3).

		    CREATE_Na(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | Na | self .

		    CREATE_Cl(C)::= {C =< 0} , true ;
		 		    {C > 0} , {C--} | Cl | self .

		    CREATE_K(C)::= {C =< 0} , true ;
		 		   {C > 0} , {C--} | K | self 
		  >> .


Na::= e1 ! [] , Na_plus .

Na_plus::= e2 ? [] , Na .

K::= e3 ! [] , K_plus .

K_plus::= e4 ? [] , K . 

Cl::= e1 ? [] , Cl_minus ;
      e3 ? [] , Cl_minus .

Cl_minus::= e2 ! [] , Cl ;
	      e4 ! [] , Cl .





	
