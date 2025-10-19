% Mg + 2Cl <--> Mg+2 + 2Cl-

-language(psifcp).
global(e1(10),e2(100),e3(50),e4(5)).
export(System).

System(N1,N2)::= << CREATE_Mg(N1) | CREATE_Cl(N2) .

		    CREATE_Mg(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | Mg | self .

		    CREATE_Cl(C)::= {C =< 0} , true ;
		 		    {C > 0} , {C--} | Cl | self 
		 >> .


Mg::= e1 ! [] , Mg_plus .

Mg_plus::= e2 ! [] , Mg_plus2 ;
           e3 ? [] , Mg .

Mg_plus2::= e4 ? [] , Mg_plus .


Cl::= e1 ? [] , Cl_minus ;
      e2 ? [] , Cl_minus .

Cl_minus::= e3 ! [] , Cl ;
	      e4 ! [] , Cl . 





	
