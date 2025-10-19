% H + H <--> H2

-language(psifcp).
global(e(10),e1(10)).

System(N1)::= << CREATE_H(N1)  .

		    CREATE_H(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | H | self 
		 >> .


H+electron(0.1)::=  e1 ! {electron} , H_BoundH(electron) ;
		    e1 ? {e2} , H_BoundH(e2) ;
		    e  ! {electron} , H_Bound(electron) . 

H_BoundH(el)::= el ? [] , H ;
	        el ! [] , H .

H_Bound(el)::= el ? [] , H .
	    


	  





	
