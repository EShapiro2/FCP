% O + O <--> On

-language(psifcp).
global(e1(100),e2(100)).
export(System).

System(N1)::= << CREATE_O(N1)  .

		    CREATE_O(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | O | self 
		 >> .

O+electron1(10)::= 
   e1 ! {electron1} , O_Bound1(electron1) ; 
   e1 ? {electron1} , O_Bound1(electron1) . 

O_Bound1(el1)+electron2(0.1)::= 
   el1 ! [] , O ;
   el1 ? [] , O ; 
   e2 ! {electron2} , O_Bound2(el1,electron2) ; 
   e2 ? {electron2} , O_Bound2(el1,electron2) .

O_Bound2(el1,el2)::= 
   el2 ! [] , O_Bound1(el1) ; 
   el2 ? [] , O_Bound1(el1) .

	  





	
