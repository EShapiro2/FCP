% O + O <--> O2

-language(psifcp).
global(e(10),ee(2)).

System(N1)::= << CREATE_O(N1)  .

		    CREATE_O(C)::= {C =< 0} , true ;
				    {C > 0} , {C--} | O | self 
		 >> .



O+electron(0.1)::=  ee ! {electron} , O_Double_Bound(electron) ;
		    ee ? {electron} , O_Double_Bound(electron) ;
		    e  ? {electron} , O_Bound1(electron) .
O_Double_Bound(el)::= el ! [] , O ;
                      el ? [] , O .

O_Bound1(el)::= el ! [] , O ;
		    e ? {electron1}, O_Bound2(el,electron1) .

O_Bound2(el,electron1)::= electron1 ! [] , O_Bound1(el) ;
				  el ! [] , O_Bound1(electron1) .



	  





	
