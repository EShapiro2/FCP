% O + O <--> O2


-language(psifcp).
global(eeCO(100),eCOH(37),eHO(40),eeOO(50),breakOH(0.25),breakHOH(0.25),breakOO(0.2)).

System(N1)::= << timer_0#Timer_OO | CREATE_O(N1)  .

		 CREATE_O(C)::= {C =< 0} , true ;
				{C > 0} , {C--} | O | self 
		 >> .

O::= eeOO ? [] , OO ;
     eeOO ! [] , true ;
     eHO  ? [] , OH ;
     eeCO ! [] , true .

OH::= breakOH ? [] , O | h2_1#H ;
      eHO ? [] , H2O ;
      eCOH ! [] , true .

OO::= breakOO ? [] , O | O .

H2O::= breakHOH ? [] , OH | h2_1#H .



	  





	
