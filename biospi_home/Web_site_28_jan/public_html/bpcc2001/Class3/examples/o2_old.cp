% O + O <--> O2


-language(psifcp).
global(eeCO(100),eCOH(37),eHO(10),eeOO(2),breakOH(0.1),breakHOH(0.1),breakOO(0.1)).

System(N1)::= << timer_old#Timer_OO | CREATE_O(N1)  .

		 CREATE_O(C)::= {C =< 0} , true ;
				{C > 0} , {C--} | O | self 
		 >> .

O::= eeOO ? [] , OO ;
     eeOO ! [] , true ;
     eHO  ? [] , OH ;
     eeCO ! [] , true .

OH::= breakOH ? [] , O | h2_old#H ;
      eHO ? [] , H2O ;
      eCOH ! [] , true .

OO::= breakOO ? [] , O | O .

H2O::= breakHOH ? [] , OH | h2_old#H .



	  





	
