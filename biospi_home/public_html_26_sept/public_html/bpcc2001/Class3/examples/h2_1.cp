% H + H <--> H2
% Symmetric covalent bond: mixed choice on the same channel ("homo dimerization") with a different type of rate calculation. The need for the additional e channel is the limitation of our systems, which does not allow the same channel to be used in symmetric and asymmetric interactions.


-language(psifcp).
global(eCH(33),eHO(40),eHH(40),breakHH(0.25)).


System(N1)::= << timer_0#Timer_HH | CREATE_H(N1)  .

		 CREATE_H(C)::= {C =< 0} , true ;
				{C > 0} , {C--} | H | self 
		 >> .

H::=  eHH ? [] , HH ;
      eHH ! [] , true ;
      eHO ! [] , true ;
      eCH ! [] , true .

HH::= breakHH ? [] , H | H .
    


	  





	
