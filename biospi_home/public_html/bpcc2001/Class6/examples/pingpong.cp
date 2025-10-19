-language(psifcp).
global(e_s1_bind(20), f_s2_bind(20), es1_unbind(0.8), fs2_unbind(0.8), es1_react(0.1), fs2_react(1), dummy(1)).

System(N1,N2,N3)::= << 
 CREATE_E(N1) | CREATE_S1(N2) | CREATE_S2(N3) |
 timer#Timer(es1_unbind) | timer#Timer(fs2_unbind) |
 timer#Timer(fs2_react) | timer#Timer(es1_react) .
  
  CREATE_E(C)::= {C =< 0} , true ;
		 	    {C > 0} , {C--} | E | self .

  CREATE_S1(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S1 | self  .

  CREATE_S2(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S2 | self  
		 >> .

E::= e_s1_bind ! [] , E_bound_S1 .
 

E_bound_S1::= es1_unbind ? [] , E | S1 ;
		  es1_react ? [] , F | P1 .

F::= f_s2_bind ! [] , F_bound_S2 .

F_bound_S2::= fs2_unbind ? [] , F | S2 ;
		  fs2_react ? [] , E | P2 .

S1::= e_s1_bind ? [] , true .

S2::= f_s2_bind ? [] , true .

P1::= dummy ? [] , true .

P2::= dummy ? [] , true .
