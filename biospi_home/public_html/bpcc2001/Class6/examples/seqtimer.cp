-language(psifcp).
global(e_s1_bind(20), es1_s2_bind(20), es1_unbind(0.8), s1s2_react(0.1), es1_s2_unbind(0.8), ep2_unbind(1), dummy(1)).

System(N1,N2,N3)::= << CREATE_E(N1) | 
			     CREATE_S1(N2) |
			     CREATE_S2(N3) |
			     timer#Timer(es1_unbind) | timer#Timer(es1_s2_unbind) |
                       timer#Timer(s1s2_react) | timer#Timer(ep2_unbind) .

  CREATE_E(C)::= {C =< 0} , true ;
		 	    {C > 0} , {C--} | E | self .

  CREATE_S1(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S1 | self  .

  CREATE_S2(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S2 | self  
		 >> .

E::= e_s1_bind ! [] , E_bound_S1 .

E_bound_S1::= es1_unbind ? [] , E | S1 ;
		  es1_s2_bind ! [] , E_bound_S1_S2 .

E_bound_S1_S2::= es1_s2_unbind ? [] , E_bound_S1 | S2;
		     s1s2_react ? [] , E_Bound_P2 | P1 .

E_Bound_P2::= ep2_unbind ? [] , E | P2 .

S1::= e_s1_bind ? [] , true .

S2::= es1_s2_bind ? [] , true .

P1::= dummy ? [] , true .

P2::= dummy ? [] , true .
