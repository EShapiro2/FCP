-language(psifcp).
global(e_s1_bind(20), es1_s2_bind(20), dummy(1)).

System(N1,N2,N3)::= << CREATE_E(N1) | 
			  CREATE_S1(N2) |
			  CREATE_S2(N3) .

  CREATE_E(C)::= {C =< 0} , true ;
		 	    {C > 0} , {C--} | E | self .

  CREATE_S1(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S(e_s1_bind) | self  .

  CREATE_S2(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | S(es1_s2_bind) | self  
		 >> .


E+(es1_unbind(0.8), s1s2_react(0.1))::=
	<< e_s1_bind ! {es1_unbind, s1s2_react} , E_bound_S1 .

         E_bound_S1+(es1_s2_unbind(0.8),ep2_unbind(1))::= 
			<< es1_unbind ? [] , E ;
			   es1_s2_bind ! {es1_s2_unbind, ep2_unbind} , 
				  E_bound_S1_S2 .

	              E_bound_S1_S2::= es1_s2_unbind ? [] , E_bound_S1 ;
				             s1s2_react ? [] , E_Bound_P2 .

	              E_Bound_P2::= ep2_unbind ? [] , E >> >> .

S(bind)::= bind ? {unbind,react} , Bound_S(bind,unbind,react).

Bound_S(bind,unbind,react)::= unbind ! [] , S(bind);
                         	react ! [] , P .

P::= dummy ? [] , true .

