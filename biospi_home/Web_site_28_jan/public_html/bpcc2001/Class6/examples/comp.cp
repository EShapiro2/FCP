-language(psifcp).
global(e_s_bind(20),es_unbind(0.8),e_i_bind(20),ei_unbind(0.8),es_react(0.1), e_p_bind(10)).

System(N1,N2,N3)::= << CREATE_ENZYME(N1) | 
			  CREATE_SUBSTRATE(N2) |
			  CREATE_INHIBITOR(N3) |
			  timer#Timer(es_unbind) |
			  timer#Timer(es_react) |
   			  timer#Timer(ei_unbind) .

  CREATE_ENZYME(C)::= {C =< 0} , true ;
		 	    {C > 0} , {C--} | Enzyme | self .

  CREATE_SUBSTRATE(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | Substrate | self .

  CREATE_INHIBITOR(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | Inhibitor | self 
		 >> .

Enzyme::= e_s_bind ! [] , ES ;
	    e_i_bind ! [] , EI .

ES::= es_unbind ? [] , Enzyme | Substrate ;
	es_react ? [] , Enzyme | Product .

EI::= ei_unbind ? [] , Enzyme | Inhibitor .

Substrate::= e_s_bind ? [] , true . 

Product::= e_p_bind ? [] , true .

Inhibitor::= e_i_bind ? [] , true . 
