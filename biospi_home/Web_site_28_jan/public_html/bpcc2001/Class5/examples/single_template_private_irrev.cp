-language(psifcp).
global(e_s_bind(20), dummy(1)).

System(N1,N2)::= << CREATE_ENZYME(N1) | 
			  CREATE_SUBSTRATE(N2) .

  CREATE_ENZYME(C)::= {C =< 0} , true ;
		 	    {C > 0} , {C--} | Enzyme | self .

  CREATE_SUBSTRATE(C)::= {C =< 0} , true ;
		 	       {C > 0} , {C--} | Substrate | self 
		 >> .

Enzyme+(es_unbind(0.8),es_react(0.1))::= 
	<< e_s_bind ! {es_unbind,es_react} , ES .

	   ES::= es_unbind ? [] , Enzyme ;
	         es_react ? [] , Enzyme 
      >> .

Substrate::= e_s_bind ? {unbind,react} , Bound_S(unbind,react).

Product::= dummy ? [] , true .

Bound_S(unbind,react)::= unbind ! [] , Substrate ;
                         react ! [] , Product .

