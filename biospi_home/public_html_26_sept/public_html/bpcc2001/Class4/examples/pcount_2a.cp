-language(psifcp).
global(dummy, alkene_Y(1), alkene_R(10), poly_R(1)).
baserate(1).

System(N1,N2) + RC ::= << {RC = 0} |CREATE_Y(N1) | CREATE_Ethylene(N2) .

CREATE_Y(C)::= 	{C =< 0} , true ;
			{C > 0} , {C--} | Y | self .

CREATE_Ethylene(C)::= 	{C =< 0} , true ;
		 		{C > 0} , {C--} | Ethylene | self .


Y+polyCC(infinite)::= alkene_Y ! {polyCC} , Y_bound(polyCC,RC) .

Y_bound(polyCC,RC)::= polyCC ! {RC} , dummy ? [] , true  . 

Ethylene::=   alkene_Y ? {polyCC_left} , EthylR(polyCC_left)  ;
	    	  alkene_R ? {polyCC_left} , EthylR(polyCC_left)  .

EthylR(polyCC_left)+polyCC_right(infinite)::= 
   alkene_R ! {polyCC_right} , EthylP(polyCC_left, polyCC_right) ;
   poly_R ! {polyCC_right} , EthylP_term(polyCC_left, polyCC_right) ;
   poly_R ? {polyCC_right} , EthylP_term(polyCC_left, polyCC_right) .

EthylP(polyCC_left, polyCC_right)::= 
   polyCC_left ? {RC}, {RC++}, polyCC_right ! {RC} ,  EthylP_Count(RC) .

EthylP_Count(RC)::= dummy ? [] , true . 

EthylP_term(polyCC_left, polyCC_right)::= 
   polyCC_left ? {RC} ,  {RC++} , EthylP_term_count(RC) .

EthylP_term_count(RC)::= dummy ? [] , true 

>> .


