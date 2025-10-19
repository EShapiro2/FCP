-language(psifcp).
global(dummy, alkene_Y(1), alkene_R(10), poly_R(1)).
baserate(1).

System(N1,N2)::= CREATE_Y(N1) | CREATE_Ethylene(N2) .

CREATE_Y(C)::= 	{C =< 0} , true ;
		{C > 0} , {C--} | Y | self .

CREATE_Ethylene(C)::= 	{C =< 0} , true ;
		 	{C > 0} , {C--} | Ethylene | self .


Y+polyCC::= alkene_Y ! {polyCC} , Y_bound(polyCC) .

Y_bound(polyCC)::= dummy ? [] , true . 

Ethylene::= alkene_Y ? {polyCC_left} , EthylR(polyCC_left)  ;
	    alkene_R ? {polyCC_left} , EthylR(polyCC_left)  .

EthylR(polyCC_left)+polyCC_right::= alkene_R ! {polyCC_right} , EthylP(polyCC_left, polyCC_right) ;
				    poly_R ! {polyCC_right} , EthylP_term(polyCC_left, polyCC_right) ;
				    poly_R ? {polyCC_r} , EthylP_term(polyCC_left, polyCC_r) .

EthylP(polyCC_left, polyCC_right)::= dummy ? [] , true . 

EthylP_term(polyCC_left, polyCC_right)::= dummy ? [] , true .


