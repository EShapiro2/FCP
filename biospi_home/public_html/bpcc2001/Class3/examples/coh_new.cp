% C + O + H <--> CO2 + CH4 + HCOOH + H2 + O2 + H2O
% Combinatorial explosion in the specification. To complete need to add break in intermediate

-language(psifcp).
global(eCH(33),eeCO(100),eCOH(37),breakOO(0.1),breakCO(0.1),breakCOH(0.3),breakCH(0.37)).
export(System).

System(N1)::= << timer_0#Timer_CO | timer_0#Timer_COH | timer_0#Timer_CH | CREATE_C(N1)  .

		 CREATE_C(C)::= {C =< 0} , true ;
				{C > 0} , {C--} | C | self 
		 >> .

C::= eCH  ? [] , CH  ;
     eeCO ? [] , CO  ;
     eCOH ? [] , COH . 

CH::= eCH  ? [] , CH2 ;
      eeCO ? [] , HCO  ;
      eCOH ? [] , HCOH . 

CO::= eCH  ? [] , HCO ;
      eeCO ? [] , CO2  ;
      eCOH ? [] , COOH . 

COH::= eCH  ? [] , HCOH ;
       eeCO ? [] , COOH  ;
       eCOH ? [] , COHOH . 

CH2::= eCH  ? [] , CH3 ;
       eeCO ? [] , H2CO  ;
       eCOH ? [] , H2COH . 


HCO::= eCH  ? [] , H2CO ;
       eCOH ? [] , COHOH . 

HCOH::= eCH  ? [] , H2COH ;
	eeCO ? [] , HCOOH  ;
        eCOH ? [] , HCOHOH . 

CO2::= breakCO ? [] , CO | o2_2#O .

COOH::= eCH ? [] , HCOOH ;
	eCOH ? [] , COOHOH .

COHOH::= eCH  ? [] , HCOHOH ;
         eeCO ? [] , COOHOH  ;
         eCOH ? [] , COHOHOH . 

CH3::= eCH  ? [] , CH4 ;
       eCOH ? [] , H3COH . 
	  
H2CO::= breakCH ? [] , HCO | h2_1#H ;
	breakCO ? [] , CH2 | o2_2#O .

H2COH::= eCH ? [] , H3COH ;
	 eCOH ? [] , H2COHOH .

HCOHOH::= eCH ? [] , H2COHOH ;
	  eCOH ? [] , HCOHOHOH .

COOHOH::= breakCO ? [] , COHOH | o2_2#O ;
	  breakCOH ? [] , COOH | o2_2#OH .
	
COHOHOH::=  eCH ? [] , HCOHOHOH ;
	    eCOH ? [] , COHOHOHOH .

CH4::= breakCH ? [] , CH3 | h2_1#H .

H3COH::= breakCH ? [] , H2COH | h2_1#H ;
         breakCOH ? [] , CH3 | o2_2#OH . 

H2COHOH::=  breakCH ? [] , HCOHOH | h2_1#H ;
            breakCOH ? [] , H2COH | o2_2#OH . 

HCOHOHOH::=  breakCH ? [] , COHOHOH | h2_1#H ;
             breakCOH ? [] , HCOHOH | o2_2#OH . 
 
COHOHOHOH::= breakCOH ? [] , COHOHOH | o2_2#OH .

HCOOH::= breakCH ? [] , COOH | h2_1#H ;
	 breakCO ? [] , HCOH | o2_2#O ;
         breakCOH ? [] , HCO | o2_2#OH .













