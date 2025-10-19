% RCOOH + NH2R' <--> RCONHR' +H2O 
% Condensation and hydrolysis between carboxylic and aminic groups (creation of peptide bond).

-language(psifcp).
global(amine(10),hydrolysis(1)).
baserate(infinite).
%export(SystemC).

System(N1,N2)::= CREATE_R_Amine(N1) | CREATE_R_Carboxyl(N2) .

CREATE_R_Amine(C)::= {C =< 0} , true ;
		     {C > 0} , {C--} | R_Amine | self .

CREATE_R_Carboxyl(C)::= {C =< 0} , true ;
			{C > 0} , {C--} | R_Carboxyl | self .

R_Amine+eRN::= NH2(eRN) | R(eRN) .

R_Carboxyl+eRC::= R(eRC) | COOH(eRC) .

NH2(eRN)::= amine ? {eRC} , Amide(eRN,eRC) | H2O .

Amide(eRN,eRC)::= hydrolysis ? [] , COOH(eRC) |  NH2(eRN) .

R(e)::= e ! [] , self .

COOH(eRC)::= amine ! {eRC} , true .

H2O::= hydrolysis ! [] , true .
