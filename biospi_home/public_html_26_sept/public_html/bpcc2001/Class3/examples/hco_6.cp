% H + O <--> H2 + O2 + H2O
% Composition of separately defined atoms, module composition, multi module run. 


-language(psifcp).

SystemCHO1(N1,N2,N3)::= h2o_3#SystemHO(N2,N3) | coh_4#System(N1) .

SystemCHO2(N1,N2,N3)::= h2o_3#SystemHO(N2,N3) | coh_5#System(N1) .


  





	
