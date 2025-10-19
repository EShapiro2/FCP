-language(psifcp).
global(hydroxyl_P(1)).

System(N1,N2)::= << CREATE_Nucleotide(N1) | CREATE_Seed_Nucleotide(N2).

		    CREATE_Nucleotide(C)::= {C =< 0} , true ;
		        		    {C > 0} , {C--} | Nucleotide | self .

		    CREATE_Seed_Nucleotide(C)::= {C =< 0} , true ;
		             			 {C > 0} , {C--} | Seed_Nucleotide | self >> .

Seed_Nucleotide::=  << 	hydroxyl_P ? {pd_ester} , Seed_Bound(pd_ester) .

		    	Seed_Bound(pd_ester)::= pd_ester ! [] , Seed_Nucleotide >> .

Nucleotide+pde(0.001)::= <<  hydroxyl_P ! {pde} , Nucleotide_5_Bound(pde) .
		      
		      	   Nucleotide_5_Bound(pde)::= pde ? [] , Nucleotide ;
					              hydroxyl_P ? {pd_ester} , Nucleotide_5_3_Bound(pde,pd_ester) .

		      	   Nucleotide_3_Bound(pd_ester)::= pd_ester ! [] , Nucleotide  .

		      	   Nucleotide_5_3_Bound(pde,pd_ester)::=
						 	   pde ? [] , Nucleotide_3_Bound(pd_ester) ;
						 	   pd_ester ! [] , Nucleotide_5_Bound(pde) >> .



