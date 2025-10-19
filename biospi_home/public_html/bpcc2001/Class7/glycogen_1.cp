
% Glycogen synthesis and branching. 
% Glycogen synthesis (polymerization 1-->4) Glycogen_n + UDP-glucose --> Glycogen_n+1 + UDP (glycogen synthase)
% Glycogen branching (polymerization 1-->6): 7-residue end-segment, from a chain of at least 11 residues, is transfered
% by branching enzyme to a branch point at least 4 residues away from other branch points. 
% 
% 
%
%
% A glycogen fibril grows by directional polymeization, and has a root side and a leaf side (1 --> 4).
%
% A residue may belong to one of three different positions:
%   1. It is on a straight segment (with no brachpoints to the leaf side), where it is either
%		- Polymerization enabled (the leaf: LC=0) (Leaf_Glucose)
%		- Cleavge and branch enabled (at distance 7 from the leaf LC=7; at least 4 residues from the root RC >= 4) (BCE_Glucose)
%		- Branch enabled and not cleavge enabled (LC <> 7; RC >= 4) (BNCE_Glucose)
%		- Disabled (LC > 0 and RC < 4) (Disabled_Glucose)
%	
%   2. It is on a branched segment (there is a brachpoint on the leaf side)
%		- Branch enabled (RC >=4 and LBC >=4) (Enabled_Branched_Glucose) 
%		- Disabled (RC <4 or LBC < 4) (Disabled_Branched_Glucose)	
%
%	Process				LC		RC		LBC
%	
%	Leaf_Glucose			0		N/A		0
%	BCE_Glucose			7		>=4		0
%	BCNE_Glucose			<>7 and >0	>=4		0
%				OR*	N/A		RC >=4		LBC >=4
%	Disabled_Glucose		>0		<4		0
%	Disabled_Branched_Glucose	N/A		RC < 4	 OR	LBC < 4
%
%	* This condition for Enabled_Branched_Glucose
%
%
%   3. It is a branch point (Branch_Glucose)
%
% Three different counters describe the position of a given glucose residue. 
% Two of the counters are used by residues on a straight segments (between a branch point or root and a polymerizing leaf) 
% 	LC - leaf counter: Distance of residue from leaf
%	RC - root counter: Distance of residue from root or root-side branch point
% Two of the counters are used by residues on branched segment (between two barch points or between a root and a brach point)
%	LBC - leaf-branch counter: Distance of residue from leaf-side branch point
%	RC - root counter Distance of residue from root or root-side branch point (as before)
%
% The counters are initialized and updated in the following way
% LC: 	Initialized to zero
%	Updated upon extension +1
%		upon cleavage (in remaining segment residues) -7
% 
% RC:   Initialized to RC of root side neighbor + 1
%	Updated upon cleavage  (in cleaved segment residues) to RC of new root side counter + 1
%	Updated upon insertion (in segment on leaf-side of the new branch point) to RC - (RC of new branch point)
%
% LBR:  Initialized to zero
%	Updated upon intertion (in segment on root-side of the new branch point) to LBC of leaf side neighbor + 1
% All private channels are infinite. All global channels MUST have a rate. Hence, all consequences of a chemical reactionwill occur 
% immediately (updated in zero time). 

-language(psifcp).
global(glycogen(1), udp_glucose(1), dummy(1), branch(1), cleave(1)).
baserate(infinite).

System(N1,N2,N3,N4)::= << CREATE_Seed_Glucose(N1) | CREATE_UDP_Glucose(N2) | CREATE_Glycogen_Synthase(N3) | CREATE_Branching_Enzyme(N4) .

		       	  CREATE_Seed_Glucose(C)+(RC,LC,LBC)::= {C =< 0} , true ;
						 {C > 0} , {C--} | {RC = 0} | {LC = 0} | {LBC = 0} | Seed_Glucose(RC,LC,LBC) | self .

		       	  CREATE_UDP_Glucose(C)+(LC,LBC)::= {C =< 0} , true ;
		 				{C > 0} , {C--} | {LC = 0} | {LBC = 0} |  UDP_Glucose(LC,LBC) | self .

		          CREATE_Glycogen_Synthase(C)::= {C =< 0} , true ;
		 				      {C > 0} , {C--} | Glycogen_Synthase | self .

		          CREATE_Branching_Enzyme(C)::= {C =< 0} , true ;
		 				      {C > 0} , {C--} | Branching_Enzyme | self >> .

Seed_Glucose(RC,LC,LBC)::= 

	glycogen ? {to_leaf} , to_leaf ! {RC,LBC} , Root_Glucose(to_leaf,RC,LC,LBC) . 		
					  
Root_Glucose(to_leaf,RC,LC,LBC)::= to_leaf ? {LC,LBC} , {LC++} , Root_Glucose(to_leaf,RC,LC,LBC) .


UDP_Glucose(LC,LBC)+(to_root,to_leaf)::= 
	udp_glucose ! {to_root} , to_root ? {RC,LBC} , {RC++} , to_root ! {LC,LBC} , Glucose(to_root,to_leaf,RC,LC,LBC) .

Glucose(to_root, to_leaf, RC, LC, LBC)::= 
{LC <0}, screen#display("error - LC<0") ; 
{LC>=0},
	<< {LBC = 0} , << {LC = 0} ,  Leaf_Glucose ;
				      {LC = 7 , RC >= 4} , BCE_Glucose ;
					  {LC > 0 , LC =\= 7 , RC >= 4} , BNCE_Glucose ;
					  {LC > 0 , RC < 4} , Disabled_Glucose >> ;
		{LBC > 0} , << {RC >= 4 , LBC >=4} , BNCE_Glucose ;
		 	           {RC < 4} , Disabled_Branched_Glucose ;
					   {LBC < 4} , Disabled_Branched_Glucose >> .

	Leaf_Glucose::= 
		glycogen ? {to_leaf} , to_leaf ! {RC,LBC} , to_leaf ? {LC,LBC} , {LC++} , to_root ! {LC,LBC} , 
					Glucose(to_root,to_leaf,RC,LC,LBC);
		to_root ? {RC,_} , screen#display(RC) | <<   {RC >=0} , {RC++} , Glucose ;
													 {RC < 0} , Disabled_Leaf_Glucose >> .
	
	Disabled_Leaf_Glucose::= to_root ? {RC,_} , {RC++} , Glucose .

	BNCE_Glucose::= to_leaf ? {LC,LBC} , {LC++} , << {LBC = 0} , to_root ! {LC,LBC} , Glucose ;
						         {LBC > 0} , {LBC++} ,  to_root ! {LC,LBC} , Glucose >> ;
			to_root ? {RC,_} , <<   {RC >=0} , {RC++} , to_leaf ! {RC,LBC} , Glucose ;
						{RC < 0} , to_leaf ! {RC, LBC} , Disabled_Glucose >> ;
			branch ? {to_branch} , Branch_Synch1(to_branch,RC,LC,LBC) .   
				      
	Branch_Synch1(to_branch,RC,LC,LBC)+(RC1,LBC1)::=  {RC1=0} | {LBC1=1} | << to_branch ! {RC1,LBC} ,  
							  screen#display(RC1) | << to_leaf ! {RC1,LBC} , 
							  to_root ! {LC,LBC1} , Branch_Point(to_root,to_branch,to_leaf) >> >>  . 


	Disabled_Glucose::= to_leaf ? {LC,LBC} , {LC++} , << {LBC = 0} , to_root ! {LC,LBC} , Glucose ;
						             {LBC > 0} , {LBC++} ,  to_root ! {LC,LBC} , Glucose >> ;
						to_root ? {RC,_} , {RC++} , to_leaf ! {RC,LBC} , Glucose .

	BCE_Glucose+(new_to_root,RC1,LC1,LBC1)::=  

		<< to_leaf ? {LC,LBC} , {LC++} , << {LBC = 0} , to_root ! {LC,LBC} , Glucose ;
		  	         		    {LBC > 0} , {LBC++} ,  to_root ! {LC,LBC} , Glucose >> ;
		   to_root ? {RC,_} , << {RC >=0} , {RC++} , to_leaf ! {RC,LBC} , Glucose ;
					   {RC < 0} , to_leaf ! {RC,LBC} , Disabled_Glucose >> ;
		   branch ? {to_branch} , Branch_Synch(to_branch,RC,LC,LBC) ; 
		   cleave ! {new_to_root} , {LC1 = -1} | {RC1 = -1} | Cleave_Synch(to_leaf) .
		
		   Cleave_Synch(to_leaf)::= to_root ! {LC1,LBC} , to_leaf ! {RC1, LBC} , new_to_root ? {RC,_} , 
				   	    {RC++} , to_leaf ! {RC,LBC} , Glucose(new_to_root,to_leaf, RC, LC, LBC) >> .

 	Branch_Synch(to_branch,RC,LC,LBC)+(RC1,LBC1)::=  {RC1=0} | {LBC1=1} | << to_branch ! {RC1,LBC} ,  
							 screen#display(RC1) | << to_leaf ! {RC1,LBC} , 
							 to_root ! {LC,LBC1} , Branch_Point(to_root,to_branch,to_leaf) >> >> .

	Disabled_Branched_Glucose::= to_leaf ? {LC,LBC} , {LC++} , << {LBC = 0} , to_root ! {LC,LBC} , Glucose ;
						         	      {LBC > 0} , {LBC++} ,  to_root ! {LC,LBC} , Glucose >> ;
				     to_root ? {RC,_} , {RC++} , to_leaf ! {RC,LBC} , Glucose >> . 

Branch_Point(to_root,to_branch,to_leaf)::= to_root ? {_,_} , self ;
					   to_branch ? {_,_} , self ;
					   to_leaf ? {_,_} , self .

Glycogen_Synthase::= udp_glucose ? {to_root} , glycogen ! {to_root} , Glycogen_Synthase  .

Branching_Enzyme::= cleave ? {to_branch} , branch ! {to_branch} , Branching_Enzyme .