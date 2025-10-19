-language(psifcp).
global(abp(1),tbp(1),dummy(1)).
baserate(infinite).

System(C1,C2,C3)+(to_ras(1),to_das(1),to_d5,to_d3,bp)::= 
  << Pol(to_ras,to_das) | D_Seed(abp,to_d3,bp) | Seed_RAS(abp,bp,to_ras) |  DAS(tbp,to_d3,to_d5,to_das) | 
     Create_Polymer(C1,to_d5) | Create_Ar(C2) | Create_Ur(C2) .

     Create_Polymer(C,to_d3)::= 
       {C =< 0} , true ;
       {C > 0} , {C--} | << to_d5a , to_d5b  . D_Nuc(abp,to_d3,to_d5a) |  
                                               D_Nuc(tbp,to_d5a,to_d5b) | 
                                               Create_Polymer(C,to_d5b) >> .

    Create_Ar(C)::= {C =< 0} , true ;
                    {C > 0} , {C--} | R_Nuc(tbp) | self.

    Create_Ur(C)::= {C =< 0} , true ;
                    {C > 0} , {C--} | R_Nuc(abp) | self

   >> .   


Pol(to_ras,to_das)::= to_ras ? {to_r5} ,  to_das ! {to_r5,to_ras} , Pol .


D_Seed(base,to_d5,bp)::= dummy ? [] , true .

D_Nuc(base,to_d3,to_d5)::= to_d3 ? {to_pol} , DAS(base,to_d3,to_d5,to_pol) .

DAS(base,to_d3,to_d5,to_pol)+bp::= base ! {bp,to_pol} , bp ? [] , to_d5 ! {to_pol} , D_Bound(base,to_d3,to_d5,bp).

D_Bound(base,to_d3,to_d5,bp)::= dummy ? [] , true .


Seed_RAS(base,bp,to_pol)+to_r3::= to_pol ! {to_r3} , Seed_R_Pol(base,bp,to_r3).

R_Nuc(base)::= base ? {bp,to_pol} , to_pol ? {to_r5,to_ras} , bp ! [] , RAS(base,bp,to_ras,to_r5) .

RAS(base,bp,to_pol,to_r5)+to_r3::= to_pol ! {to_r3} , R_Pol(base,bp,to_r5,to_r3) .  

Seed_R_Pol(base,bp,to_r3)::= dummy ? [] , true .

R_Pol(base,bp,to_r5,to_r3)::= dummy ? [] , true . 

    



