-language(psifcp).
global(e1(90),e2(10),dummy).
export(System).

System(N1,N2,N3,N4)+(e3(infinite),e4(infinite))::= 
<< Na | Cl | Na_plus | Cl_minus .

Na::=  e1 ! N1 * [] , {N1--} , e3 ! [] ,  screen#display(counter=N1,append(count_txt)) | Na ;
       e3 ? [] , {N1++} ,  Na .

Na_plus::= e2 ? N3 * [] , {N3--} , e3 ! [] , Na_plus ;
	     e3 ? [] , {N3++} , Na_plus .

Cl::= e1 ? N2 * [] , {N2--} , e4 ! [] , Cl ;
      e4 ? [] , {N2++} , Cl .

Cl_minus::= e2 ! N4 * [] , {N4--} , e4 ! [] , Cl_minus;
            e4 ? [] , {N4++} , Cl_minus >> .
	
