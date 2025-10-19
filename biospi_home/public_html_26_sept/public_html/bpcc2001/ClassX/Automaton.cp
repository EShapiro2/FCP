-language(psifcp).

global(dunny) .

%Start the automaton , state = S0 , letter is empty , Input is user variable.
Automaton(Input) ::= { State = "S0" } | {Symbol = "" } | Extract .

%Cut the head of the input and store it in input.
%Input is the rest of the old input
%State doesn't change.
Extract(State,Input) ::= { string_to_dlist( Input, [Character | Tail], []) } |
                	 { list_to_string( Tail, Input' ) } |
               		 { list_to_string( [Character], Symbol ) } 
			| screen#display("I" = Input ) 
			| screen#display("L" = Symbol)
		        | screen#display("S" = State) 
			| Branch.

%%% definition of the automaton directions
%%% each letter that was read from the input changes the state.

Branch(State,Symbol,Input) ::= 

			{ (State = "S0"), (Symbol = "a") , (Input =\= "") } , screen#display("S0-a->S0")  | { State' = "S0" } | screen#display("SBranch" = State) | Extract  ;

 	 		 { (State = "S0") ,(Symbol = "b") , (Input =\= "") } , screen#display("S0-b->S1") | { State' = "S1" } | Extract  ;

 	 		 { (State = "S0") ,(Symbol = "a")  , (Input = "") }  , screen#display("accepted")  | Acceptor ;

 	 		 { (State = "S0") ,(Symbol = "b")  , (Input = "") }  , screen#display("DeadEnd")  | DeadEnd ;

 	 		 { (State = "S1") , (Symbol = "a") , (Input =\= "") } , screen#display("S1-a->S1")  | { State' = "S1" } | Extract  ;

 	 		 { (State = "S1") ,  (Symbol = "b") , (Input =\= "") } , screen#display("S1-b->S0")  | { State' = "S0" } | Extract  ;

 	 		 { (State = "S1") ,(Symbol = "a") , (Input = "") }  ,screen#display("DEadEnd")  | DeadEnd ;

			 { (State = "S1") ,(Symbol = "b") , (Input = "") }  ,screen#display("accepted")  | Acceptor ;

			 { otherwise }  ,screen#display("error")  | Error;


%processes which stach on dummy channecl , dead end when a word was not accepted and accepetor when the word was accepted.

Acceptor ::= dunny ? [] , true.

DeadEnd ::= dunny ? [] , true.

Error ::= dunny ? [] , true.

































