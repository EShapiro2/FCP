-language(psifcp).

States + MNO ::= { MNO = "MNOP" } | Extract .

Extract(MNO) ::= { string_to_dlist( MNO, [Character | Tail], []) } |
                { list_to_string( [Character], State ) } |                { list_to_string( Tail , MNO' ) } |

		screen#display("S"=  State )|
		screen#display("M" = MNO)|

		Branch | Iterate .

Branch(State) ::= { (State = "M") } , Minnesota ;
                 { (State = "N") } , NewYork ;
                 { (State = "O") } , Oregon ;
                 { otherwise } , NoneOfTheAbove .

Iterate(MNO) ::= { (MNO = "") } , screen#display("The End") ;
                { otherwise } , Extract .

Minnesota(State) ::= screen#display(State - "Minnesota").

NewYork(State) ::= screen#display(State - "New York").

Oregon(State) ::= screen#display(State - "Oregon").

NoneOfTheAbove(State) ::= screen#display(State - "Who Knows").



