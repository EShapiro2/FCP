/*

Concurrent Prolog Programming Techniques
	
Instructor:  William Silverman
	
Handout No. 6,  Monday, December 22, 1986
	
	
Recursive process networks
	
	
*/
	
% Process rings
	
simple_ring(Elements,End) :-
	ring(Elements?,End,End).
	
ring([X|Xs],Left,Right) :-
	cell(X,Left,Middle),
	ring(Xs?,Middle,Right).
ring([],End,End).
	
/*
A ring with bidirectional communication streams.
*/
	
bidirectional_ring(Elements,FromLeft,FromRight) :-
	biring(Elements,FromLeft,ToLeft,ToRight,FromRight),
	FromLeft = ToRight,
	ToLeft = FromRight.
	
biring([X|Xs],FromLeft,ToLeft,ToRight,FromRight) :-
	bicell(X,FromLeft?,ToLeft,LeftToRight,RightToLeft),
	biring(Xs?,LeftToRight?,RightToLeft,ToRight,FromRight?).
biring([],From,To,From,To).
	
	
	
/*
Note: the following definition instantiates the left and right channels
of the ring into an infinite (circular) list, containing
the cell elements in both orders.  To see it, use:
	
@#bidirectional_ring([1,2,3],L,R)
@L^
@R^
	
If you try to annotate L^ and R^ in the call,
an infinite list will be printed
*/
	
	
bicell(X,FromLeft,[X|FromRight],[X|FromLeft],FromRight).
	
	
/*
Note:  a simple ring can turn into a biring by the call:
(using link structure: link(FromLeftToRight,FromRightToLeft))
*/
	
cell(X,link(FromLeft,ToLeft),link(ToRight,FromRight)) :-
	bicell(X,FromLeft?,ToLeft,ToRight,FromRight?).
	
	
	
% A toroidal network
	
torus(Array) :-
	torus(Array,Bottoms,Tops),
	Bottoms = Tops.
	
torus([Row|Array],Bottoms,Tops) :-
	row(Row,First,Last,Bottoms,Middles),
	First = Last,
	torus(Array?,Middles,Tops).
torus([],Bottoms,Tops) :-
	Bottoms = Tops.
	
row([Element|Row],Left,Last,[Bottom|Bs],[Top|Ts]) :-
	cell(Element,Left,Right,Bottom,Top),
	row(Row?,Right,Last,Bs,Ts).
row([],Left,Last,[],[]) :-
	Left = Last.
	
% A cell that performs array relaxation
	
cell(	value(N,Initial,Final),
	link(FromLeft,ToLeft),link(ToRight,FromRight),
	link(FromBottom,ToBottom),link(ToTop,FromTop)
) :-
	ToRight = [Initial|Out],
	ToLeft = [Initial|Out],
	ToTop = [Initial|Out],
	ToBottom = [Initial|Out],
	cell1(N?,Initial?,FromLeft?,FromRight?,FromBottom?,FromTop?,Out,Final).

cell1(	N,_,
	[ValueLeft|FromLeft],[ValueRight|FromRight],
	[ValueBottom|FromBottom],[ValueTop|FromTop],
	[Value|Out],Final
) :-
	N > 0 |
	Value := (ValueLeft + ValueRight + ValueBottom + ValueTop) / 4,
	N1 := N - 1,
	cell1(N1?,Value?,FromLeft?,FromRight?,FromBottom?,FromTop?,Out,Final).
cell1(0,Value,_,_,_,_,[],Value).


% An array relaxation driver

relax(ArrayIn,N) :-
	screen#display_stream(ArrayOut,type(ground)),
	relax(ArrayIn,N,ArrayOut).

relax(ArrayIn,N,ArrayOut) :-
	prepare_array(ArrayIn?,N?,Array,ArrayOut),
	torus(Array?).


prepare_array([RowIn|ArrayIn],N,[Row?|Array?],[RowOut?|ArrayOut?]) :-
	prepare_row(RowIn?,N,Row,RowOut),
	prepare_array(ArrayIn?,N,Array,ArrayOut).
prepare_array([],_,[],[]).

prepare_row([Initial|RowIn],N,[value(N,Initial,Final)|Row],[Final?|RowOut]) :-
	prepare_row(RowIn?,N,Row,RowOut).
prepare_row([],_,[],[]).


array(a,[[1,2,3],
	 [4,5,6],
	 [7,8,9]
	]
).

array(b,[[ -9, 20, 10,  0],
	 [-33,  0,  5,  8],
	 [-80, -8, 19,  5],
	 [  0, -1, 30,  1]
	]
).
