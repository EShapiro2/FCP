/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:



       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

/*
       Variable Dictionary Server
       11/89
*/

-export([start/1]).
-language(compound).
-mode(trust).
-serve([ground/3, freeze/6, add/2, add/3, find/3, bindings/2,
	unbind/1, size/1, unbind/0,
	inherit/1]).

Format ::= namevars ; parsed ; freeze.
Self_Ref   ::= truncate ; isomorphic ; ignore.
AddReply ::= old ; new ; false(cant_unify).
FindReply ::= true ; false ; false(cant_unify).
Var_Type ::= known ; we ; var ; ro.
Selector   ::= 0 ; 1 ; 2 ; 3.
Id ::= String ; Integer.
State ::= basic ; incremental .
IDs ::= [id(Any, Integer, [Any])].

procedure add(String, Any).
procedure add(String, Any, AddReply).
procedure bindings([String=Any], Selector).
procedure find(Id, Any, FindReply).
procedure freeze(Any, Any, Integer, Integer, Self_Ref, Format).
procedure ground(Any, {Any, [{String, Any}]}, {Integer, Integer, Format}).
procedure size(Integer).
procedure unbind.
procedure unbind(Id).


start(In) + (Stop = _)  :-
	basic # requests(Out?),
	serve.

serve(In, Out, Stop) :-

    In ? add(S, V) :
      Out ! add(S, V, _) |
	serve;

    In ? basic |
	self;

    In ? state(State) :
      State = basic |
	self;

    In ? inherit(New') :
      Out ! bindings(Bs, 0) |		% Get all named variables
	inherit_bindings(Bs, NewStop?, New, New'),
	start(New?, NewStop),
	serve;

    In ? Other,
    otherwise :
      Out ! Other |
	serve;

    In = [] :
      Out = [],
      Stop = stop .


inherit_bindings(Bs, Stop, New1, New2) :-

    Bs ? (Name = Term) :
      New1 ! add(Name, TermCopy?) |
	listen(Term, TermCopy, Stop),
	self;

    Bs = [] : Stop = _,
      New1 = New2 .

listen(From, To, Stop) :-

    unknown(Stop),
    tuple(From),
    A := arity(From),
    make_tuple(A, Via) |
	listen_tuple;

    unknown(Stop),
    From ? Car :
      To = [Car'? | To'?] |
	listen(Car?, Car', Stop),
	listen;

    otherwise :	Stop = _,		% constant or vector or Stop
      To = From? .

listen_tuple(From, To, Stop, Via, A) :-

    A > 0,
    arg(A, From, FromA),
    arg(A, Via, ToV),
    A' := A - 1 :
      ToV = ToA? |
	listen_tuple,
	listen(FromA?, ToA, Stop);

    A =< 0 : From = _, Stop = _,
      To = Via .
