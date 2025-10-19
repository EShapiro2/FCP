/*
** This module is part of EFCP.
**

     Copyright 2007 Yossie Lichentenstein, William Silverman
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
       Execution Profiler.
       Yossie Lichentenstein, Bill Silverman 28/05/86
*/

/*
   These are the procedures in this module :

   profile                (  List of modules to be profiled ,  
                             Goal to be reduced and profiled , 
                             Output variable )
   prepare_output         (  Merged output , Output variable )
  
 
  Data is { Spawned Processes , Reductions (Calls) }

*/   

-export([profile/3, interpret/3, prepare/3]).
-mode(interrupt).
-language(compound).

procedure profile((String ; [String]), (String ; Tuple), [Measurement]).

Measurement ::= (ModuleId # String/Integer, Integer, Integer, Integer) .
ModuleId ::= String | ServiceId .

profile(Modules, Goal, Output) :-
	interpret # goals(Modules, Goal, Cumulate),
	binary_merge(Cumulate, Out),
	prepare_output(Modules, Out, Output).

interpret(Modules, Goal, Cumulate) :-
	interpret # goals(Modules, Goal, Cumulate).

prepare(Modules, Cumulate, Output) :-
	binary_merge(Cumulate, Out),
	prepare_output(Modules, Out, Output).


prepare_output(Modules, Out, Output) :-

    Modules = all,
    Out ? { {Id, Functor, Arity}, {Creations, Reductions, RPC} } :
      Output ! (Id # Functor/Arity: Reductions, Creations, RPC) |
	prepare_output;

    Modules =\= all,
    Out ? { {Id, Functor, Arity}, {Creations, Reductions, RPC} },
    Id = [Name | _] :
      Output ! (Name#Functor/Arity: Reductions, Creations, RPC) |
	prepare_output;

    Out = [] : Modules = _,
      Output = [] .


ordered_merge(In1, In2, Out) :-

    In1 ? {Id, {C1, R1, RPC1}},
    In2 ? {Id, {C2, R2, RPC2}} :
      In1'' = [{Id, Cumulant} | In1'] |
	cumulate_data(C1, R1, RPC1, C2, R2, RPC2, Cumulant),
	ordered_merge;

    In1 ? {Id1, Data1},
    In2 = [{Id2, _ } | _], Id1 @< Id2 :
      Out ! { Id1, Data1 } |
	ordered_merge;

    In2 ? {Id2, Data2},
    In1 = [{Id1, _} | _], Id2 @< Id1 :
      Out ! {Id2, Data2} |
	ordered_merge;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .


cumulate_data(C1, R1, RPC1, C2, R2, RPC2, {Creations, Reductions, RPC}^) :-
    Creations := C1 + C2,
    Reductions := R1 + R2,
    RPC := RPC1 + RPC2 |
	true.


binary_merge(In, Out) :-
	level1_server(In, Odd, Even, PairList),
	binary_merger3(Odd, Even, PairList, Out).


level1_server(In, Odds, Evens, PairList) :-

    In ? Odd :
      Odds = [Odd] |
	level1_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [],
      Evens = [],
      PairList = [] .


level1_server(In, Evens, PairList) :-

    In ? Even :
      Evens = [Even],
      PairList ! {One, Two} |
	level1_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_merger0(In1, In2, PairList, Out) :-
	ordered_merge(In1, In2, Out1),
	binary_merger2(PairList, Out2, UpList),
	binary_merger3(Out1, Out2, UpList, Out).


binary_merger1(PairList, UpList) :-

    PairList ? {In1, In2} :
      UpList ! {Out1, Out2} |
	ordered_merge(In1, In2, Out1),
	binary_merger2(PairList', Out2, UpList');

    PairList = [] :
      UpList = [] .


binary_merger2(PairList, Out2, UpList) :-

    PairList ? {In1,In2} |
	binary_merger1(PairList', UpList),
	ordered_merge(In1, In2, Out2);

    PairList = [] :
      UpList = [],
      Out2 = [] .


binary_merger3(Out1, Out2, UpList, Out) :-

    list(Out2) |
	binary_merger0(Out1, Out2, UpList, Out);

    Out2 = [] : UpList = _,
      Out1 = Out .
