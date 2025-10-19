/*
** This module is part of EFCP.
**

     Copyright 2007 Michael Hirsch, William Silverman
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
       Precompiler for FCP - add an extra stream to procedures which need it
       Tailored for Hierarchical System.
       27 January 1985
*/

-export([index/3]).
-mode(trust).
-language(compound).

/*

  Index transforms Remote Procedure Calls  Target#Goal  into:

	distribute # {Integer, Goal}	Target is a string
	transmit # {Target, Goal}	Target is not a string

  Index transforms Distributed Processor Calls  Goal@Target  into:

	distribute # {Integer, Goal}	Target is a string
	link # {Target, Goal}		Target is not a string

  where  Integer  is an index to the modules export list.

  Input:  Program = [procedure(Functor/Arguments,[Clause, ...]), ...]
  Output: NewProgram = [procedure(Functor/Arguments,[NewClause, ...]), ...]
  Output: Exports = [ExternalId, ...]		- external references

  Format: Clause = {H,G,B}
  Format: Identifier = ServiceName
  Format: ExternalId = ServiceName or link(LinkName)

*/

Program ::= [Procedures].
Procedure ::= procedure(GoalId, [Clauses]).
GoalId ::= String/Integer.
Clause ::= {Predicate, [Any], [Predicate]}.
Predicate ::= String ; Tuple.
External ::= String ; link(String).

procedure index(Program, Program, [External]).


index(Program, NewProgram, Imports) :-
	procedures(Program, NewProgram, Importers),
	import_merger(Merger),
	utils # binary_sort_merge(Importers, Sorted, Merger),
	indices(Sorted, 1, Imports).

import_merger(In) :-
    In ? ordered_merge(In1, In2, Out) |
	import_merge(In1, In2, Out),
	import_merger.
import_merger([]).

import_merge(In1, In2, Out) :-

    In1 ? I1,
    In2 = [I2 | _],
    I1 @< I2 :
      Out ! I1 |
	import_merge;

    In1 = [I1 | _],
    In2 ? I2,
    I2 @< I1 :
      Out ! I2 |
	import_merge;

    In1 = [I1 | _],
    In2 ? I2 :
      I1 = I2 |
	import_merge;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .


indices(Sorted, N, Imports) :-

    Sorted ? import(Id, Ix),
    N++ :
      Imports ! Id,
      Ix = N |
	indices;

    Sorted =?= [] :
      N = _,
      Imports = [].


procedures(Program, NewProgram, Imports) :-
    Program ? procedure(Ident, Clauses) : 
      NewProgram ! procedure(Ident, NewClauses?) |
	procedures,
	clauses(Clauses, NewClauses, Imports, Imports').

procedures([], []^, []^).


clauses(Clauses, NewClauses, Imports1, Imports2) :-

    Clauses ? {H, G, B} :
      NewClauses ! {H, G, B'?} |
	clauses,
	rewrite_body(B, B', Imports1, Imports1');

    Clauses = [] :
      Imports1 = Imports2,
      NewClauses = [] |
	true.


rewrite_body(Body, NewBody, Imports1, Imports2) :-

    Body ? Goal :
      NewBody ! Goal'? |
	rewrite_body,
	rewrite_goal(Goal, Goal', Imports1, Imports1');

    Body = [] :
      NewBody = [],
      Imports1 = Imports2 |
	true.


rewrite_goal((Name # Goal), RPC, Im1, Im2) :-
	remote_procedure_call(Name, Goal, RPC, Im1, Im2).
rewrite_goal((Goal @ Link), RPC, Im1, Im2) :-
	remote_procedure_spawn(Link, Goal, RPC, Im1, Im2).
rewrite_goal(Goal, Goal^, Im, Im^) :-
    otherwise |
	true.


remote_procedure_call(	Name, Goal, 
			(distribute # {Index?, Goal})^, 
			[import(Name, Index) | Im]^, Im
) :-
    string(Name) |
	true.
remote_procedure_call(	Service, Goal,
			(transmit # {Service, Goal})^,
			Im^, Im
) :-
    otherwise |
	true.


remote_procedure_spawn(	Link, Goal, 
			(distribute # {Index?, Goal})^,
			[import(link(Link), Index) | Im]^, Im
) :-
    string(Link) |
	true.
remote_procedure_spawn(Link, Goal, (link # {Link, Goal})^, Im^, Im) :-
    otherwise |
	true.

