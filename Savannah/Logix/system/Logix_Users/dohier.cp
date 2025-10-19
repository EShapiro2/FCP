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

-language(compound).
-mode(interrupt).
-export([modules/3, directors/3]).

procedure modules(Any, [Any], Any).

modules(Root, Var, Calls) :-
	common(Root, Var, Calls, modules).

procedure directors(Any, [Any], Any).

directors(Root, Var, Calls) :-
	common(Root, Var, Calls, directors).

common(Root, Var, Calls, Kind) :-
    freeze(Var(Calls), FVC, _) |
	hierarchy # source_tree(Root, RTID, T),
	c(FVC, RTID, T, Kind, _).


c(FVC1, RTID, T, Kind, FVC2) :-
    T = {RId, Ms, Ts} |
	ds(FVC1', RTID, Ts, Kind, FVC2, Ds),
	choose(Kind, Ms, Ds, Dos),
	do(FVC1, RTID, RId, Dos, FVC1').

ds(FVC1, RTID, Ts, Kind, FVC2, Ds) :-

    Ts ? T, T = {[D | _], _, _} :
      Ds ! D |
	ds,
	c(FVC1, RTID, T, Kind, FVC1');

    Ts = [] : RTID = _, Kind = _,
      FVC2 = FVC1,
      Ds = [] .

choose(Kind, Ms, Ds, Dos) :-

    Kind = modules : Ds = _,
      Dos = Ms ;

    Kind = directors : Ms = _,
      Dos = Ds .

do(FVC1, RTID, RId, Dos, FVC2) :-

    known(FVC1),
    Dos ? Name :
      melt(FVC1, {RTID#V, Cs}, _) |
	relative_path(RId, Name, V),
	call_services,
	do_when_reply;

    Dos = [] : RTID = _, RId = _,
      FVC2 = FVC1 .

  call_services(Cs, V, Reply) :-

    list(Cs) |
	computation # display(term, V, [type(ground), close(Cs, Cs')]),
	computation_utils # call_list(Cs', Reply);

    otherwise |
	computation_utils # call_output(Cs, [output(V)], [], Reply).

  relative_path(Id, Partial, Relative) :-

    Id ? Node, list(Id') :
      Partial' = Node # Partial |
	relative_path;

    Id = [Node] :
      Relative = Node # Partial ;

    Id = [] :
      Relative = Partial .

  do_when_reply(FVC1, RTID, RId, Dos, FVC2, Cs, Reply) :-

    Reply =?= true :
      Cs = _ |
	do;

    Reply = false(Diagnostic) |
	computation # [event(Diagnostic(Cs)), events(Events)],
	do_when_suspended;

    otherwise :
      Dos = _,
      FVC1 = _,
      FVC2 = _,
      RId = _,
      RTID = _ |
	computation # event(failed(Reply, Cs)).

  do_when_suspended(FVC1, RTID, RId, Dos, FVC2, Events) :-

    Events ? suspended :
      Events' = _ |
	do;

    Events ? Other, Other =\= suspended |
	self.
