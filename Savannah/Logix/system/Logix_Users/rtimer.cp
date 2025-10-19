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

       contact: William.Silverman@weizmann.ac.il

**
*/

/*
**
** Real Timer - time Sub-Computation Remote Procedure Call until terminated
**
**   Out is RealTime, or as defined for  timer.cp  with  real_time = RealTime
*/

-export([goal/2]).
-mode(trust).
-language([evaluate, compound, colon]).
-include(info_constants).

procedure goal(Any, Any).

goal(Call, Out) :-
	goal_rpc(Call, RPC, Goal, Goal'),
	computation_utils # call_id_goal(self # RPC, Id, Goal, Reply),
	time_start(Call, Out, Id, Goal', Reply).


goal_rpc(Call, RPC, Goal1, Goal2) :-

    Call = Call' @ Link :
      Goal2 = Goal2' @ Link |
	goal_rpc;

    otherwise :
      Call = RPC,
      Goal1 = Goal2 .


time_start(Call, Out, Id, Goal, Reply) :-

    Reply = true : Call = _ |
	computation # call([Id # [attributes(A), Goal']], Events),
	processor # machine(idle_wait(Done)),
	time0(Done, A, Goal, Goal', Measures0),
	time1(Events, Measures1),
	difference(Measures1, Measures0, Out);

    Reply = false(Reason) : Out = _, Id = _, Goal = _ |
	computation # failed(Call, Reason).


time0(Done, A, Goal1, Goal2, Measures0) :-
    known(A),
    Done = done |
	get_info_measures(Measures0),
	time0a.

  time0a(Goal1, Goal2, Measures0) :-
    tuple(Measures0) :
      Goal1 = Goal2.


time1(Events, Measures1) :-

    Events = [] |
	get_info_measures(Measures1);

    Events ? String,
    string(String) |
	time1;

    otherwise,
    Events ? Diagnostic |
	computation # Diagnostic,
	time1.


difference(Measures1, Measures0, Out) :-

    we(Out) :
      Out = Out'? | 
	get_info_differences(Measures1, Measures0, [real_time = Out']);

    otherwise |
	get_info_differences(Measures1, Measures0, Out).

    
