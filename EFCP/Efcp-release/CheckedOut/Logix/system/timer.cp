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
       Command Timer
       1985
*/

-export([rpc/2, rpc/3, time/2, time/3]).
-language([evaluate, compound, colon]).
-mode(failsafe).
-include(info_constants).

/* 
** The timer process samples "info" values and then starts the Call process.
**
** When the Call process terminates, is halted, or becomes idle, the "info"
** values are sampled again, and the differences computed.  The differences
** designated by Out are computed and unified with the specified Output.
**
** The output may be a writable variable, Out, in which case it is unified
** with a default tuple containing selected differences.  Alternative forms
** of Out may be a list of descriptors of the form  Name = Value, wnere Name
** is one of:
**
**   cpu, free_heap, used_heap, creations, reductions, suspensions,
**   terminations, activations, charge_time, pq_length, freed_heap,
**   heap_words, gc, copied_heap
**
**     "cpu" stands for (cumulated) compute time;
**     "pq_length" stands for (currrent) process queue length;
**     "gc" stands for (cumulated) garbage collection time;
**     "real_time" is a synonym for "charge_time".
**
**   Most of the Values are computed as simple differences between the
**   measured named statistics at the end and the beginning of the process.
**   The statistics named free_heap, pq_length and copied_heap refer to
**   the current value - they are not cumulated, and differences would be
**   meaningless.
*/

procedure rpc(Any, Any).

rpc(Call, Configure) + (Synchronize = _) :-
	goal_rpc(Call, RPC, Goal, Goal'),
	computation_utils # call_id_goal(self # RPC?, Id, Goal, Reply),
	rpc_start.


procedure time(Any, Any).

time(Call, Configure) + (Synchronize = _) :-
	goal_rpc(Call, RPC, Goal, Goal'),
	computation_utils # call_id_goal(self # RPC?, Id, Goal, Reply),
	time_start.


time_start(RPC, Configure, Synchronize, Id, Goal, Reply) :-

    Reply =?= true :
      RPC = _ |
	computation # Id # [ [] | Goals? ],
	processor # [machine(idle_queue(Done, 1)) | Waiter],
	configure_timer(Configure, Actions, EventKey, Display, Out),
	time_start_idle,
	start_when_known,
	drop_leading_nulls(Actions?, Actions'),
	timing(Events?, Actions'?, Display, Measures1),
	difference;

    Reply =?= false(Reason) :
      Goal = _,
      Id = _,
      Process = time |
	synchronize_fail(Synchronize),
	timing_failed.

  time_start_idle(Done, Measures0) :-
    Done =?= done |
	get_info_measures(Measures0).

  start_when_known(Goals, Goal, Measures0, EventKey,
		   Synchronize, Waiter, Events
  ) :-

    EventKey =?= key(Events'),
    ground(Measures0) :
      Synchronize = {Ready, Ready},
      Events = Events'?,
      Goals = [Goal],
      Waiter = [];

    EventKey = none,
    ground(Measures0) :
      Synchronize = {Ready, Ready},
      Goals = [Goal],
      Waiter = [machine(idle_wait(Events))].

/*
** timing
**
** Wait for Event, an Action (not idle(AtTime)), a terminal Action.
*/

timing(Events, Actions, Display, Measures1) :-

    known(Events) |
	timing_events;

    Actions ? Action,
    Action =\= idle(_) |
	self;			/* Ignore Action other than idle(AtTime) */

    Actions ? Action,
    Action =?= idle(_) :
      Actions' = _, 
      Actions'' = Action |
	self;

    Actions =?= none :
      Actions' = _ |
	self;			/* Wait for Events only. */

    Actions =\= none,
    Actions =\= [_|_] :
      Events = _ |
	display_end_actions(Measures1, Display, time, Actions),
	get_info_measures(Measures1).

  timing_events(Events, Actions, Display, Measures1) :-

/* Ignore Event, except for terminated, aborted or no more stream. */

    Events ? terminated :
      Events' = _,
      Events'' = terminated |
	timing;

    Events ? aborted :
      Events' = _,
      Events'' = aborted |
	timing;

    otherwise,
    Events ? _Event |
	timing;

/* Otherwise do the best you can. */

    Events =\= [_|_] :
      Actions = _,
      Events' = _,
      Actions' = Events |
	timing.

difference(Measures1, Measures0, Display, Out) :-

    we(Out),
    known(Measures1) :
      Out = (cpu = _CU, reductions = _R, creations = _CR, heap_words = _HW) |
	get_info_differences(Measures1, Measures0, Out),
	display_difference(Display, Out);

    otherwise,
    known(Measures1) |
	get_info_differences(Measures1, Measures0, Out),
	display_difference(Display, Out).

  display_difference(Display, Out) :-

    known(Out),
    arg(1, Display, false) :
      true;

    known(Out),
    Display =?= true(No) |
	display_timer(No, time, Out).

/*
** rpc is reductions per cycle...
*/

rpc_start(RPC, Configure, Synchronize, Id, Goal, Reply) :-

    Reply =?= true :
      RPC = _ |
	Id # [[] | Goals],
	processor # machine(idle_queue(Done, 1)),
	configure_timer(Configure, Actions, _EventKey, Display, Out),
	rpc_started;

    Reply = false(Reason) :
      Goal = _,
      Id = _,
      Process = rpc |
	synchronize_fail(Synchronize),
     	timing_failed.

  rpc_started(Goal, Synchronize, Actions, Display, Out, Goals, Done) :-
    Done = done,
    info(INFO_REDUCTIONS, InitialReductions) :
      Goals ! Goal,
      Counter = 0,
      Synchronize = {Ready, Ready} |
	drop_leading_nulls(Actions?, Actions'),
	rpc_counter.


rpc_counter(InitialReductions, Actions, Display, Out, Counter, Goals) :-
    
    Actions =?= none,
    info(INFO_PQ_LENGTH, L),
    L > 0,
    Counter++ :
      deschedule |
	self;

    Actions ? Action,
    Action =\= done(_),
    Action =\= idle(_),
    Counter++ :
      deschedule |
	self;

    otherwise,
    info(INFO_REDUCTIONS, FinalReductions),
    TotalReductions := FinalReductions - InitialReductions,
    Cycles := Counter - 1,
    Reductions := TotalReductions - Counter - 3 :
      Goals = [] |
	display_end_actions(end, Display, rpc, Actions),
	quotient(Reductions, Cycles, RPC),
	send_rpc(RPC,
		 Display,
		 ('reductions' = Reductions, 'cycles' = Cycles,
			'reductions_per_cycle' = RPC
		 ),
		 Out
	).

  quotient(Numerator, Denominator, Quotient) :-

    Denominator = 0 :
      Numerator = Quotient ;

    otherwise |
	Quotient := Numerator / Denominator.

  send_rpc(Trigger, Display, Out1, Out2) :-

    known(Trigger),
    arg(1, Display, false) :
      Out1 = Out2;

    known(Trigger),
    Display =?= true(No) :
      Out1 = Out2 |
	display_timer(No, rpc, Out2).

/*
** Utilities
*/

configure_timer(Configure, Actions, EventKey, Display, Out) :-

    Configure =?= configure(D, N, O) :
      Actions = none,
      Display = D(N),
      EventKey = none,
      Out = O;

    Configure =?= configure(A, E, D, N, O) :
      Actions = A,
      Display = D(N),
      EventKey = key(E),
      Out = O;

    we(Configure) :
      Actions = none,
      Configure = Out?,
      Display = false([]),
      EventKey = none;

    Configure =\= configure(_, _, _),
    Configure =\= configure(_, _, _, _, _) :
      Actions = none,
      Display = false([]),
      EventKey = none,
      Out = Configure.


display_end_actions(Trigger, Display, Process, Actions) :-

    known(Trigger),
    Actions =?= [] :
      Actions' = finished |
	self;

    Actions =?= none :
      Actions' = done |
	self;

    Actions =?= [Action | _] :
      Actions' = Action |
	self;

    otherwise,
    Display =?= _Display(No),
    known(Trigger) |
	display_timer(No, Process, Actions).


display_timer(No, Process, Out) :-

    integer(No) |
	utils#append_strings(["<", No, "> ", Process, " :"], Prefix),
	computation # display(term, Out, [wait(Prefix), prefix(Prefix)]);

    otherwise :
      No = _,
      Out = _,
      Process = _.


drop_leading_nulls(As1, As2) :-

    As1 ? limit(_) |		/* drop (multiple) limit(s) */
	self;

    As1 ? Ambient,		/* conserve (multiple) ambient(s) */
    Ambient =?= ambient(_) :
      As2 ! Ambient |
	self;

    As1 ? idle(_) |		/* drop at most one idle */
      As1' = As2;

    otherwise :
      As1 = As2.


goal_rpc(Call, RPC, Goal1, Goal2) :-

    Call = Call' @ Link :
      Goal2 = Goal2' @ Link |
	goal_rpc;

    otherwise :
      Call = RPC,
      Goal1 = Goal2 .


synchronize_fail({[],[]}^).
synchronize_fail({[], _}^).
synchronize_fail({_, []}^).


timing_failed(Configure, Process, RPC, Reason) :-

    Configure =?= configure(_Actions, _Events, _Display, No, Out) |
	unify_without_failure([], Out),
	display_timer(No, Process, (RPC, Reason));

    Configure =?= configure(_Display, No, Out) |
	unify_without_failure([], Out),
	display_timer(No, Process, (RPC, Reason));

    unknown(Configure) :
      Process = _,
      Reason = _,
      RPC = _ |
	unify_without_failure([], Configure);

    otherwise :
      Process = _,
      Configure = _,
      Reason = _,
      RPC = _.
