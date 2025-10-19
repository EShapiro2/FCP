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

-export([starter/5]).
-mode(trust).
-language(compound).

/*
	Timer interface
	2013
*/

starter(Process, RPC, Limit, Configure, Public) :- 
    Public = run(Run, Control, Events),
    vector(Control),	
    number(Limit) :
      Configure = configure(Actions?, Events, _Display, No, _Out),
      NG = new_goal(No, Process(RPC, Limit)) |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(record(Record), Scheduler?, Scheduler'),
	write_channel(cutoff(Limit, StateList), Scheduler'?, Scheduler''),
	choose_actions(Process, Record, StateList, Actions),
	computation_utils # call_id_goal(RPC, _Id, _Goal, Reply),
	timer # Process(Time, Configure, Synchronize),
	start.

  choose_actions(rpc, Every, _, Every^).
  choose_actions(time, _, Done, Done^).

  start(Reply, Scheduler, RPC, Run, Time, Synchronize, NG) :-

    Reply = true,
    channel(Scheduler) :
      NG = _,
      Synchronize = {RPC, Run},
      Time = [];

    otherwise,
    channel(Scheduler) :
      Reply = _,
      Synchronize = _,
      Run = self # true,
      Time = RPC |
	computation # shell(NG).