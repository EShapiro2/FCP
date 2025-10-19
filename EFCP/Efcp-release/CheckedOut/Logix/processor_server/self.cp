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
       Processor Context server
       1/92
*/

-language(compound).
-mode(trust).
-export([start/2]).

start(In, Ready) :-
    In ? {Goal, Common} :
      Common = fork(Common', fork(Common'', Common''')),
      Links ! {lookup(file, File), _Ok1, Common'},
      Links' ! {lookup(interface, Interface), _Ok2, Common''},
      make_channel(Defer, Deferred),
      In'' = Out? |
	device_server # start(Devices?, Defer),
	dlv_server # start(Dlv?),
	doors_api_server # start(Doors?),
	file_server # server(Files?, File?, Interface?, Defer),
	interface_server # start(Inters?),
	link_server # start(Links?, Defer),
	machine_server # start(Machs?, Defer),
	math_server # start(Maths?),
	terminal_server # start(Outputs?),
	room_server # start(Rooms?),
	time_server # start(Time?, Defer),
	preview([{Goal, Common'''} | In'], Deferred, Serve, Defer),
	when(File?, Interface?, Ready, Serve, Out),
	server.

when(File, Interface, Ready, In, Out) :-

    integer(File),
    integer(Interface) :
      Ready = ready,
      In = Out ;	

    Ready = ready : File = _, Interface = _, In = _, Out = _ .

preview(In, Deferred, Serve, Defer) :-

    In ? Request(Common),
    Request = Kind(Goal), Kind =\= event,
    unknown(Goal) |
	self,
	defer(Goal, Request(Common), Defer);

    In ? Request(Common),
    Request = Kind(Goal), Kind =\= event,
    arg(1, Goal, Functor),
    unknown(Functor) |
	self,
	defer(Functor, Request(Common), Defer);

    In ? Request(Common),
    Request = Kind(Goal), Kind =\= event,
    arg(1, Goal, Functor),
    known(Functor) :
      Common = fork(Common', Common''),
      Serve ! {Kind(Goal, Ok), Common'} |
	self,
	wait_ok;

    In ? Request(Common),
    Request = Kind(Goal), Kind =\= event,
    string(Goal) :
      Common = fork(Common', Common''),
      Serve ! {Kind(Goal, Ok), Common'} |
	self,
	wait_ok;

    In ? Request(Common),
    Request = Kind(List), Kind =\= event,
    List ? Goal :
      Common = fork(Common', Common''),
      In'' = [{Kind(Goal), Common'}, {Kind(List'), Common''} | In'] |
	self;

    In ? Request(Common),
    Request = Kind([]), Kind =\= event :
      Common = done |
	self;

    In ? Request(Common), Request = event(Event) :
      Serve ! {room(log(Event, _Reply), _Ok), Common} |
	self;

    In ? Request(Common),
    Request = Kind(Goal, _), Kind =\= event,
    unknown(Goal) |
	self,
	defer(Goal, Request(Common), Defer);

    In ? Request(Common),
    Request = Kind(Goal, _), Kind =\= event,
    arg(1, Goal, Functor),
    unknown(Functor) |
	self,
	defer(Functor, Request(Common), Defer);

    In ? Request(Common),
    Request = Kind(Goal, Ok'), Kind =\= event,
    arg(1, Goal, Functor),
    known(Functor) :
      Common = reply(Ok, Ok, Ok', Common'),
      Serve ! {Kind(Goal, Ok), Common'} |
	self;
 
    In ? Request(Common),
    Request = Kind(Goal, Ok'), Kind =\= event,
    string(Goal) :
      Common = reply(Ok, Ok, Ok', Common'),
      Serve ! {Kind(Goal, Ok), Common'} |
	self;

    In ? Request(Common),
    Request = Kind(List, Ok), Kind =\= event,
    List ? Goal :
      Common = fork(Common', Common''),
      In'' = [{Kind(Goal), Common'}, {Kind(List', Ok), Common''} | In'] |
	self;

    In ? Request(Common),
    Request = Kind([], Ok), Kind =\= event :
      Common = reply(true, true, Ok, done) |
	self;

    In ? Request(Common), Request = event(Event, Reply) :
      Serve ! {room(log(Event, Reply), _Ok), Common} |
	self;

    In ? {Other, Common},
    otherwise |
	unserve,
	self;

    Deferred ? retry(RC) :
      In' = [RC | In] |
	self;

    Deferred ? defer(Uninstantiated, RC) |
	defer(Uninstantiated, RC, Defer),
	self;

    In = [],
    unknown(Deferred) |
	close_channel(Defer),
	self;

    In = [],
    Deferred = [] : Defer = _,
      Serve = [] .

unserve(Other, Common) :-

    true :
      Common = exception(invalid, Other) ;

    known(Common),
    Other = _Functor(_Request, Ok),
    var(Ok) :
      Ok = false(aborted) ;

    otherwise : Other = _, Common = _ .

server(In, Devices, Dlv, Doors, Files, Inters, Links,
       Machs, Maths, Outputs, Rooms, Time
) :-

    In ? {device(Goal, Ok), Common} :
      Devices ! {Goal, Ok, Common} |
	self;

    In ? {dlv(Goal, Ok), Common} :
      Dlv ! {Goal, Ok, Common} |
	self;

    In ? {doors_api(Goal, Ok), Common} :
      Doors ! {Goal, Ok, Common} |
	self;

    In ? {file(Goal, Ok), Common} :
      Files ! {Goal, Ok, Common} |
	self;

    In ? {interface(Goal, Ok), Common} :
      Inters ! {Goal, Ok, Common} |
	self;

    In ? {link(Goal, Ok), Common} :
      Links ! {Goal, Ok, Common} |
	self;

    In ? {machine(Goal, Ok), Common} :
      Machs ! {Goal, Ok, Common} |
	self;

    In ? {math(Goal, Ok), Common} :
      Maths ! {Goal, Ok, Common} |
	self;

    In ? {terminal(Goal, Ok), Common} :
      Outputs ! {Goal, Ok, Common} |
	self;

    In ? {room(Goal, Ok), Common} :
      Rooms ! {Goal, Ok, Common} |
	self;

    In ? {time(Goal, Ok), Common} :
      Time ! {Goal, Ok, Common} |
	self;

    otherwise,
    In ? {Invalid, Common}, Invalid = {_, _, Ok} :
      Ok = false(invalid),
      Common = done |
	self;

    In ? {Any, Common}, Any = {_, _, Ok},
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] :
      Devices = [],
      Dlv = [],
      Doors = [],
      Files = [],
      Inters = [],
      Links = [],
      Machs = [],
      Maths = [],
      Outputs = [],
      Rooms = [],
      Time = [] .

defer(Variable, RC, Retry) :-

    known(Variable) :
      write_channel(retry(RC), Retry) ;

    otherwise,
    RC = {Goal, Common} : Variable = _, Retry = _,
      Common = exception(blocked, Goal) ;

    RC = _(Common),
    known(Common) : Variable = _, Retry = _ .

wait_ok(Ok, Request, Common) :-

    Ok = true : Request = _,
      Common = done ;

    Ok = false(Reason) :
      Common = exception(Reason, Request) ;

    known(Common) : Ok = _, Request = _ .

/* Various subordinate servers inherit this procedure - it is not used here */

terminate(In) :-

    In ? Request,
    Request = _(Ok, Abort), known(Abort) :
      Ok = false(aborted) |
	self;

    In = [] | true.
