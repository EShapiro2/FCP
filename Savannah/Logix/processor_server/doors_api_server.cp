/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman, Marilyn Safran
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
       Doors Api interface process
       30/06/94
*/

-mode(trust).
-export([start/1]).
-language(dfcp).


procedure start(Input).

Input ::= {Command, Ok, Common}.

start(Input) :-

    Input ? {initialize(In, Out, Arguments, Reply), Ok, Common},
    writable(Out),
    known(Arguments),
    writable(Ok),
    writable(Reply) |
	Common = directed_interface(COut?, CIn),
	processor # link(lookup(doorsfcp, Api), ApiOk),
	proceed,
	self;

    Input ? {Other, Ok, Common},
    otherwise |
	Ok = false(unknown(Other)),
	Common = done,
	self;

    Input ? {Any, Ok, Halt},
    known(Halt) |
	Ok = false(aborted(Any)),
	self;

    Input = [] |
	true.

proceed(Api, In, Out, Arguments, Reply, CIn, COut, Ok, ApiOk) :-

    ApiOk = true,
    listener(Api) |
	execute_reply(Api, doorsInitialize(Read, Arguments, Warning), InitOk),
	initialized;

    ApiOk =\= true |
	Api = _, In = _, CIn = _, Arguments = _,
	Reply = reply(-1, -1),
	COut = [],
	Out = [],
	Ok = ApiOk.

initialized(Api, In, Out, Read, Reply, CIn, COut, Ok, Warning, InitOk) :-

    InitOk = 0 |
	Reply = reply(0, Warning),
	Ok = true,
	stream # merger(Outs?, Out),
	serve(Api, In, Read, CIn, COut, Outs);

    integer(InitOk),
    InitOk =\= 0 |
	Api = _, In = _, CIn = _, Read = _,
	Out = [],
	Reply = reply(InitOk, Warning),
	COut = [],
	Ok = true;

    otherwise |
	Api = _, In = _, CIn = _, Read = _, Warning = _,
	Out = [],
	Reply = reply(-1, -1),
	COut = [],
	Ok = false(InitOk).


serve(Api, In, Read, CIn, COut, Outs) :-

    Read ? Stream,
    listener(Api) |
	Outs ! merge(Stream),
	self;

    In ? Ms,
    ground(Ms),
    listener(Api) |
	execute_reply(Api, Ms, Reply),
	check_ms;

    In = [],
    listener(Api) |
	In' = done,
	execute_reply(Api, {doorsExit}, Reply),
	Ms = doorsExit,
	check_ms;

    known(CIn),
    listener(Api) |
	In = _,
	In' = done,
	execute_reply(Api, {doorsAbort}, Reply),
	Ms = doorsAbort,
	check_ms;

    In = done |
	Api = _, Read = _, CIn = _,
	COut = [],
	Outs = [].

check_ms(Api, In, Read, CIn, COut, Outs, Ms, Reply) :-

    Reply = 0 |
	Ms = _,
	serve;

    Reply =\= 0 |
	COut ! exception(false(Reply), Ms),
	serve.


execute_reply(Api, Request, Reply) :-
    integer(Api) |
	execute(Api, {Request, Reply});
    otherwise |
	Api = _, Request = _,
	Reply = execute_failed.
