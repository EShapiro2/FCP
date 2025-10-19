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
       SpiFcp Trace channel activity from monitor debug output
       2000
*/

-language([evaluate,compound,colon]).
-mode(failsafe).
-export([run/2, run/3, run/4, run/5]).

-include(spi_constants).

REALTIME => 12.

START => '+'.
END => '-'.
SEND => '->'.
RECEIVE => '<-'.
SEPARATOR => ':'.

run(Goal, Arg) :-
    number(Arg) |
	run(Goal, traced, Arg);
    otherwise |
	run(Goal, Arg, TIME_LIMIT_VALUE).

run(Goal, File, Limit) :-
	runit + (Scale = 1, Format = none, Style = short).

run(Goal, File, Limit, Arg) :-
    number(Arg) |
	runit + (Scale = Arg, Format = none, Style = short);
    otherwise |
	format_arg + (Scale = 1, Format = Arg).

run(Goal, File, Limit, Scale, Format) :-
    number(Scale) |
	format_arg.
run(Goal, File, Limit, Format, Scale) :-
    number(Scale) |
	format_arg.

format_arg(Goal, File, Limit, Scale, Format) :-

    Format =?= none :
      Style = short |
	runit;

    Format =?= process :
      Style = short |
 	runit;

    Format =?= creator :
      Style = creator |
 	runit;

    Format =?= full :
      Style = creator |
	runit;

    otherwise :
      Limit = _,
      File = _,
      Goal = _,
      Scale = _ |
	fail("Unrecognized format" - Format).

runit(Goal, File, Limit, Scale, Format, Style) :-

    Goal =?= _#_,
    string(File), File =\= "",
    Limit >= 0,
    convert_to_real(Scale, Scale'),
    0.0 < Scale' |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(debug(Stream), Scheduler, Scheduler'),
	write_channel(cutoff(Limit, _State), Scheduler'),
	copy_leading_limits_and_idle(Stream, Stream'),
	computation#[Goal, events(Events)],
	computation # spi_utils # show_list(Values?, [Style, 3], Processes),
	synchronize_output,
	screen#display_stream(Out?, [put(File), width(10000)]),
	filter_data;

    otherwise :
      Style = _ |
	fail("Bad argument" - run(Goal, File, Limit, Scale, Format)).

  copy_leading_limits_and_idle(As1, As2) :-

    As1 ? limit(_) |		/* copy (multiple) limit(s) */
	self;

    As1 ? idle(_) |		/* drop at most one idle */
      As1' = As2;

    otherwise :
      As1 = As2.

  synchronize_output(Processes, Out) :-

    Processes ? Process :
      Out ! Process |
	self;

    Processes =\= [_|_],
    Processes =\= [] :
      Out = Processes |
	computation#display(term, (trace:Out));    

    otherwise :
      Out = Processes.

filter_data(Stream, Events, Scale, Format, Values) :-

    Stream ? start(Process) :
      Values ! START(Process) |
	self;

    Stream ? done(Now, Sender(_SendName, _SendChannelId),
	               Receiver(_ReceiveName, _ReceiveChannelId)),
    Scaled := Now*Scale,
    Format = none :
      Values ! Scaled,
      Values' ! END(Sender), 
      Values'' ! END(Receiver) |
	self;

    Stream ? done(Now, Sender(SendName, _SendCreatedId),
	               Receiver(ReceiveName, _ReceiveCreatedId)),
    Scaled := Now*Scale,
    Format = process :
      Values ! Scaled,
      Values' ! SEND(END(Sender), SendName), 
      Values'' ! RECEIVE(END(Receiver), ReceiveName) |
	self;

    Stream ? done(Now, Sender(_SendName, SendCreatedId),
	               Receiver(_ReceiveName, ReceiveCreatedId)),
    Scaled := Now*Scale,
    Format = creator :
      Values ! Scaled,
      Values' ! SEND(END(Sender), SendCreatedId), 
      Values'' ! RECEIVE(END(Receiver), ReceiveCreatedId) |
	self;

    Stream ? done(Now, Sender(SendName, SendCreatedId),
	               Receiver(ReceiveName, ReceiveCreatedId)),
    Scaled := Now*Scale,
    Format = full :
      Values ! Scaled,
      Values' ! SEND(END(Sender),
		     SEPARATOR(SendName, SendCreatedId)), 
      Values'' ! RECEIVE(END(Receiver), 
			 SEPARATOR(ReceiveName, ReceiveCreatedId)) |
	self;

    Stream ? Idle, Idle =?= idle(_) :
      Stream' = _,
      Stream'' = Idle |
	self;

    Stream ? _Element,
    otherwise |
	self;

    Stream =\= [_ | _] :
      Events = _,
      Format = _,
      Scale = _,
      Values = Stream;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Format = _,
      Scale = _,
      Values = [].
