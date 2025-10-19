/*
** This module is part of EFCP.
**

     Copyright 2007 Avraham Houri, William Silverman
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
       machine_time monitor.
       9/88
*/

-export(open/2).
-language(compound).

procedure open(Type, In).

In ::= [Request] ; [].

Type ::= real ; virtual ; profiling.
Request ::= close ;
	    get(Period, Delay);
	    reset(Period, Delay);
	    set(Period, Delay);
	    stream(Counts).
Delay, Period ::= Number.
Counts ::= [Integer].


procedure open(Type, In).

open(Type, In) :-
	processor # [device(signals(Devices)), link(lookup(timer, Offset))],
	start.

Devices ::= Stream.
Offset ::= Integer.
Signals ::= [signal].

procedure start(Type, In, Devices, Offset).

start(Type, In, Devices, Offset) :-
    string(Type),
    Reset := real(0) :
      execute(Offset, open(Type, Signals)),
      Signals' = Signals?,
      Counts = _ |
	server.

procedure server(Type, In, Devices, Offset, Signals, Period, Counts).

server(Type, In, Devices, Offset, Signals, Reset, Counts) :-

    In ? get(Delay, Period) :
      execute(Offset, get_timer(Type, {{PS, PM}, {DS, DM}})) |
	self,
	Delay := DS + real(DM)/1000000, Period := PS + real(PM)/1000000;

    In ? reset(Delay, Period),
    Reset' := real(0) : Reset = _,
      execute(Offset, get_timer(Type, {{PS, PM}, {DS, DM}})) |
	self,
	Delay := DS + real(DM)/1000000, Period := PS + real(PM)/1000000,
	execute(Offset, set_timer(Type, {{0, 0}, {0, 0}}), Offset');

    In ? set(Delay, Period),
    Delay >=0, Period >= 0,
    DS := integer(Delay),  DM := integer(1000000*(Delay - DS)),
    PS := integer(Period), PM := integer(1000000*(Period - PS)) : Reset = _,
      Reset' = Period,
      execute(Offset, set_timer(Type, {{PS, PM}, {DS, DM}})) |
	self;
				% Counts should start more-or-less
				% in sync with the signals.
    In ? stream(RealOut) :
      RealOut = Counts? |
	self;
				% Stay at end of signal stream.
    Signals ? signal :
      execute(Offset, read_timer(Type, Counts, Counts'?)) |
	self;

    unknown(Signals),
    Devices ? restart,
    Reset' := real(0) :
/* The timer was opened and not closed before the save.  The delay **
** could be recalculated from a sample taken just before the save. */
      execute(Offset, open(Type, Signals')),
      In' = [set(Reset, Reset) | In] |
	self;

    Signals = [] : Type = _, In = _, Devices = _, Offset = _, Reset = _,
      Counts = [] ;

    In = [] : Devices = _, Offset = _, Reset = _, Signals = _,
      Counts = [] |
	close_timer(Offset, Type).


procedure close_timer(Offset, Type).

close_timer(Offset, Type) :-
    true :
      execute(Offset, set_timer(Type, {{0,0}, {0,0}})) |
	execute(Offset, close(Type), _).

procedure execute(Offset, Tuple, Offset).

execute(Offset, Command, Offset^) :-
    true :
      execute(Offset, Command) |
	true.
