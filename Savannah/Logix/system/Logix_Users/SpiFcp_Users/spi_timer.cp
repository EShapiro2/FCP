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
** SpiFcp Timer caller
*/

-export([starter/3]).
-mode(trust).
-language(compound).

starter(Control, Limit, Goal) :-
    vector(Control),
    number(Limit) ,
    Goal =?= Process(_Run, Configure), 
%   Configure = configure(Actions, Events, Display, N, Out)
    arg(2, Configure, Actions) |
	computation # spi_monitor # scheduler(Scheduler),
	write_channel(record(Record), Scheduler, Scheduler'),
	write_channel(cutoff(Limit, StateList), Scheduler'?),
	configure(Process, Record?, StateList?, Actions),
	computation#timer#Goal.

  configure(rpc, Record, _StateList, Record^).
  configure(time, _Record, StateList, StateList^).
