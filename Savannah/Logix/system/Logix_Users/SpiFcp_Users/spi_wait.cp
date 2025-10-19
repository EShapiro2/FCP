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
       SpiFcp wait for internal time exceeded
       2004
*/

-language(compound).
-mode(interrupt).
-export(until/2).

/* until(Then, Reply)
**
** Wait until the monitor's internal time (Now) passes Then:
** set Reply = "true".
**
** If internal time is reset, indicated by the end of the debug stream:
** set Reply = "false".
**
** Track internal time from the monitor's debug stream.
**
*/

until(Then, Reply) :-
	computation # spi_status # debug(S),
	wait_until.

  wait_until(S, Then, Reply) :-

    S ? Done, arg(1, Done, done),
    arg(2, Done, Now), Then =< Now :
      S' = _,
      Reply = true;

    S ? _,
    otherwise |
	self;

    S =?= [] :
      Then = _,
      Reply = false.
