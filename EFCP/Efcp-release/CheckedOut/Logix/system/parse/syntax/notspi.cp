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

serve(Reqs, S1) :-

    Reqs ? prefix_operator(NotSpi, B, C, D),
    NotSpi =\= 'c2p',
    NotSpi =\= 'p2c',
    NotSpi =\= 's2s',
    NotSpi =\= 'local',
    NotSpi =\= 'enter',
    NotSpi =\= 'accept',
    NotSpi =\= 'exit',
    NotSpi =\= 'expel' :
      S1 ! prefix_operator(NotSpi, B, C, D) |
	serve;

    Reqs ? prefix_operator(_IsSpi, B, C, D),
    otherwise :
      B = 98,
      C = 97,
      D = false |
	serve;

    Reqs ? Other :
      S1 ! Other |
	serve;

    Reqs = [] :
      S1 = [].
