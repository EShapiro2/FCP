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

-monitor(alter).
-language(compound).
-export([off/0, on/0]).

List ::= [Any].
Truth ::= true ; false.

procedure on.
procedure off.

procedure alter(List).
procedure alter(List, Truth, Truth).

/*
** Filter input to transform.
**
** States: Not filtering (On = true)  -->  Filtering <= "off"
**         Filtering (On = false)  --> Not Filtering <= "on"
**
** While filtering, change all calls to completely transform a module,
** to instead omit the typed transformation, so as to suppress creation
** of the _type attribute for compiled modules.
**
*/


alter(In) + (On = true, Off = _) :-

    In ? off,
    On = true : On' = false |
	trap # filter(transform, _Goals, Calls),
	modify(Calls, Off),
	alter;

    In ? on,
    On = true |
	alter;

    In ? off,
    On = false |
	alter;

    In ? on,
    On = false :
      On' = true,
      Off = false,
      Off' = _ |
	alter;

    In = [] :
      On = Off |
	true;

    In ? Other,
    Other =\= on, Other =\= off |
	alter,
	fail(Other, unknown).


procedure modify(List, Truth).

modify(Calls, Off) :-

    unknown(Off),
    Calls ? trapped(_ # languages(A1,fcp,A3,A4,A5,A6,A7,A8),
		    changed(languages(A1,typed,A3,A4,A5,A6,A7,A8))^
	    ) |
	modify;

    otherwise,
    Calls ? trapped(_, false^) |
	modify;

    Off = false : Off' = true |
	transform # untrap,
	modify;

    Calls = [] : Off = _ |
	true.
