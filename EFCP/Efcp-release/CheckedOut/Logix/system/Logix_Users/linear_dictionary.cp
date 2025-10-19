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
       Linear Dictionary Monitor
       06/89
*/

-language(compound).
-export([requests/2]).
-mode(trust).

Requests ::= [(add(Any, Any) ; add(Any, Any, Reply))].
Reply ::= old ; new.
Dictionary ::= [Entry].
Entry ::= {Any, Any}.

procedure requests(Requests, Dictionary).

/*
	A linear dictionary server :

	In:	requests : add(Name, Value), add(Name, Value, Reply).

	Out:	dictionary is a difference-list; new entries are added to
		the tail.
*/

procedure requests(Requests, Dictionary, Dictionary, Dictionary).

requests(In, Out) + (Head = List, Tail = List) :-

    In ? add(Name, Value) |
	lookup(Name, Value, Head, Tail, Tail', _),
	requests;

    In ? add(Name, Value, Reply) |
	lookup(Name, Value, Head, Tail, Tail', Reply),
	requests;

    In = [] :
      Tail = [],
      Head = Out |
	true.

OpenDictionary ::= [Entry | Dictionary].

procedure lookup(Any, Any, OpenDictionary, OpenDictionary, Dictionary, Reply).

lookup(Name, Value, Head, Tail1, Tail2, Reply) :-

    Head = [Name(Value^) | _] :
      Tail1 = Tail2,
      Reply = old |
	true;

    Head ? _,
    otherwise |
	lookup;

    Head = Tail1 :
      Tail1 = [Name(Value) | Tail2],
      Reply = new |
	true.
