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
       SpiFcp Object Monitor
       2006
*/

-language([evaluate, compound, colon]).
-export([create/2, create/3, create/4, monitor/5]).
-include(spi_constants).
-mode(interrupt).

create(Name, Object) :-
    create(Name, 0, Object, _).

create(Name, Value, Object) :-
	create(Name, Value, Object, _).

create(Name, Value, Object, Values) :-

    we(Object) :
      make_vector(OBJECT_ARITY, Object, Output),
      Output = {Values, Requests},
      store_vector(OBJECT_VALUES, Value, Object, Object') |
	read_vector(OBJECT_VALUES, Object', Value'),
	monitor;

    vector(Object),
    arity(Object, OBJECT_ARITY) :
      Value = _,
      Name = _,
      Values = Values'?,
      write_vector(OBJECT_REQUESTS, values(Values'), Object);

    otherwise |
	fail(create(Name, Value, Object, Values)).  

monitor(Name, Value, Object, Values, Requests) :-

    Requests ? close(true^) :
      close_vector(OBJECT_VALUES, Object) |
	self;

    Requests ? name(Name?^, true^) |
	self;

    Requests ? read(V?^, true^) :
      V = Value |
	self;

    Requests ? store(V, true^) :
      store_vector(OBJECT_VALUES, V, Object, Object') |
	self;

    Requests ? values([Value | Values?]^, true^) |
	self;

    Requests ? Request,
    arity(Request, Arity),
    arg(Arity, Request, Reply),
    otherwise :
      Reply = false |
	self;

    Requests ? Request,
    otherwise |
	fail(spi_object(Name) - Request),
	self;

    Requests =?= [] :
      Requests' = _,
      close_vector(OBJECT_VALUES, Object) |
	self;

    Values ? Value' :
      Value = _ |
	self;

    Values =?= [] :
      Requests = _,
      Value = _,
      Name = _,
      Values = _,
      close_vector(OBJECT_REQUESTS, Object).
