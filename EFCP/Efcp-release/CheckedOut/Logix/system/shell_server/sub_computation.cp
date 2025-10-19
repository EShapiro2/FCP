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
       Computation shell
       04/92.
*/


-language([inherit, dfcp]).
-export([start/4]).
-mode(trust).

start(Requests, Events, Redelegates, Delegates) :-

	computation # self # service_id(SId),
	start1(Requests, Events, Redelegates, Delegates, SId?).

start1(Requests0, Events1, Redelegates, Delegates, SId) :-

    ground(SId) |
	Requests ! change_scope(SId, Ok),
	Requests' = Requests0,
	computation # "_domain"(domain_channel(Domain)),
	computation # "_controls"(CCC),
	controls(CCC?, Out!, CCC', SuperCH),
	computation_server # computation(Requests?, CCC'?, Domain?, Events),
	delegate(Out?, Events?, Events1, SuperCH?, Redelegates, Delegates),
	ok(Ok?).

ok(True) :-
    True = true | true.

controls(CCIn, Out, CCOut, SuperCHOut) :-

    CCIn = {Ss, L, R, SuperCH} |
	CCOut = {Ss, L, R, Out},
	SuperCHOut = SuperCH .

delegate(Out, InEs, OutEs, SuperCH, Redelegates, Delegates) :-

    Out ? Delegated, Delegated = delegated(_, _) |
	Delegates ! Delegated,
	self;

    Out ? Link, Link = link(_, _, _) |
	Delegates ! Link,
	self;

    Out ? Request, Request =\= delegated(_, _), Request =\= link(_, _, _) |
	write_channel(Request, SuperCH, SuperCH'),
	self;

    Redelegates ? Request |
	write_channel(Request, SuperCH, SuperCH'),
	self;

    InEs ? Event |
	OutEs ! Event,
	self;

    unknown(Out),
    InEs = [] |
	Delegates = [],
	Out' = [],
	Delegates' = [],
	self;

    Out = [], Redelegates = [],
    Delegates = [], InEs = [] |
	SuperCH = _,
	OutEs = [] .


