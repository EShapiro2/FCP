/*
** This module is part of EFCP.
**

     Copyright 2007 Marilyn Safran
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

-language([inherit,dfcp]).
-export(generic_data_to_snapshot).
-mode(interrupt).

application_table(Rcv, AppTbl) :-
+tables#table(Rcv, AppTbl);

    Rcv ? initialize_application_table(Ok?), AppTbl = application |
	Rcv'' = [create_table(application, {from, content}, [], Ok)
	| Rcv'?],
	self.

nil_application_snapshot(Rcv) :-
  Rcv ? (_From : make_snapshot(application, Snapshot?, Ok?)) |
	Snapshot = [],
	Ok = true,
	self.

generic_application_snapshot(Rcv, AppTbl) :- 
  Rcv ? (_From : make_snapshot(application, Snapshot?, Ok?)), ground(AppTbl) |
	Rcv'' = [get_table_data(AppTbl, Data, Ok) | Rcv'?],
	generic_data_to_snapshot(application, Data?, Snapshot),
	self.

generic_data_to_snapshot(Type, Data, Snapshot) :-
  Data ? {From, Content}, ground(Type) |
	processor#interface(gmdate(Date)),
	Snapshot ! ([application_itself] : event(Type, Date?,(From : Content))),
	self;

  Data = [] |
	Type = _,
	Snapshot = [].

generic_application_event_filter(Type,SelfConnect, SelfConnect1, Event, ListenerName, Ok) :-

  Type = application |
	SelfConnect = SelfConnect1,
	Event = _, ListenerName = _, Ok = true.
