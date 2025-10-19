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
-export([procedures]).


/* procedures/3
**
** Rename Procedures:
**
**   Primary and imported procedures are unchanged.  Included private
**   procedures (those which are not imported to the primary module),
**   and all references to them, are renamed with suffix  $Index' .
*/

procedures(Completed, Renamed, Exports) :-

   Completed ? descriptor(ExportedIds\Rest, PIdN) |
	utilities # sort_ids(ExportedIds, ExportedIds'),
	rename.

rename(Completed, Renamed, Exports, ExportedIds, Rest, PIdN) :-

   Completed ? Declaration, Declaration = _(_) :
      Renamed ! Declaration |
	self;

   Completed ? proc(Functor1, Arity, Args, RHs1, State),
   State = Status(PId) :
      Renamed ! Functor2?(Arity, Args, RHs2?) |
	rename_procedure,
	rename_rhs,
	exported(PId, Arity, ExportedIds, ExportedIds', Exports, Exports'?),
	self;

   Completed = [] : PIdN = _,
      Renamed = [] |
	export_rest.

exported(PId, Arity, Ids1, Ids2, Es1, Es2) :-

   Ids1 ? Functor/PId :
      Ids2 = Ids1',
      Es1 = [Functor/Arity | Es2] ;

   Ids1 =\= [_/PId | _] : Arity = _,
      Ids2 = Ids1,
      Es1 = Es2 .

export_rest(ExportedIds, Rest, Exports) :-

   ExportedIds ? Functor/_PId :
      Exports ! Functor |
	self;

   ExportedIds = [] :
      Exports = Rest .
      

rename_procedure(Status, PId, PIdN, Functor1, Functor2) :-

   Status = private,				% rename copied local procedure
   PId -= PIdN,
   convert_to_string(PId', IS),
   string_to_dlist(IS, Is, []),
   string_to_dlist(Functor1, Fs, [Dollar | Is]) :
      ascii('$', Dollar) |
	list_to_string(Fs, Functor2);

/* primary/imported procedures unchanged */

   Status =\= private : PId = _, PIdN = _,
      Functor2 = Functor1 .


rename_rhs(PIdN, RHs1, RHs2) :-

   RHs1 ? _(Guard, Body1) :
      RHs2 ! rhs(Guard, Body2?) |
	body,
	self;

   RHs1 = [] : PIdN = _,
      RHs2 = [] .

body(PIdN, Body1, Body2) :-

   Body1 ? primitive(Goal) :
      Body2 ! goal(Goal) |
	self;

   Body1 ? goal(Functor, Goal, private(Index)),
   Index -= PIdN :
      Body2 ! goal(RenamedGoal?) |
	rename_goal,
	self;

   Body1 ? goal(_Functor, Goal, State), State =\= private(_) :
      Body2 ! goal(Goal) |
	self;

   Body1 ? RPC, RPC = rpc(_, _) :
      Body2 ! RPC |
	self;

   Body1 = [] : PIdN = _,
      Body2 = [] .

/*
** rename call to copied local procedure
*/

rename_goal(Index, Functor, Goal, RenamedGoal) :-

   Goal = Functor,
   convert_to_string(Index, IS),
   string_to_dlist(IS, Is, []),
   string_to_dlist(Functor, Fs, [Dollar | Is]) :
      ascii('$', Dollar) |
	list_to_string(Fs, RenamedGoal);

   tuple(Goal), Goal =\= `_,
   convert_to_string(Index, IS),
   string_to_dlist(IS, Is, []),
   string_to_dlist(Functor, Fs, [Dollar | Is]) :
      ascii('$', Dollar),
      RGs = [Functor'? | Gs?] |
	tuple_to_dlist(Goal, [Functor | Gs], []),
	list_to_string(Fs, Functor'),
	list_to_tuple(RGs?, RenamedGoal);

   otherwise : Index = _, Functor = _,
      RenamedGoal = Goal .
