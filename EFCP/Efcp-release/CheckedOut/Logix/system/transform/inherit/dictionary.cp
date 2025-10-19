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
-export([server]).


server(Rs, Nx) :-
   true :
      Ix = 1 |
	stream # hash_table(Ms?),
	serve_requests.

serve_requests(Rs, Ms, Ix, Nx) :-

   Rs ? get(SId, Reply) :
      Ms ! lookup(SId, New?, Old, Result) |
	get_source,
	self;

   Rs ? index(Ix^),
   Ix++ |
	self;

   Rs ? index_value(Ix^) |
	self;

   Rs = [] :
      Ms = [],
      Nx = Ix .


get_source(Result, Old, New, SId, Reply) :-

   Result = new,
   SId = [Name | DId] : Old = _,
      Reply = Reply'? |
	get_source # context(DId, Name, [], Result', {Errors, []}),
	analyse_source;

   Result = old : SId = _,
      New = Old,
      Reply = Old .


analyse_source(Result, Errors, New, Reply) :-

   Result = false(_) : Errors = _,
      Reply = Result,
      New = Reply ;

   Result =\= false(_),
   Errors =\= [] :
      Reply = false(parsing_errors(Errors)),
      New = Reply ;

   Result =\= false(_), Result =\= module(_, _, _),
   Errors = [] :
      Reply = false(not_module),
      New = Reply ;

   Result = module(Options, Attributes, Clauses),
   Errors = [] |
	transform # language_names(Options, inherit, Attributes, Languages),
	languages_upto_inherit(Languages, List),
	transform(List, Options, Clauses, Clauses', Errors'),
	transform_result.

languages_upto_inherit(Languages, List) :-

   Languages ? L, L =\= inherit, L =\= compound, L =\= dfcp,
		  L =\= lpi, L =\= include, L =\= nil :
      List ! L |
	self;

   otherwise : Languages = _,
      List = [] .
	

transform(List, Options, Clauses1, Clauses2, Errors) :-

   List = [] : Options = _,
      Clauses2 = Clauses1,
      Errors = [] ;

   otherwise |
	transform # languages(Options, fcp, [language(List)] , _,
				Clauses1, Clauses2, Errors, []).


transform_result(Attributes, Errors, New, Reply, Clauses) :-

   Errors = [] :
      New = old(Attributes, Clauses),
      Reply = new(Attributes, Clauses) ;

   Errors =\= [] : Attributes = _, Clauses = _,
      Reply = false(Errors),
      New = Reply .
