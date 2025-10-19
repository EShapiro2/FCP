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
       Driver for lpi transformation - logic programs with inheritance.
       November 1991
*/


-language(compound).
-export([transform/5]).
-mode(trust).


transform(Attributes1, Clauses, Attributes2, Terms, Errors) :-

					% get path of transformed file
	get_context(Attributes1?, ModuleName, Context),
% services for principle module
	servers # ids(IdC, ModuleName?, Context),
% begin with principle module
	module # clauses(Clauses, Terms1, _Estate(Rscs), IdC, IdC', Diags([])),
	remote # filter_calls(Terms1, Terms),
	servers # diagnostics(Diags?, Errors),
	servers # dictionary(Lookups?),
	depends(Rscs?, Lookups, Names),
	depends_on(Names?, Attributes1?, Attributes2),
	close_channel(IdC').

/***
* Search attributes list for id of analysed module.
***/

get_context(Attr, ModuleName, Context) :-

    Attr ? service_id(MId),
    MId ? ModuleName^ : Attr' = _,
      Context = MId' ;

    Attr ? A, A =\= service_id([_|_]) |
	get_context;

    Attr = [] :
      ModuleName = "unknown_module" |
	computation # service_id(Context) .

/***
* Extract dependence on heritage from remote modules.
***/

depends(Rscs, Lookups, Names) :-

    Rscs ? rsc(RPC, Head, Local, RHSS, Depends), RPC = _ModuleName # _Call,
    Depends ? Addition :
      Lookups ! lookup(Addition, [], _),
      Rscs'' = [rsc(RPC, Head, Local, RHSS, Depends') | Rscs'] |
	depends;

    Rscs ? rsc(ModuleName # _Call, _Head, _Local, _RHSS, []) :
      Lookups ! lookup(ModuleName, [], _) |
	depends;

    Rscs = [] :
      Lookups = names(Names, []) .
	

depends_on(Names, Attributes', Attributes) :-

    Names = [] :
      Attributes = Attributes' ;

    Names =\= [] :
      Attributes ! depends(Names) .
