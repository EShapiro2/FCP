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
       Pre-compiler boot program transformer
       1986
*/

-export([system/2]).
-mode(trust).
-language(compound).


List ::= [Any].

procedure system(Procedures, Procedures).

Procedures ::= [procedure(Any, [{boot(Any, Any), {List, List}, List} | List])].


system(SourceProcedures, BootedProcedures) :-
    SourceProcedures ? procedure(Id, Boots) :
      BootedProcedures = [procedure(Id, NewBoots) | SourceProcedures'] |
	boot_procedure(Boots, NewBoots).


boot_procedure(Boots, NewBoots) :-
    Boots ? Clauses :
      NewBoots ! NewClauses |
	boot_clause(Clauses, NewClauses),
	boot_procedure.
boot_procedure([], []^).

boot_clause(	{ {Boot, `In, Out},
			{Ask, Tell},
			Body
		},
		{ {Boot, `boot(in), Out},
			{Ask, [`boot(in) = [system | `In] | Tell]},
			Body
		}^
).
boot_clause(Clauses, Clauses^) :-
    otherwise |
	true.
