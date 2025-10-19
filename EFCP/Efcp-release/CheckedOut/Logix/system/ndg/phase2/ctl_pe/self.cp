/*
** This module is part of EFCP.
**

     Copyright 2007 Shmuel Kliger
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

-language([compound,typed]).
-export([ctls/5]).
-mode(trust).

Entry ::= {Areg,Vreg,Type,Value}.

Type ::= new ; load ; car ; addr ; sub_arg ; deref ;
	list ; nil ; string ; number ; integer ; real ; constant ; 
	tuple ; compound ; known ; unknown ; vector ; module ; var.

TypeCheck ::= list ; string ; number ; integer ; real ; constant ; tuple ;
		compound ; known ; unknown ; vector ; module ; var.

procedure ctls(Dgs, Ctls, Iterate, Pr, SC).

ctls(Dgs, Ctls, Iterate, Pr, SC)
:-
	ctls#ctls(Dgs, Ctls, Iterate, Pr, SC).
