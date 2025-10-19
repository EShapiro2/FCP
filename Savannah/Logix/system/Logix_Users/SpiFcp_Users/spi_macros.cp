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
       SpiFcp Transformation macros
       2002
*/

-language(compound).
-export([spi2cmp/3, spi2fcp/3]).
-mode(failsafe).

spi2cmp(Name, Attributes, Results) :-
    string_to_dlist(Name,Fp,[46, 99, 109, 112]),
    list_to_string(Fp, FN) |
	get_source#file(Name, [], R, _),
	transform + (Target = compound).

spi2fcp(Name, Attributes, Results) :-
    string_to_dlist(Name,Fp,[46, 102, 99, 112]),
    list_to_string(Fp, FN) |
	get_source#file(Name, [], R, _),
	transform + (Target = dg).

transform(FN, Attributes, R, Target, Results) :-
    R = module(O, A, S) |
	concatenate(Attributes, A, AI),
	transform#languages(O, Target, AI, _AO, S, SO, Results, Done),
	pretty # module(SO,SP),
	file#put_file(FN, SP, put, Done);
    otherwise :
      FN = _,
      Attributes = _,
      Target = _,
      Results = R.

  concatenate(L1, L2, L3) :-

    L1 ? I :
      L3 ! I |
	self;

    L1 =?= [] :
      L3 = L2;

    L1 =\= [_|_], L1 =\= [] :
      L1' = [L1] |
	self.
