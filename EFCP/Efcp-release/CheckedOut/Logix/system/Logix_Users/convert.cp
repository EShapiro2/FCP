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
	Transform the named file up to the target transformation.
	Target = fcp implies complete transformation (up to precompile).
	Converted is a pretty-printed source file.
*/

-language(compound).
-export([source / 3, context / 4]).
-mode(interrupt).


source(Name, Target, Converted) :-
      context(computation, Name, Target, Converted).


context(Context, Name, Target, Converted) :-
  string_to_dlist(Target, TCs, []),
  string_to_dlist(Name, FCs, [46 | TCs]),
  list_to_string(FCs, Name') |
      extract_mode(Target, Mode),
      get_source # context(Context, Name, [], R, _),
      transform.


extract_mode(biospi, aspic^).
extract_mode(spifcp, aspic^).
extract_mode(_Other, fcp^) :-
  otherwise |
      true.


transform(Name, R, Target, Mode, Converted) :-

  R = module(O, A, S) |
      transform # languages(O, Target, A, _AO, S, SO, Converted, Done),
      pretty # module(SO, SP, Mode),
      file # put_file(Name, SP, put, Done) ;

  otherwise :
    Mode = _,
    Name = _,
    Target = _,
    Converted = R.


concatenate(L1, L2, L3) :-

  L1 ? I :
    L3 ! I |
      self ;

  L1 =?= [] :
    L3 = L2 ;

  L1 =\= [_ | _],
  L1 =\= [] :
    L1' = [L1] |
      self.
