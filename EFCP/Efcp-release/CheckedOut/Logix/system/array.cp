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
       Manipulate array, represented by vector.
       1999
*/

-mode(failsafe).
-export([make/3]).
-language(compound).

N ::= Integer.
In ::= [Command].
Ok ::= Tuple ; false.
Command ::= read(Integer, Any) ; write(Integer, Any).
A ::= Vector.


procedure make(N, In, Ok).

make(N, In, Ok) :-

    true :
      make_vector(N, A, Ok) |
	array(In, A);

    otherwise : N = _, In = _,
      Ok = false .


procedure array(In, A).

array(In, A) :-

    In ? read(I, V),
    read_vector(I, A, V^) |
	array;

    In ? write(I,V) :
      store_vector(I, V, A, A') |
	array;

    In = [] |
	close_array(1, A).


procedure close_array(N, A).

close_array(N, A) :-

    N' := N + 1 :
      close_vector(N, A) |
	close_array;

    otherwise : N = _, A = _ .
