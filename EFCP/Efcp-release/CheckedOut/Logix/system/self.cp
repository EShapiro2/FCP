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
      Primitives
      1988
*/

-export([arity/2, arg/3,
	 length/2, string_hash/2, string_length/2, nth_char/3,
	 make_tuple/2, copy_skeleton/2, string_to_dlist/3, list_to_string/2,
	 tuple_to_dlist/3, list_to_tuple/2,
	 convert_to_integer/2, convert_to_real/2, convert_to_string/2,
	 convert_if_integer/2,
	 freeze/3, freeze/4, freeze/6,
/*************** dfreeze crashes dg compiler 23/01/2000 *********************/
	 /* dfreeze/4, dfreeze/5, dfreeze/6, */ melt/3,
	 make_channel/2, write_channel/2, write_channel/3, close_channel/1,
	 make_vector/3, write_vector/3, write_vector/4,
	 store_vector/3, store_vector/4, read_vector/3, close_vector/2,
	 acos/2, asin/2, atan/2, cos/2, sin/2, tan/2,
	 exp/2, log/2, sqrt/2, pow/3,
	 random/1, srandom/1,
	 invalid_reason/2, make_invalid/2, execute/2,
	 remote_call/4
	]
).

-mode(interrupt).

