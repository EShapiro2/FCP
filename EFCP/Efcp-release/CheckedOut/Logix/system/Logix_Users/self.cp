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
      String Primitives
      2011
*/

-export([stribrk/3, strpbrk/3, stripbrk/4,
	 strcat/3, strncat/4,
	 strchr/3, strrchr/3,
	 strcmp/3, strncmp/4, 
         strcpy/3, strncpy/4,
         strspn/3, strcspn/3,
	 stristr/3, strpstr/3,
	 strtok/2, strtok/3,
	 strlen/2 
        ]
).

-mode(interrupt).
