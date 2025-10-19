/*
** This module is part of EFCP.
**

     Copyright 2007 Marilyn Safran
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

-language([nil]).
-mode(interrupt).

LF => 10.	% ascii value of line-feed <lf>
BLANK => 32.	% ascii value of blank
AT_SIGN => 64.	% ascii value for @
DOT => 46.	% ascii value for .
PERCENT => 37.  % ascii value for %
HASH => 35. 	% ascii for #

SIGNIFICANT_DIGITS => 7.
