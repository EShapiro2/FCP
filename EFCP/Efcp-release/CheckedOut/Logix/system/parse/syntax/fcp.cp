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

-language(syntax).

yfx( ';'  , 1070).	% 18/6/89	1050
yfx( '|'  , 1060).	% 18/6/89	1100
yfy( ':'  , 1050).	% 18/6/89	1060
yfx( ','  , 1000).
yfx( '&'  ,  990).      % 3/5/2000      1000
yfx( '!'  ,  900).	% 1/1/88	new
xfy( '?'  ,  900).	% 4/5/88	new
yfy( '='  ,  800).	% 1/1/88	700
xfy( '@'  ,  720).
yfx( '#'  ,  710).
yfy( '<'  ,  700).
yfy( '>'  ,  700).
xfy( '+'  ,  500).
xfy( '-'  ,  500).
xfy( '*'  ,  400).
xfy( '/'  ,  400).
yfy( '\'  ,  300).
yfy( ':-' , 1200).
yfy( '<-' , 1190).	% 18/6/89	new
yfy( '->' , 1190).	% 02/12/2001	new
yfy( '=>' , 1010).	% 15/12/91	new
yfy( '@<' ,  800).	% 1/1/88	700
yfy( ':=' ,  700).
yfy( '+=' ,  700).	% 27/4/91	new
yfy( '-=' ,  700).	% 27/4/91	new
yfy( '==' ,  700).
yfy( '#<' ,  700).
yfy( '\=' ,  700).
yfy( '=<' ,  700).
yfy( '>=' ,  700).
xfy( '\/' ,  250).
xfy( '/\' ,  240).
yfy( '::=', 1200).	% 1/1/88	new
yfy( '<=>', 1010).	% 15/12/91	new
yfy( '=?=',  800).	% 1/1/88	700
yfy( '=\=',  800).	% 1/1/88	700
yfy( '=:=',  700).
xfy( 'div',  400).
yfy( 'mod',  300).
fx(  '#'  ,  710).
fy(  '+'  ,  220).
fy(  '-'  ,  220).
fy(  '~'  ,  220).
fx(  '`'  ,  210).
fy(  '?'  ,  210).
yf(  '^'  ,  205).	% 27/4/91	200
yf(  '??' ,  200).	% 22/5/91	new
yf(  '++' ,  200).	% 27/4/91	new
yf(  '--' ,  200).	% 27/4/91	new
fx( 'procedure', 100).	% 1/1/88	new
xf(  '!'  ,  100).	% 20/10/91	changed from 900
/* added for ambients */
fx( 'p2c', 98).		% 17/1/02	new
fx( 'c2p', 98).		% 17/1/02	new
fx( 's2s', 98).		% 17/1/02	new
fx( 'local', 98).	% 29/1/02	new
fx( 'enter', 98).	% 17/1/02	new
fx( 'accept', 98).	% 17/1/02	new
fx( 'exit', 98).	% 17/1/02	new
fx( 'expel', 98).	% 17/1/02	new
