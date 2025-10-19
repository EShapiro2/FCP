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
       SpiFcp Return monitor status
       2004
*/
-language(compound).
-mode(interrupt).
-export([now/1, debug/1, ordinal/1, record/1, get_status/2, extract/3]).

/*
** now, ordinal: return the value of the corresponding named BioSpi parameter.
**
** debug, record: return the output stream corresponding to the named
** BioSpi parameter.
**
** get_status: return the value of the named BioSpi parameter;
**             use to get cutoff_limit, cutoff_status or any other.
**
** extract: return the named item from the Status list.
*/

/* Return Value = current internal time. */
now(Value?^):- get_status(now, Value).

/* Return the private ordinal value which is next to be assigned. */
ordinal(Value?^) :- get_status(ordinal, Value).

/* Return the debug stream. */
debug(Stream?^) :- get_status(debug, Stream).

/* Return the record stream. */
record(Stream?^) :- get_status(record, Stream).

/* Return the named status value. */
get_status(Name, Value?^) :-
	computation # spi_monitor #
			status(Status), extract(Status, Name, Value).

extract(Status, Name, Value):-

    Status =\= [_|_], Status =\= [] :
      Status' = [Status] |
	self;

    Status ? Name(VV) :
      Status' = _ ,
      Value = VV? ;

    Status ? (Name = VV) :
      Status' = _ ,
      Value = VV? ;

    Status ? X ,
    X =\= Name(_), X =\= (Name = _) | 
	self ; 

    Status =?= [] :
      Name = _ ,
      Value = [] .
