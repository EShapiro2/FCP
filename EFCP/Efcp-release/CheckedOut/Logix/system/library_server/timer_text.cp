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

       contact: William.Silverman@weizmann.ac.il

**
*/

-export([string/1]).

string(Text) :- true : Text =

"library.

-mode(trust).
-language([evaluate, compound, colon]).
-export([get_info_measures/1, get_info_differences/3]).
-include(info_constants).

/*
** get_info_measures(Measures^)
**
** Make an INFO_ARITY tuple.
** Set each argument to the tuple to the corresponding value of info.
**
** Out: Measures - the tuple
*/

get_info_measures(Measures) :-
    writable(Measures),
    make_tuple(INFO_ARITY, Tuple),
    info(INFO_CPU_TIME, A1),
    arg(INFO_CPU_TIME, Tuple, B1),
    info(INFO_FREE_HEAP, A2),
    arg(INFO_FREE_HEAP, Tuple, B2),
    info(INFO_USED_HEAP, A3),
    arg(INFO_USED_HEAP, Tuple, B3),
    info(INFO_CREATIONS, A4),
    arg(INFO_CREATIONS, Tuple, B4),
    info(INFO_REDUCTIONS, A5),
    arg(INFO_REDUCTIONS, Tuple, B5),
    info(INFO_SUSPENSIONS, A6),
    arg(INFO_SUSPENSIONS, Tuple, B6),
    info(INFO_TERMINATIONS, A7),
    arg(INFO_TERMINATIONS, Tuple, B7),
    info(INFO_ACTIVATIONS, A8),
    arg(INFO_ACTIVATIONS, Tuple, B8),
    info(INFO_COLLECTIONS, A9),
    arg(INFO_COLLECTIONS, Tuple, B9),
    info(INFO_PQ_LENGTH, A10),
    arg(INFO_PQ_LENGTH, Tuple, B10),
    info(INFO_FREED_HEAP, A11),
    arg(INFO_FREED_HEAP, Tuple, B11),
    info(INFO_CHARGE_TIME, A12),
    arg(INFO_CHARGE_TIME, Tuple, B12),
    info(INFO_GC_TIME, A13),
    arg(INFO_GC_TIME, Tuple, B13),
    info(INFO_COPIED_HEAP, A14),
    arg(INFO_COPIED_HEAP, Tuple, B14) :
      A1 = B1,
      A2 = B2,
      A3 = B3,
      A4 = B4,
      A5 = B5,
      A6 = B6,
      A7 = B7,
      A8 = B8,
      A9 = B9,
      A10 = B10,
      A11 = B11,
      A12 = B12,
      A13 = B13,
      A14 = B14,
      Measures = Tuple.

/*
** get_info_differences(Measure1, Measure0, Out)
**
** Given a list of info requirements and two Measurements, fill in the
** difference between the named info and the corresponding values.
** Each element of the list has the form: Name = Difference, where
**
**   Name is a string - one of:
**     
**     cpu, free_heap?, used_heap?, creations, reductions, suspensions,
**     terminations, activations, collections, pq_length?,
**     real_time ~ INFO_CHARGE_TIME, heap_words! ~ INFO_FREED HEAP,
**     gc_time, copied_heap?
**
**   Difference is the difference between the corresponding measurements
**   arguments for all but free_heap, used_heap, pq_length, copied_heap,
**   for which Difference = the last recorded value, and for heap_words,
**   for which Difference = total heap words used.
**
**   The reply Done is set to 'done' when all differences specified by Out
**   have been computed.
*/ 

get_info_differences(Measures1, Measures0, Out) + (Done = _) :-

    Out ? (cpu = X^),
    arg(INFO_CPU_TIME, Measures1, X1),
    arg(INFO_CPU_TIME, Measures0, X0),
    X := X1 - X0 |
	self;
/*
** This is not cumulated, so difference is meaningless.
*/
    Out ? (free_heap = X^),
    arg(INFO_FREE_HEAP, Measures1, X) |
	self;
/*
*/
    Out ? (used_heap = X^),
    arg(INFO_USED_HEAP, Measures1, X1),
    arg(INFO_USED_HEAP, Measures1, X0),
    X := X1 - X0 |
	self;

    Out ? (creations = X^),
    arg(INFO_CREATIONS, Measures1, X1),
    arg(INFO_CREATIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (reductions = X^),
    arg(INFO_REDUCTIONS, Measures1, X1),
    arg(INFO_REDUCTIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (suspensions = X^),
    arg(INFO_SUSPENSIONS, Measures1, X1),
    arg(INFO_SUSPENSIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (terminations = X^),
    arg(INFO_TERMINATIONS, Measures1, X1),
    arg(INFO_TERMINATIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (activations = X^),
    arg(INFO_ACTIVATIONS, Measures1, X1),
    arg(INFO_ACTIVATIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (collections = X^),
    arg(INFO_COLLECTIONS, Measures1, X1),
    arg(INFO_COLLECTIONS, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (charge_time = X^),
    arg(INFO_CHARGE_TIME, Measures1, X1),
    arg(INFO_CHARGE_TIME, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (real_time = X^),
    arg(INFO_CHARGE_TIME, Measures1, X1),
    arg(INFO_CHARGE_TIME, Measures0, X0),
    X := X1 - X0 |
	self;
/*
** This is not cumulated, so difference is meaningless.
*/
    Out ? (pq_length = X^),
    arg(INFO_PQ_LENGTH, Measures1, X1),
    arg(INFO_PQ_LENGTH, Measures0, X0),
    X := X1 - X0 |
	self;
/*
*/
    Out ? (freed_heap = X^),
    arg(INFO_FREED_HEAP, Measures1, X1),
    arg(INFO_FREED_HEAP, Measures0, X0),
    X := X1 - X0 |
	self;

    Out ? (heap_words = HW^),
    arg(INFO_USED_HEAP, Measures1, U1),
    arg(INFO_USED_HEAP, Measures0, U0),
    arg(INFO_COLLECTIONS, Measures1, C1),
    arg(INFO_COLLECTIONS, Measures0, C0),
    arg(INFO_FREED_HEAP, Measures1, F1),
    arg(INFO_FREED_HEAP, Measures0, F0),
    HW := (U1-U0) + (real(C1)*F1 - real(C0)*F0) |
	self;

    Out ? (gc = X^),
    arg(INFO_GC_TIME, Measures1, X1),
    arg(INFO_GC_TIME, Measures0, X0),
    X := X1 - X0 |
	self;
/*
** This is not cumulated, so difference is meaningless.
*/
    Out ? (copied_heap = X^),
    arg(INFO_COPIED_HEAP, Measures1, X) |
	self;
/*
*/
    Out = (Arg, Out') :
      Out'' = [Arg | Out'] |
	self;

    otherwise,
    Out ? _ |
	self;

    Out =\= [_ | _], Out =\= [] :
      Out' = [Out] |
	self;

    Out = [] :
      Measures1 = _,
      Measures0 = _,
      Done = done.
"

	| true.
