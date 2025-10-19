/*
** This module is part of EFCP.
**

     Copyright 2007 Avraham Houri, Shmuel Kliger
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

-export([stream/2, term/2]).
-language(compound).
-mode(interrupt).

stream(In, Out) :-

    In ? T,
    known(T) |				% known(INVALID) is true
	term(T, T'),
	stream_out(In', T'?, Out);

    In ? T,
    var(T) |
	term(T'?, T),
	stream_out(In', T', Out);

    In =\= [_|_], In =\= [] |		% INVALID =\= [_|_], INVALID =\= []
	term(In, Out);

    In = [] :
      Out = [] ;

    var(In) |
	stream(Out?, In);

    invalid(In) :
      Out = In .


stream_out(In, T, Out) :-

    var(In) :				% var(INVALID) is false
      Out = [T | Out'] |
	stream(Out'?, In);

    otherwise :
      Out = [T | Out'?] |
	stream(In, Out').

term(T, T') :-

    var(T) |
	term(T'?, T);

    invalid(T) :
      T' = T ;

    vector(T) |
	wait_vector(T, T', valid);

    otherwise,
    dfreeze(T, TF, TL, TV) :
      melt(TF, TM, TL') |
	validate(TV?, TL?, TL'?, valid, Right, Right),
	send(Right?, TM?, T, T').

validate(TV, FL, ML, Left, Right, Result) :-

    TV = [] :
      FL = _,		% []
      ML = _,		% []
      Right = Left,
      Result = _ ;

    TV ? we,
    FL ? V',
    ML ? V |
	wait_term(V?, V', Result),
	self;

    TV ? both,
    FL ? _,
    ML ? _ |
	self;

    TV ? ro,
    FL ? RO,
    ML ? V |
	wait_ro(RO, V, Left, Left', Result),
	self.


wait_term(V, V', Result) :-

    Result = valid |
	term(V, V');

    Result =\= valid :
      V = _, V' = _ .


wait_ro(RO, V, Left, Right, Result) :-

    vector(RO) :
      Right = Left |
	wait_vector(RO, V, Result);

    Left = invalid :
      RO = _, V = _,
      Right = invalid,
      Result = _ ;

    invalid(RO) :
      V = _,
      Left = _,
      Right = invalid,
      Result = _ ;

    otherwise :
      RO = _, V = _,
      Left = _ |
      Right = iterate,
      Result = _ ;

    Result = invalid :
      RO = _, V = _,
      Left = _, Right = _ .

wait_vector(VO, VN, Result) :-

    Result = valid,
    A := arity(VO) :
      make_vector(A, VN, VI) |
	vector_streams(A, VI, VO);

    otherwise :
      VO = _, VN = _,
      Result = _ .

vector_streams(A, VI, VO) :-

    A-- > 0,
    arg(A, VI, AI) |
	relay(AI, A, VO),
	self;

    A =< 0 :
      VI = _, VO = _ .

relay(AI, A, VO) :-

    AI ? T,
    var(T) :
      write_vector(A, T', VO, VO') |
	term(T, T'?),
	self;

    AI ? T,
    otherwise :
      write_vector(A, T'?, VO, VO') |
	term(T?, T'),
	self;

    otherwise : AI = _,			% Even if AI is INVALID ?
      close_vector(A, VO) .


send(Result, Melted, T, T') :-

    Result = invalid :
      Melted = _,
      T = _,
      make_invalid(T', transaction) ;

    Result = iterate :
      Melted = _ |
	term(T, T');

    Result = valid :
      T = _,
      T' = Melted .
