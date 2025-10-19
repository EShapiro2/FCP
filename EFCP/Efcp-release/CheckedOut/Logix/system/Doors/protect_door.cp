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

-language(compound).

% Protect Door: ensures that every message received on the input
% stream is either a valid term or is INVALID.

% A term is _valid_ if all its variables are writable.

protect_door(Door,Door1) :-
	Door = door(IdCard,Snd,Rcv) |
		Door1 = door(IdCard,Snd,Rcv'?),
		protect_rcv(Rcv?,Rcv').

protect_rcv(Rcv,Snd) :-
	invalid(Rcv) |  Snd = [];

	Rcv ? Message |
		valid_message(Message?,Message'),
		Snd ! Message'?,
		self.

valid_message(M,M1) :-
	% This routine will be simplified
	% when a valid(X) kernel is a available.
	valid(M,true,Ok),
	assign(Ok?,M,M1).

valid(X,L,R) :-
	constant(X) |
		R = L;

	X = [X1|Xs] |
	valid(X1,L,M),
	valid(Xs,M? , R);

	tuple(X), arity(X,N) |
		valid_tuple(N,X,L,R);

	var(X) |
		R = L;

	invalid(X) |
		L = _, R = false(invalid(X)).


valid_tuple(N,X,L,R) :-
	N = 0 | X = _, R = L;

	N > 0,  arg(N,X,A) |
		valid(A,L,M),
		N' := N-1,
		valid_tuple(N'?,X,M?,R).



assign(Ok,X,Y) :-
	Ok = true | Y = X;

	Ok =\= true | X = _, Y = Ok.	
