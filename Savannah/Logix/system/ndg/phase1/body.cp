/*
** This module is part of EFCP.
**

     Copyright 2007 Shmuel Kliger
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

-export([body/3,copytuple/5]).
-language(compound).
-mode(trust).

procedure body(Body, DgBodies, SC).

body(Body, DgBodies, SC)
:-
	Body ? BodyGoal,
	N := arity(BodyGoal),
	make_tuple(N,Goal),
	arg(1,BodyGoal,Pred),
	arg(1,Goal,GPred),
	SC = {Left,Right} :
	GPred = Pred,
	SC' = {Left',Right},
	DgBodies ! spawn(Goal) |
	     copytuple(BodyGoal, 1, N, Goal, {Left,Left'}),
	     self ;

	Body = [], SC = {L,R} : DgBodies = [], R = L.

copytuple(Tuple, Low, High, To, SC)
:-
	High-- > Low, SC = {L,R},
	arg(High,Tuple,Arg), arg(High,To,NewArg) : SC' = {L',R} |
	     copytuple1(Arg, NewArg, {L,L'}),
	     self ;

	High = Low, SC = {L,R} : L = R, Tuple = _, To = _.

copytuple1(A, NewA, SC)
:-
	A = integer(_), SC = {L,R} : NewA = A , L = R ;

	A = string(_), SC = {L,R} : NewA = A , L = R ;

	A = nil(_), SC = {L,R} : NewA = A , L = R ;

	A = real(_), SC = {L,R} : NewA = A , L = R ;

	A = tuple(Tuple),
        arity(Tuple,Ar),
	make_tuple(Ar,NewTuple) :
	NewA = tuple(NewTuple) |
	    copytuple(Tuple, 0, Ar, NewTuple, SC) ;

	A = list([Car|Cdr]), SC = {L,R} : NewA = list([CarArg|CdrArg]) |
	    copytuple1(Car, CarArg, {L,L'}),	
	    copytuple1(Cdr, CdrArg, {L',R}) ;

	A = variable(V), V =\= '_', SC = {L,R} : NewA = psi([A]), L = R ;

	A = ro(V), V = variable(_), SC = {L,R} : NewA = ro(psi([V])), L = R ;

	A = ro(V), V = psi(_), SC = {L,R} : NewA = A, L = R ;

	A = variable('_'), SC = {L,R} : NewA = A, L = R ;

	A = psi(_), SC = {L,R} : NewA = A, L = R.
