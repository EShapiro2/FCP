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

-export([tell/3]).
-language(compound).
-mode(trust).

procedure tell(Clss, DgL, SC).

tell(Clss, DgL, SC)
:-
	Clss ? {_,{_,Tell,Body}}, DgL = DgH\DgT, SC = {L,R} :
	DgH ! {DgTells,DgBodies}, DgL' = DgH'\DgT, SC' = {L',R} |
             tell2(Tell, DgTells, {L,M}),
	     body#body(Body, DgBodies, {M,L'}),
	     self ;

	Clss = [], DgL = DgH\DgT, SC = {L,R} : 
        DgH = DgT, R = L.

tell2(Tell, DgTells, SC)
:-
	Tell ? TellGuard, SC = {L,R}, tuple(TellGuard),
	N := arity(TellGuard),
	make_tuple(N,DgTellTest),
	arg(1,TellGuard,Pred),
	arg(1,DgTellTest,TellPred) :
	TellPred = Pred,
	DgTells ! DgTellTest, 
	SC'={L',R} |
	     body#copytuple(TellGuard, 1, N, DgTellTest, {L,L'}),
	     self ;
	     
	Tell ? TellGuard, string(TellGuard) :
	DgTells ! TellGuard |
	     self ;

        Tell = [], SC = {L,R} : DgTells = [], R = L.
