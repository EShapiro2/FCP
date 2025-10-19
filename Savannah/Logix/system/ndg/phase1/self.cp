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

-language(compound).
-scope(ndg).
-export([dgs/4,dts/4]).
-mode(trust).

Cp ::= {Dic,[{ClsId,Cls}]}.

ClsId ::= Integer.

Cls ::= {Ask,Tell,Body}.

Ix ::= {value(Psi),[{Unification,[{ClsId,Cls}]}],ClsIds} ;
       {Guard}.

ClsIds ::= [ClsId]. 

Unification ::= integer(Integer) ; string(String) ; real(Real) ; nil([]); 
	        list ; tuple/Integer.

Guard ::= {Any,Conns,Checks}.

Conns ::= [Integer].

Checks ::= [Integer].

Psi ::= psi([variable(Any)]) ; psi([Integer]).


procedure dgs(Procs, IxAlg, Dgs, SC).

dgs(Procs, IxAlg, Dgs, SC) 
:-	
	SC = {L,R} |
	make_channel(Ch, Req),
	dgs1(Procs, IxAlg, Dgs, {L,R}, Ch),
	guards#server(Req),
	close_ch(R, Ch).

close_ch(R,Ch) 
:- 
	R = done : close_channel(Ch) ;
	R = {_,done} : close_channel(Ch).

dgs1(Procs, IxAlg, Dgs, SC, Ch)
:-
	Procs ? procedure({P,A,I},Dic,F,S,Clss),
	SC = {L,R} :
	Dgs ! {{P,A,I},Dic,Dg},
	SC' = {M,R} |
	decision_graph#decision_graph({P,A}, Clss, suspend(F,S), IxAlg, Dg,
	                              Ch, {L,M}),
	dgs1 ;

	Procs = [], SC = {L,R} : Dgs = [], L = R, IxAlg = _, Ch = _.

procedure dts(Procs, IxAlg, Dgs, SC).

dts(Procs, IxAlg, Dgs, SC) 
:-
	Procs ? procedure({P,A,I},Dic,F,S,Clss),
	SC = {L,R} :
	Dgs ! {{P,A,I},Dic,Dg},
	SC' = {M,R} |
	decision_tree#decision_tree({P,A}, Clss, suspend(F,S), IxAlg, Dg,
	                              {L,M}),
	dts ;

	Procs = [], SC = {L,R} : Dgs = [], L = R, IxAlg = _.




