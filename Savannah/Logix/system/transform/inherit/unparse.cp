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

-language(compound).
-export([procedures]).


procedures(Procedures, Clauses) :-

   Procedures ? Functor(0, Args, RHs) :
      Clauses ! Clause |
	lhs(Functor, 0, Args, LHS),
	rhs,
	clause,
	self;

   Procedures ? Functor(Arity, Args, RHs),
   Arity =\= 0,
   make_tuple(Arity, Head),
   arg(1, Head, Functor) :
      Clauses ! Clause? |
	lhs,
	rhs,
	clause,
	self;

   Procedures ? typedef(Declaration) :
      Clauses ! Declaration |
	self;

   Procedures ? evaluate(Directive) :
      Clauses ! Directive |
	self;

   Procedures = [] :
      Clauses = [] .
	
lhs(Functor, Arity, Args, LHS) :-

   Arity =< 0 |
	lhs1(Functor, Args, LHS);

   Arity++ > 0,
   make_tuple(Arity', Head) |
	head(Head, 0, Arity', [Functor | Args], Args'),
	lhs1(Head, Args'?, LHS).


head(Head, Index, Arity, Args1, Args2) :-

   Index++ < Arity,
   Args1 ? Arg,
   arg(Index', Head, Arg^) |
	self;

   Index >= Arity : Head = _,
      Args2 = Args1 .


lhs1(Head, Args, LHS) :-

   Args =\= [] :
      LHS = (Head + Commas?) |
	comma_list;

   Args = [] |
	LHS = Head .

comma_list(Args, Commas) :-

   Args ? X, Args' =\= [] :
      Commas = (X, Commas'?) |
	self;

   Args = [X] :
      Commas = X ;

   Args = [] :		% Bodies
      Commas = true .

clause(LHS, RHS, Clause) :-

   RHS = [] :
      Clause = LHS ;

   RHS =\= [] :
      Clause = (LHS :- RHSS?) |
	semis(RHS, RHSS).

semis(RHS, RHSS) :-

   RHS ? RH, RHS' =\= [] :
      RHSS = (RH ; RHSS'?) |
	self;

   RHS = [RH] :
      RHSS = RH .


rhs(RHs, RHS) :-

   RHs ? _([], []) :
      RHS ! true |
	self;

   RHs ? _([], Body), Body =\= [] :
      RHS ! Body''? |
	body(Body, Body'),
	comma_list(Body'?, Body''),
	self;

   RHs ? _(Guard, []), Guard =\= [] :
      RHS ! (Guard'? | true) |
	comma_list(Guard, Guard'),
	self;

   RHs ? _(Guard, Body), Guard =\= [], Body =\= [] :
      RHS ! (Guard'? | Body''?) |
	comma_list(Guard, Guard'),
	body(Body, Body'),
	comma_list(Body'?, Body''),
	self;

   RHs = [] :
      RHS = [] .
	

body(Body1, Body2) :-

   Body1 ? goal(Goal) :
      Body2 ! Goal |
	self;

   Body1 ? primitive(Goal) :
      Body2 ! Goal |
	self;

   Body1 ? rpc(A,B) :
      Body2 ! (A#B) |
	self;

   Body1 = [] :
      Body2 = [] .
