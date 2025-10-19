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
       Translate Stochastic Pi Calculus notation to FCP.
       May 2000.
*/

-export([translate/4, new_scope/4]).
-mode(interpret).
-language([evaluate, compound, colon]).
-include(spi_constants).

/*
** Translate/4
**
** Translate Stochastic Pi Calculus module to spifcp.
**
** Input:
**
**   Source      - Stochastic Pi Calculus code, minus attributes.
**
** Output:
**
**   Spifcp      - spifcp code.
**   Errors      - Diagnostics in the form:  <prefix>(Argument)
**                 followed by NextErrors.
*/

translate(Source, Spifcp, Errors, NextErrors) :-

    true :
      Spifcp = Terms? |
	filter_spifcp_attributes(Source, Source', Terms, Terms'?),
	processes.

/* Copy Stochastic Spi Calculus Attributes. */

filter_spifcp_attributes(Source, NextSource, Terms, NextTerms) :-

    Source ? String, string(String) :
      Terms ! String |
	self;

    Source ? Tuple, Tuple =\= (_ ::= _), Tuple =\= (_ :- _) :
      Terms ! Tuple |
	self;

    otherwise :
      NextSource = Source,
      Terms = NextTerms.

/* Translate processes. */

processes(Source, Terms, Errors, NextErrors) :-

    Source ? (LHS ::= RHS) |
	spi_lhs(LHS, LHS, RHS'?, Source', Terms, Errors, Errors'?),
	spi_rhs(RHS, RHS', Errors', NextErrors);

    Source ? (LHS :- RHS) |
	logix_lhs(LHS, LHS, RHS'?, Source', Terms, Errors, Errors'?),
	logix_rhs(RHS, RHS', Errors', NextErrors);

    Source ? Other,
    otherwise :
      Errors ! invalid_process(Other) |
	self;

    Source =?= [] :
      Terms = [],
      Errors = NextErrors.

  spi_lhs(LHS, Predicate, RHS, Source, Terms, Errors, NextErrors) :-

    Predicate =?= `Functor, string(Functor),
    nth_char(1, Functor, C), CHAR_A =< C, C =< CHAR_Z :
      Terms ! (LHS :- RHS) |
	processes;

    tuple(Predicate), arity(Predicate) > 1,
    arg(1, Predicate, `Functor), string(Functor),
    nth_char(1, Functor, C), CHAR_A =< C, C =< CHAR_Z :
      Terms ! (LHS :- RHS) |
	processes;

    Predicate =?= (Predicate' + _Parameters), LHS =?= Predicate |
	spi_lhs;

    otherwise :
      Predicate = _,
      RHS = _,
      Errors ! invalid_left_hand_side(LHS) |
	processes.

  logix_lhs(LHS, Predicate, RHS, Source, Terms, Errors, NextErrors) :-

    string(Predicate),
    nth_char(1, Predicate, C), CHAR_a =< C, C =< CHAR_z :
      Terms ! (LHS :- RHS) |
	processes;

    tuple(Predicate), arity(Predicate) > 1,
    arg(1, Predicate, Functor), string(Functor),
    nth_char(1, Functor, C), CHAR_a =< C, C =< CHAR_z :
      Terms ! (LHS :- RHS) |
	processes;

    Predicate =?= (Predicate' + _Parameters), LHS =?= Predicate |
	logix_lhs;

    otherwise :
      Predicate = _,
      RHS = _,
      Errors ! invalid_left_hand_side(LHS) |
	processes.

logix_rhs(RHS, Logix, Errors, NextErrors) :-

    RHS =?= (Clause ; RHS') :
      Logix = (Clause' ; Logix') |
	logix_rhs(Clause, Clause', Errors, Errors'?),
	self;

    RHS =?= (_Ask : _Tell) :
      Logix = (RHS | true),
      Errors = NextErrors;

    RHS =?= (Guard | Body),
    Body =\= (_ | _),
    Guard =?= (_Ask : _Tell) :
      Logix = RHS,
      Errors = NextErrors;

    RHS =?= (Ask | Body),
    Body =\= (_ | _),
    Ask =\= (_ : _) :
      Logix = (Ask : true | Body),
      Errors = NextErrors;

    RHS =\= (_ : _),
    RHS =\= (_ | _) :
      Logix = (true : true | RHS),
      Errors = NextErrors;

    otherwise :
      Logix = true,
      Errors = [invalid_logix_rhs(RHS) | NextErrors].

spi_rhs(PC, Spifcp, Errors, NextErrors) :-

    PC =?= (Clause ; PC') :
      Spifcp = (Clause' ; Spifcp') |
	spi_rhs(Clause, Clause', Errors, Errors'?),
	self;

    /* Just parallel goals */
    PC =?= (Goal | PC'), PC' =\= (_, _), PC' =\= (_, _ | _),
    Goal =\= (_ , _) :
      Spifcp = (Goal'?, Spifcp') |
	spi_rhs(Goal, Goal', Errors, Errors'?),
	self;

    PC =?= (_ | (_,_|_)) :
      Spifcp = true,
      Errors = [invalid_body(PC) | NextErrors];

    PC =?= (_ | PC'), PC' =?= (_, _) :
      Spifcp = true,
      Errors = [invalid_body(PC) | NextErrors];

    /* Possibly compound guard, and single goal */
    PC =?= (_, _) |
	process_guard_goal;

    /* Possibly compound guard, and at least two parallel goals */
    PC =?= (Head | PC'),
    Head =?= (_, _) |
	process_guard_goals;

    PC =?= 0 :
      Spifcp = true,
      Errors = NextErrors;

    otherwise :
      Spifcp = PC,
      Errors = NextErrors.


compound_guard(Guard, Body, Goal, RHS, Errors, NextErrors) :-

    Guard =?= (Predicate, Guard'),
    Predicate =\= (_ | _) :
      RHS = (Predicate | RHS') |
	self;

    Guard =?= (Predicate, _),
    Predicate =?= (_ | _) :
      Goal = 0,
      RHS = Body,
      Errors = [invalid_guard(Predicate) | NextErrors];

    Guard =\= (_, _) :
      Goal = Guard,
      RHS = Body,
      Errors = NextErrors.

process_guard_goal(PC, Spifcp, Errors, NextErrors) :-

    PC = (Guard, Compound),
    Compound = (_, _) :
      Spifcp = (Guard | [Compound]),
      Errors = NextErrors;

    PC = (Guard, Compound),
    Compound = (_ ; _) :
      Spifcp = (Guard | [Compound]),
      Errors = NextErrors;

    PC = (Guard, PC'),
    PC' =\= (_, _),
    PC' =\= (_ ; _) :
      Spifcp = (Guard | Spifcp'?) |
	spi_rhs.

process_guard_goals(Head, PC, Spifcp, Errors, NextErrors) :-

    Head = (Guard, Compound),
    Compound = (_, _) :
      Spifcp = (Guard | [(Compound | PC)]),
      Errors = NextErrors;

    Head = (Guard, Compound),
    Compound = (_ ; _) :
      Spifcp = (Guard | [(Compound | PC)]),
      Errors = NextErrors;

    Head =?= (Guard, Goal),
    Goal =\= (_, _),
    Goal =\= (_ ; _),
    PC =\= (_, _) :
      PC' = (Goal | PC),
      Spifcp = (Guard | Spifcp'?) |
	spi_rhs;

    otherwise :
      Spifcp = 0,
      Errors = [invalid_rhs((Head | PC)) | NextErrors].


new_scope(List, New, Errors, NextErrors) :-

    List =?= [RHS], RHS =\= (_ ::= _), RHS =\= (_ :- _) :
      New = [RHS'?] |
	spi_rhs(RHS, RHS', Errors, NextErrors);

    List =?= [Body | Processes],
    Body =\= (_ ::= _), Body =\= (_ :- _), Processes =?= [(_ ::= _)| _] :
      New = [Body'? | Processes'?] |
	spi_rhs(Body, Body', Errors, Errors'),
	processes(Processes, Processes', Errors', NextErrors);

    List =?= [Body | Processes],
    Body =\= (_ ::= _), Body =\= (_ :- _), Processes =?= [(_ :- _)| _] :
      New = [Body'? | Processes'?] |
	spi_rhs(Body, Body', Errors, Errors'),
	processes(Processes, Processes', Errors', NextErrors);

    List =?= [Channels, Body | Processes],
    Channels =\= (_ ::= _), Body =\= (_ ::= _),
    Channels =\= (_ :- _), Body =\= (_ :- _) :
      New = [Channels, Body'? | Processes'?] |
	spi_rhs(Body, Body', Errors, Errors'),
	processes(Processes, Processes', Errors', NextErrors);

    List =?= [(_ ::= _)| _] :
      New = Processes |
	processes(List, Processes, Errors, NextErrors);

    List =?= [(_ :- _)| _] :
      New = Processes |
	processes(List, Processes, Errors, NextErrors).
