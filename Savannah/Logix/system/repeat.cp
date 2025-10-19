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
** EFCP is distributed in the hope that  t will be useful,
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
       Run multiple procedures.
       2003
*/

-language([evaluate,compound,colon]).
-mode(interrupt).
-export([list/2, list/3, path/2, path/3, run/1, run/2, service/3, service/4,
	 synchronize/2, synchronize/3]).
-include(spi_constants).

/*
** Input
**
** Service - the implied server name of calls of the form - #Goal .
**
** Source - a <quantified_process_set> : see www.nongnu.org/efcp/Logix/run.txt
**
** NameGoals - "true" or "false", specifying whether a variable name should
**             be treated as a literal <service_name> or <predicate functor>
**             or as an uninstantiated variable .
**
**   see "More About Logix" : "Logix Syntax, Semantics and Services" :
**                      "Repeat Macro and Service"
**
** The default for NameGoals is "true". 
**
** Output (from list)
**
** List - a <call_list>, a (nested) list of elements of the form:
**
**        <goal>
**          or
**        <service_name> # <call_list>
**
** Output (from path, service)
**
** Path   a <remote_procedure call> or a list of them:
**
**        <goal>
**          or
**        <service_name> # <path>
**
** where <path> is a <goal> or recursively <service_name> # <path>
**
** 
** For elements of Path specified by a variable or by a tuple whose first
** argument is a variable, NameGoals = "true" and the (string) name of the
** variable begins with an upper-case letter, the variable is replaced by
** its name.
*/

SERVICE => "***".	/* reserved */
SYNCHRONIZE(Argument) => {{Count, Result}, {Count', Argument}}.


run(Source) + (NameGoals = true) :-
	path,
	computation # Path?.


path(Source, Path) + (NameGoals = true) :-
	computation # shell(service(Service)),
	service.


service(Service, Source, Path) + (NameGoals = true) :-

    unknown(Source) :
      Variable = Source,
      Source' = Result? |
	synchronize(Source, Reply),
	service1;

    Source =?= Variable # Goal,
    unknown(Variable) :
      Source' = Result? # Goal |
	synchronize(Variable, Reply),
	service1;

    otherwise |
	service3.

  service1(Service, Source, Path, NameGoals, Variable, Reply, Result) :-

    NameGoals =?= true,
    Reply =?= Kind(Value),
    Kind =\= "_var", Kind =\= "_ro" :
      Variable = _,
      Result = Value |
	service3;

    NameGoals =?= false,
    Reply = _Kind(_Value),
    we(Variable) :
      Reply = _,
      Variable = Service?,
      Result = Service? |
	service3;

    Reply =?= Var(Value),
    Var = "_var" :
      Variable = _,
      Value = Service?,
      Result = Service? |
	service3;

    otherwise :
      Reply = _,
      Result = Variable |
	service3.

  service3(Service, Source, Path, NameGoals) :-
	list,
	normalize.


list(Source, List) + (NameGoals = true) :-
	set(1, Source, List, [], NameGoals).


synchronize(Variable, Reply) + (Frozen = _) :-
	computation # dictionary(
		freeze(Variable, Frozen, 1, 1000, ignore, parsed)),
	varname.

 varname(Variable, Frozen, Reply) :-

    unknown(Variable),
    arg(2, Frozen, Name),
    string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      Reply = name(Name);

    unknown(Variable),
    otherwise,
    arg(1, Frozen, Type) :
      Reply = Type(Variable);

    known(Variable) :
      Frozen = _,
      Reply = known(Variable) |
	true.
%	computation#display(term, (variable(Variable): frozen(Frozen))). 


set(Count, Source, List, NextList, NameGoals) :-

    Count > 0,
    Source ? Set |
	set + (Source = Set),
	set(Count, Source', NextList', NextList, NameGoals);

    Count > 0,
    Source =?= (Source', Set) |
	set,
	set(Count, Set, NextList', NextList, NameGoals);

    Count > 0,
    Source =?= N*Source',
    integer(N),
    Count' := Count*N |
	set;

    Count > 0,
    NameGoals =?= true,
    unknown(Source) |
	unknown_item(Source, SYNCHRONIZE(Source'), Result),
	set_unknown + (Retry = Source');

    Count > 0,
    NameGoals =?= true,
    tuple(Source),
    arity(Source) > 1,
    arg(1, Source, Functor),
    unknown(Functor) |
	unknown_item(Functor, SYNCHRONIZE(Functor'), Result),
	utils # tuple_to_dlist(Source, [_Functor | Args], []),
	utils # list_to_tuple([Functor' | Args], Goal),
	set_unknown + (Source = Functor', Retry = Goal);


    Count > 0,
    NameGoals =?= true,
    Source =?= Target # Goal,
    unknown(Target) |
	unknown_item(Target, SYNCHRONIZE(Target'), Result),
	set_unknown + (Source = Target', Retry = Target'#Goal);


    Count > 0,
    Source =?= N*Target#Goal :
      Source' = N*(Target#Goal) |
	set;

    Count > 0,
    Source =?= #Goal :
      Source' = SERVICE#Goal |
	set;

    Count > 0,
    Source =?= Target#Source',
    known(Target),
    Source =\= _*_#_ :
      List = [Target#List'? | NextList] |
	set + (NextList = []);

    Count-- > 0,
    otherwise,
    Source =\= [] :
      List ! Source |
	set;

    otherwise :
      Count = _,
      Source = _,
      NameGoals = _,
      List = NextList .

  set_unknown(Count, Source, List, NextList, NameGoals, Retry) :-
    known(Source) :
      Source' = Retry |
	set.

/*
** unknown_item: inspect Variable, and return the variable itself or its name.
*/

unknown_item(Variable, Synchronize, Result) :-
	synchronize + (Frozen = _),
	wait_item.

  wait_item(Reply, Result, Synchronize) :-
    Reply = _Kind(R),
    Synchronize = {S1, S2} :
      Result = R,
      S1 = S2.

/*
** normalize: replace SERVICE with specified/acquired Service;
** flatten List; remove redundant list brackets.
*/

normalize(Service, List, Path) :-

    List =?= SERVICE # Goal :		% This clause recognizes (transformed)
      Path = Service? # Goal' |		% Source terms of the form : #Goal .
	normalize(Service, Goal, Goal');

    List =?= Target # Goal,
    Target =\= SERVICE :
      Path = Target' # Goal' |
	normalize(Service, Target, Target'),
	normalize(Service, Goal, Goal');

    List =?= [Target] :			% Remove redundant brackets.
      List' = Target |
	self;

    list(List), List =\= [_] |
	normalize_list;

    otherwise :
      Service = _,
      Path = List.

normalize_list(Service, List, Path) :-

    List ? Item :
      Path ! Item' |
	normalize(Service, Item, Item'),
	self;

    otherwise :
      Service = _,
      Path = List.
