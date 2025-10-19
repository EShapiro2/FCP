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
       Transform/Run multiple procedures.
       2003
*/

-language([evaluate,compound,colon]).
-mode(interrupt).
-export([list/2, list/4, path/2, path/3, run/1, run/2, service/3, service/4,
	 synchronize/2, synchronize/3]).
-include(spi_constants).

/*
** Input
**
** Service - the (initial) implied server name of calls of the form - #Goal .
**
** Source - a (comma'ed list of) <quantified_process_set> :
**	    see www.nongnu.org/efcp/Logix/run.txt
**
** NameGoals - "true" or "false", specifying whether a variable name should be
**             treated as a literal(<service_name> or <predicate functor>) or
**             as an uninstantiated variable .
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
** Path   a <remote_procedure call> or a (comma'ed) list of them:
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

SERVICE => "***".
SYNCHRONIZE(Argument) => {{Count, Result}, {Count', Argument}}.


run(Source) + (NameGoals = true) :-
	path,
	run_path.

  run_path(Path) :-

    Path =?= (P1, P2) |
	run_path(P1), run_path(P2);
    
    otherwise |
	computation # Path.

/*
** Given Source, get current Service and wait for it to be instantiated as a
** string.
*/

path(Source, Path) + (NameGoals = true) :-
	computation # shell(service(Service)),
	service_known.

/*
** Given Service and Source, verify Service and wait for it to be instantiated
** as a string.
*/

service(Service, Source, Path) + (NameGoals = true) :-
	target(NameGoals, true, Service, Service', _, _, 1, _),
	service_known.

/*
** Wait for Service to be instantiated as a string, verify Source and wait for
** it to be known.
*/

service_known(Service, Source, NameGoals, Path) :-
    string(Service) |
	target(NameGoals, true, Source, Source', Service, _, initial, _),
	source_known.

/*
** Wait for Source to be known, transform it into List and clean up List,
** producing Path.
*/

source_known(Service, Source, NameGoals, Path) :-
    known(Source) |
	list,
	normalize.

/*
** list:	Detect, apply and remove multipliers;
** 		Apply and advance Service for subsequent #<term> and _#<term>.
*/

list(Source, List) + (Service = SERVICE, NameGoals = true) :-
    string(Service) |
	set(Source, NameGoals, 1, true, Service, _NextService, List, []).

/*
** Iterate walk of Source, cumulate Count (product), provide Variable names,
** insert implicit, targets, distribute Counts, expand Items with Count > 1.
*/

set(Source, NameGoals, Count, Base, Service, NextService, List, NextList) :-

    Count > 0 |
	set1;

    Count =< 0 :
      Base = _,
      NameGoals = _,
      Source = _,
      Service = NextService,
      List = NextList .

  set1(Source, NameGoals, Count, Base, Service, NextService, List, NextList) :-

    Source ? Set |
	set1(Set, NameGoals, Count, true, Service, Service', List, List'), 
	set1(Source', NameGoals, Count, Base, Service'?, NextService,
		      List', NextList
	);

    Source =?= (Set, Source') |
	set1(Set, NameGoals, Count, true, Service, Service', List, List'), 
	set1(Source', NameGoals, Count, Base, Service'?, NextService,
		      List', NextList
	);

    Source =?= N*Source',
    integer(N),
    Count' := Count*N |
	set;

    NameGoals =?= true,
    unknown(Source) |
	unknown_item(Source, SYNCHRONIZE(Source'), Result),
	unknown_term(Source', Result, Count', Count''),
	set;

    NameGoals =?= true,
    tuple(Source),
    arity(Source) > 1,
    arg(1, Source, Functor),
    unknown(Functor) |
	unknown_item(Functor, SYNCHRONIZE(Functor'), Result),
	utils # tuple_to_dlist(Source, [_Functor | Args], []),
	utils # list_to_tuple([Functor' | Args], Source'),
	unknown_functor(Functor', Source', Count', Count''),
	set;

    Source =?= Target # Goal,
    we(Target) :
      Source' = Target'? # Goal |
	target(NameGoals, Base, Target, Target', Service, Service',
				Count, Count'
	),
	set;

    Source =?= N*Target#Goal :
      Source' = N*(Target#Goal) |
	set1;

    Source =?= #Goal :
      Source' = Service#Goal |
	set1;

    Source =?= Target#Source',
    Source =\= _*_#_,
    Target =\= [_|_] :	/* and is instantiated - check for string */
      List = [Target'? # List' | NextList] |
	target(NameGoals, Base, Target, Target', Service, Service',
				Count, Count'
	),
	set1 + (Base = false, NextList = []);

    Source =?= Target#Source',
    Source =\= _*_#_,
    Target =?= [_|_] :
      Base = _,
      List = [Target' # List' | NextList] |
	set1(Target, NameGoals, 1, true, _, _, Target', []), 
	set1 + (Base = false, NextList = []);

    otherwise,
    Count--,
    Source =\= [] :
      List ! Source |
	set;

    otherwise :
      Base = _,
      Count = _,
      Source = _,
      NameGoals = _,
      List = NextList,
      Service = NextService.

        
  unknown_term(Term, Retry, Count, Count') :-
    known(Count),
    known(Term) :
      Term = Retry,
      Count = Count'.

  unknown_functor(Term, Retry, Count, Count') :-
    known(Count),
    known(Term),
    arg(1, Retry, Functor) |
      Term = Functor,
      Count = Count'.

/*
** Extract (writable) Variable name and if suitable, return as kind "name";
** otherwise, return kind "_var" or "notwe" (not writable).
*/

synchronize(Variable, Reply) + (Frozen = _) :-
	computation # dictionary(
	    freeze(Variable, Frozen, 1, MAX_STRING_LENGTH, ignore, parsed)),
	varname.

  varname(Variable, Frozen, Reply) :-

    we(Variable),
    arg(2, Frozen, Name),
    string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      Reply = name(Name);

    we(Variable),
    otherwise,
    arg(1, Frozen, Type) :
      Reply = Type(Variable);

    otherwise :
      Frozen = _,
      Reply = notwe(Variable) |
	true.

/*
** Determine the kind of the Target, and if named return the name and update
** Service; if writable and not named, unify it with Service and return it;
** otherwise, wait until it is instantiated and return it and update Service -
** a failure diagnostic is signaled if the instantiated value is not a string.
*/

target(NameGoals, Base, Target, Target'', Service, NextService,
			Count, Count'''
) :-

    known(Count),
    we(Target),
    NameGoals =?= false :
      Target'' = Target'? |
	target_not_named(Base, Target, Target', Service, NextService,
				Count, Count'''
	);

    known(Count),
    we(Target),
    NameGoals =\= false :
      Target'' = Target'? |
        unknown_item(Target, SYNCHRONIZE(Target'), Result),
	set_unknown_target(Target, Target', Count', Count''),
	target_not_named(Base, Target, Target', Service, NextService,
				Count'', Count'''
	);

    known(Count),
    otherwise :			% Target is read-only or instantiated
      NameGoals = _ |
	target_not_named(Base, Target, Target'', Service, NextService,
				Count, Count'''
	).

 target_not_named(Base, Target, Target', Service, NextService, Count, Count') :-

    known(Count),
    we(Target),
    we(Target'),
    Base =?= true :
      Service = NextService,
      Target' = Service,
      Count = Count';

    known(Count),
    we(Target),
    we(Target'),
    Base =?= false :
      Service = NextService,
      Target' = Target,
      Count = Count';

    known(Count),
    we(Target),
    string(Target'),
    Base =?= true :
      Service = _,
      NextService = Target',
      Count = Count';

    known(Count),
    we(Target),
    string(Target'),
    Base =?= false :
      Service = NextService,
      Count = Count';

    string(Target),
    Base =?= true :
      Service = _,
      Target = Target',
      Target' = NextService,
      Count = Count';

    string(Target),
    Base =?= false :
      Service = NextService,
      Target = Target',
      Count = Count';

    list(Target) :
      Base = _,
      Service = NextService,
      Target = Target',
      Count = Count';

    otherwise,
    known(Target),
    Count = initial :
      Base = _,
      Service = NextService,
      Target = Target',
      Count = Count';

    otherwise,
    known(Target) :
      Base = _,
      Count = _,
      Count' = _,
      NextService = _,
      Service = _,
      Target' = _ |
        fail(not_a_string(Target)).

  set_unknown_target(Target, Target', Count, Count') :-

    known(Count),
    we(Target') :
      Target' = Target,
      Count = Count';

    known(Count),
    known(Target') :
      Target = _,
      Count = Count'.


/*
** unknown_item: inspect Variable, and return the variable itself or its name.
*/

unknown_item(Variable, Synchronize, Result) :-
	synchronize + (Frozen = _),
	wait_item.

  wait_item(Reply, Synchronize, Result) :-

    Reply =?= Kind(R), Kind =\= name,
    Synchronize = {S1, S2} :
      Result = R,
      S1 = S2;

    Reply =?= name(R),
    string(R),
    Synchronize = {{Count, Result}, {Count', Argument}} :
      Argument = R,
      Result = R,
      Count = Count';

    otherwise,			/* This case should not occur */
    Reply =?= name(R),
    Synchronize = {S1, S2} |
% computation#display(term, wait_item(Reply, Synchronize, Result)),
        unify_without_failure(Result, R),
        unify_without_failure(S1, S2).

/*
** normalize:	Replace base bracketed list by comma'ed list as needed;
** 		flatten List;
** 		remove redundant list brackets; 
*/

normalize(Source, List, Path) :-

    list(List),
    Source =?= (_,_) |		% Source is a comma'ed list.
	normalize_item(List, List'), 
	convert_list(List', Out, Out, Path);

    list(List),
    Source =?= _*(_,_) |	% Source is a multiplied , comma'ed list.
	normalize_item(List, List'), 
	convert_list(List', Out, Out, Path);

    List =?= [List'],
    Source =\= [_] |		% The brackets are an artifact of set.
	self;

    List =?= [List'],		% Source is a named variable;
    unknown(Source) |		% List' is its name.
	normalize_item;

    otherwise :			% Source was (also) a 1-element list. 
      Source = _ |
	normalize_item.

  convert_list(In, Out, Complete, Path) :-

    In ? Item,
    list(In') :
      Out = (Item, Out') |
	self;

    In =?= [Item] :
      Item = Out,
      Complete = Path;

    otherwise :    
      In = Out,
      Complete = Path.

  normalize_item(List, Path) :-

    list(List),
    List =?= [S#G] :
      List' = S#G |
	self;

    list(List),
    List =\= [_#_] |
	normalize_list;

    List =?= Target # [Goal] :		% Remove redundant brackets.
      List' = Target # Goal |
	self;

    List =?= Target # Goal,
    Goal =\= [_] :
      Path = Target' # Goal' |
	normalize_item(Target, Target'),
	normalize_item(Goal, Goal');

    otherwise :
      Path = List.

  normalize_list(List, Path) :-

    List ? Item :
      Path ! Item' |
	normalize_item(Item, Item'),
	self;

    otherwise :
      Path = List.
