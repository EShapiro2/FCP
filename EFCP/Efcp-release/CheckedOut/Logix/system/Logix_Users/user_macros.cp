/*
** This module is part of EFCP.
**

     Copyright 2007 Ehud Shapiro, William Silverman
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
       User Shell default macros
       01-09-86
*/

-export([expand/2]).
-mode(trust).
-language([evaluate,compound,colon]).

CALLGOAL(Service, Path, Goal) =>
      Cs = [service(Current_Service),
	    to_context(repeat#service(Current_Service?, Service, Path)),
	    forward(Goal) | Commands]\Commands.

/*	expand/2
**
**	Expand macro commands.
**
**	expand(Command, Commands\Commands1)
**
**	->	Command  is a console command.
**	<=	Commands\Commands1  is a difference list of commands to the
**		system_macro processor and the user shell.
**
**	Commands which are not expanded here are relayed to the Logix system
**	user_macros.
*/

procedure expand(Any, [Any]\[Any]).

expand(Command, Cs) :-

    Cs = SCs\LCs :
      Cs' = Commands\[] |
	expand_logix,
	relay_derived.

  relay_derived(Commands, SCs, LCs) :-

    Commands ? Command |
	super # super # user_macros # expand(Command, SCs\SCs'),
	self;

    Commands =?= [] :
      SCs = LCs.

expand_logix(Command, Cs) :-

    unknown(Command) |
	defer_command(Command, Command, Cs);

    tuple(Command),
    arg(1, Command, V),
    unknown(V) |
	defer_command(Command, V, Cs);

    tuple(Command), arity(Command) > 1,
    arg(1, Command, M), known(M),
    arg(2, Command, V),
    unknown(V) |
	possibly_deferred;

    otherwise |
	expand_all.

  possibly_deferred(Command, M, V, Cs) :-

    M =\= "|",
    M =\= debug, M =\= profile, M =\= rpc, M =\= run, M =\= time:
      V = _ |
	expand_all;

    otherwise :
      M = _ |
	defer_command(Command, V, Cs).

  defer_command(Command, V,
		([to_context(computation # shell(defer(V, logix(Command))))
		 | Commands]\Commands)^).


expand_all(Command, Cs) :-

    Command =?= logix(Command') |
	expand_logix;

    Command =?= goal :
      Command' = goal(_) |
	self;

    Command =?= goal(No) :
      Cs = [state(No, Goal, _, _),
	    to_context(computation # display(term, Goal, [prefix(IdG), known(IdG)]))
	   | Commands]\Commands |
	utils#append_strings(['<',No,'> goal = '], IdG);

    Command =?= O/V :
      Cs = [to_context(computation # display(option, Option, NewValue, _))
	   | Commands]\Commands |
	screen_option(O, V, Option, NewValue);

    Command =?= (Id | X),
    Id >= 0 |
	computation # dictionary(find, Id, Variable, Reply),
	assign(Reply, Id, X, Variable, Cs);

    Command =?= a :
      Cs = [abort|Commands]\Commands ;
    Command =?= a(Computation) :
      Cs = [abort(Computation)|Commands]\Commands ;

    Command =?= at :
      Cs = [ service(Module),
             to_context( [Module? # attributes(A),
	                  computation#display(stream, A?, prefix(Module?))
			 ]
	     )
	   | Commands]\Commands;

    Command =?= at(Module) :
      Goal = to_context([Path? # attributes(A),
			 computation#display(stream, A?, prefix(Path?))]),
      CALLGOAL(Module, Path, Goal);


    Command =?= c :
      Cs = [compile|Commands]\Commands;
    Command =?= c(Source) :
      CALLGOAL(Source, Module, compile(Module?));

    Command =?= c(Source, Options) :
      CALLGOAL(Source, Module, compile(Module?, Options));

    Command =?= d(It) :
      CALLGOAL(It, It', debug(It'?));

    Command =?= h :
      CL = [	" logix(Command)     - execute Command",
 		" a / a(No)          - abort computation No",
		" at / at(Service)   - attributes(Service)",
		" c / c(Module)      - compile(Module)",
		" d(It)              - debug(It) (Goal or RPC)",
		" goal / goal(No)    - goal of computation No",
		" h                  - get this list",
		" i(File)            - input file",
		" l / l(Module)      - lint(Module)",
		" less(Service)      - activate:  less Service.cp",
		" more(Service)      - activate:  more Service.cp",
		" quit               - quit logix system",
		" r / r(No)          - resolvent of computation No",

		" r(Ids) / r(No, Ids)- extract Ids of resolvent of computation No",
		" re / re(No)        - resume computation No",
		" run(Service)       - start computation with Service",   
		" s / s(No)          - suspend computation No",
		" vi / vi(Module)    - edit Module",
		" O/V                - set screen option O to V",
		" N|X                - assign numbered variable to X",
		" Out!Term           - Send Term on stream or channel",
		" Out!               - close stream or channel",
		" {String}           - invoke UNIX shell with String"
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command =?= i(Path) :
      CALLGOAL(Path, Path', input(Path'?));

    Command =?= l :
      Cs = [lint | Commands]\Commands ;
    Command =?= l(Service) :
      CALLGOAL(Service, Service', lint(Service'?));
    Command =?= l(Service, Options) :
      CALLGOAL(Service, Service', lint(Service'?, Options));

    Command =?= Display(Path),
    Display =?= less :
      Cs = Commands\Commands |
	fetch_source,
	display_source;
    Command =?= Display(Path),
    Display =?= more :
      Cs = Commands\Commands |
	fetch_source,
	display_source;

    Command =?= quit :
      Cs = Commands\Commands',
      Commands ! to_context(processor # device(quit, _Ok)) ;

    tuple(Command),
    1 < arity(Command), arity(Command) < 4,
    arg(1, Command, profile) |
	meta_function(Command, Cs);

    Command =?= r :
      Cs = [resolvent | Commands]\Commands ;
    Command =?= r(Computation), integer(Computation) :
      Cs = [resolvent(Computation) | Commands]\Commands ;

    Command =?= r(Ids), "" @< Ids :
      Cs = [computation(CO), display(stream, Goals, prefix(CN)) |
		Commands]\Commands |
	utils # append_strings(["<",CO,">"], CN),
        resolvent # extract(CO, Ids, Goals);
    Command =?= r(CO, Ids) :
      Cs = [display(stream, Goals, prefix(CN))|Commands]\Commands |
	utils # append_strings(["<",CO,">"], CN),
        resolvent # extract(CO, Ids, Goals);

    Command =?= re :
      Cs = [resume|Commands]\Commands ;
    Command =?= re(Computation) :
      Cs = [resume(Computation)|Commands]\Commands ;

    tuple(Command),
    1 < arity(Command), arity(Command) < 4,
    arg(1, Command, rpc) |
	meta_function(Command, Cs);

    Command =?= run(Service) |
      CALLGOAL(Service, Service', Service'?);

    Command =?= s :
      Cs = [suspend|Commands]\Commands ;
    Command =?= s(Computation) :
      Cs = [suspend(Computation)|Commands]\Commands ;

    tuple(Command),
    1 < arity(Command), arity(Command) < 4,
    arg(1, Command, time) |
	meta_function(Command, Cs);

    Command =?= vi |
	edit(vi, _Module, Cs);

    Command =?= vi(Module) |
	repeat # path(Module, Module'),
	edit(vi, Module'?, Cs);

    Command =?= {String},
    string(String) :
      Cs = Commands\Commands |
	excecute_unix_call(true, String);

    Command =?= "_complete_names"(Head, Tail) :
      Tail = [at, goal, logix, less, more, quit, reset, unbind, vi | Tail'?],
      Cs = ["_complete_names"(Head, Tail') | Commands]\Commands;

    Command =?= "_all_functors"(Head, Tail) :
      Head' = [a, c, d, h, i, l, r, re, s, "/", "|", "!", "{ }" | Head],
      Tail = [at, goal, less, logix, more, quit, reset, unbind, vi | Tail'?],
      Cs = ["_all_functors"(Head', Tail') | Commands]\Commands;

    otherwise :
      Cs = [Command | Commands]\Commands .


assign(true, _, X, Variable, ([(Variable = X) | Commands]\Commands)^).
assign(	_, Id, _, _,
	([to_context(computation # display(term, unknown(Id)))
	 | Commands]\Commands)^
) :-
    otherwise |
	true.


edit(Editor, Module,
	([service(Module),
	  to_context(file # pwd(PWD)),
	  close(Module, Ok)
	 | Commands]\Commands
	)^
) :-
	utils # append_strings([Editor, ' ', PWD, Module, '.cp'], Command),
	excecute_unix_call(Ok, Command).


excecute_unix_call(Ok, String) :-

    Ok = true,
    known(String) |
	processor # interface(unix_call(String));

    Ok =\= true : String = _ .

screen_option(t, n, type^, namevars^).
screen_option(t, p, type^, parsed^).
screen_option(t, f, type^, freeze^).
screen_option(s, h, special^, hex^).
screen_option(s, m, special^, melted^).
screen_option(s, n, special^, none^).
screen_option(d, I, depth^, I^).
screen_option(l, I, length^, I^).
screen_option(w, I, width^, I^).
screen_option(i, I, indent^, I^).
screen_option(r, I, iterations^, I^).
screen_option(A, V, A^, V^) :-
	otherwise | true.


fetch_source(Path, Dir, Service, Ok) :-
	repeat # path(Path, Path'),
	computation_utils # call_id_goal(self # Path'?, Dir, Service, Ok).

display_source(Ok, Display, Dir, Service) :-

    Ok =?= true |
	Dir? # '_unique_id'(UID),
	utils # append_strings([Display, ' ', UID, Service?, '.cp'], String),
	excecute_unix_call(Ok, String?);

    Ok =\= true :
      Dir = _,
      Display = _,
      Service = _.

meta_function(Command, Cs) :-
    arg(2, Command, RPC) |
        tuple_to_dlist(Command, [Functor, _RPC | TheRest], []),
        list_to_tuple([Functor, Target? | TheRest], Command'),
        meta_function_command.

  meta_function_command(Command, RPC, Target, Cs) :-
    known(Command) :
      Cs = [service(Service),
            to_context(repeat#service(Service, RPC, Target, true)),
            forward(Command)
           | Commands]\Commands.
