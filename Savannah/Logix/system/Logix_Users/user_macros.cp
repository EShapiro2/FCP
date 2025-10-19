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

	Expand macro commands.

	expand(Command, Commands\Commands1)

	->	Command  is a console command.
	<=	Commands\Commands1  is a difference list of commands to the
		system_macro processor and the user shell.

	Commands which are not expanded here are relayed to the Logix system
	user_macros.
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
    M =\= rpc, M =\= run, M =\= time :
      V = _ |
	expand_all;

    otherwise :
      M = _ |
	defer_command(Command, V, Cs).

  defer_command(Command, V, ([shell(defer(V, Command)) | Commands]\Commands)^).


expand_all(Command, Cs) :-

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

    Command =?= Service - G1#G2 :
      Command' = Service - (G1#G2) |
	self;

    Command =?= Service - Goal, Command =\= _ - _#_ :
      CALLGOAL(Service? # Goal, Service', to_context(Service'?));

    Command =?= - G1#G2 :
      Command' = - (G1#G2) |
	self;

    Command =?= - Goal, Command =\= - _#_ |
	CALLGOAL(#Goal, Call, to_context(Call?));

    Command =?= (Id | X),
    Id >= 0 |
	computation # dictionary(find, Id, Variable, Reply),
	assign(Reply, Id, X, Variable, Cs);

    Command =?= a :
      Cs = [abort|Commands]\Commands ;
    Command =?= a(Computation) :
      Cs = [abort(Computation)|Commands]\Commands ;

    Command =?= at |
	service_attributes(_Service, Cs);
    Command =?= at(Service),
      known(Service) |
	repeat # path(Service, Service'),
	service_attributes(Service'?, Cs);
    Command =?= at(Service),
      unknown(Service) |
	repeat # synchronize(Service, Reply),
	wait_at_sync(Reply, Cs);

    Command =?= c :
      Cs = [compile|Commands]\Commands;
    Command =?= c(Source) :
      CALLGOAL(Source, Module, compile(Module?));

    Command =?= c(Source, Options) :
      CALLGOAL(Source, Module, compile(Module?, Options));

    Command =?= d(It) :
      CALLGOAL(It, It', debug(It'?));

    Command =?= h :
      CL = [	" a / a(No)          - abort computation No",
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
		" Service - Goal     - call Service#Goal",
		" - Goal             - call Current#Goal",
		" {String}           - invoke UNIX shell sh with String"
	 ],
      Cs = [to_context(computation # display(stream,CL)) | Commands]\Commands ;

    Command =?= i(Path) :
      CALLGOAL(Path, Path', input(Path'));

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

    Command =?= run(Service) |
      CALLGOAL(Service, Service', Service'?);

    tuple(Command),
    1 < arity(Command), arity(Command) < 4,
    arg(1, Command, rpc) |
	timing(Command, Cs);

    Command =?= s :
      Cs = [suspend|Commands]\Commands ;
    Command =?= s(Computation) :
      Cs = [suspend(Computation)|Commands]\Commands ;

    tuple(Command),
    1 < arity(Command), arity(Command) < 4,
    arg(1, Command, time) |
	timing(Command, Cs);

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
      Tail = [at, goal, less, more, quit, reset, unbind, vi | Tail'?],
      Cs = ["_complete_names"(Head, Tail') | Commands]\Commands;

    Command =?= "_all_functors"(Head, Tail) :
      Head' = [a, c, d, h, i, l, r, re, s,"/", "-", "|", "!", "{ }" | Head],
      Tail = [at, goal, less, more, quit, reset, unbind, vi | Tail'?],
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


wait_at_sync(Reply, Cs) :-

    Reply =?= Type(Service),
    Type =\= "_var", Type =\= "_ro" |
	service_attributes(Service, Cs);

    Reply =?= _Type(Service),		/* _var or _ro */
    otherwise :
      Cs = [defer(Service, at(Service)) | Commands]\Commands.

service_attributes(New, ([service(New, Old),
      to_context(repeat#service(Old, New, New')),
      forward(computation_utils # call_list([New'? # attributes(A)], Reply)),
      forward(shell(display_stream(A, prefix(New'))))
			 | Commands]\Commands)^)
:-
	display_service_attributes(Reply, A, New', Old).

  display_service_attributes(Reply, A, New, Old) :-
    Reply =?= true :
      A = _,
      Old = _ |
	computation#shell(service(Update?, Current)),
	update_service;
    otherwise :
      New = _,
      Reply = A,
      New' = Old |
	computation#shell(service(Update?,Current)),
	update_service.

  update_service(Current, Update, New) :-
    Current =?= computation_utils :
      Update = New;
    otherwise :
      New = _,
      Update = Current.
  

fetch_source(Path, Dir, Service, Ok) :-
	repeat # path(Path, Path'),
	computation_utils # call_id_goal(self # Path'?, Dir, Service, Ok).

display_source(Ok, Display, Dir, Service) :-

    Ok =?= true |
	Dir? # '_unique_id'(UID),
	utils # append_strings([Display, ' ', UID, Service?, '.cp'], String),
	excecute_unix_call(true, String);

    Ok =\= true :
      Dir = _,
      Display = _,
      Service = _.


timing(Command, Cs) :-

    Command =?= Process(RPC) :
      Out = _,
      Display = true |
	remote_call(RPC, RPC', Service1, Service2),
	timer;

    Command =?= Process(RPC, Out) :
      Display = false |
	remote_call(RPC, RPC', Service1, Service2),
	timer.

timer(Process, RPC, Display, Out, Service1, Service2, Cs) :-
    RPC = _#_ :
      Cs = [service(Service1),
	    to_context(repeat#service(Service1, RPC, RPC', true)),
	    forward(Process(RPC', configure(Display, No, Out))),
	    new_goal(No, Process(RPC', Out)),
	    service(Service2)
	   | Commands]\Commands.
