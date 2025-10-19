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
       User Ambient macros
       1998
*/

-export([expand/2]).
-mode(trust).
-language([evaluate,compound,colon]).
-include(spi_constants).

/*	expand/2

	Expand macro commands.

	expand(Command, Commands\Commands1)

	->	Command  is a console command.
	<=	Commands\Commands1  is a difference list of commands to the
		system-macro processor and the user shell.

	Commands which are not expanded here are relayed to SpiFcp_Users
	user_macros.
*/

procedure expand(Any, [Any]\[Any]).

expand(Command, Cs) :-

    Cs = SCs\LCs :
      Cs' = Commands\[] |
        expand_biospi,
        relay_derived.

  relay_derived(Commands, SCs, LCs) :-

    Commands ? Command |
        super # super # user_macros # expand(Command, SCs\SCs'),
        self;

    Commands =?= [] :
      SCs = LCs.

expand_biospi(Command, Cs) :-

    unknown(Command) |
	defer_command(Command, Command, Cs);

    tuple(Command),
    arg(1, Command, V),
    unknown(V) |
	defer_command(Command, V, Cs);

    tuple(Command),
    arg(1, Command, M), arg(2, Command, V),
    known(M), unknown(V) |
	deferred_macros;

    otherwise |
	expand_all.

  deferred_macros(Command, M, V, Cs) :-

    M =\= atrace, 
    M =\= record, M =\= rpc, M =\= run,
    M =\= trace, M =\= time :
      V = _ |
	expand_all;

    otherwise :
      M = _ |
	defer_command(Command, V, Cs).

  defer_command(Command, V, ([shell(defer(V, Command)) | Commands]\Commands)^).


expand_all(Command, Cs) :-

% BioSpi  macros

    Command =?= ph :
      Command' = ph(true) |
	self;

    Command =?= ph(PhReady) :
      CL = [	" atr / atr(Ambient) - display ambient tree",
		" btr / btr(Ambient) - ambient tree with busy channels",
		" ctr / ctr(Ambient) - ambient tree with communicating channels",
		" rtr / rtr(Ambient) - ambient tree with resolvent",
		" spc(C)             - Show BioSpi channel",
		" atrace(Gs, F, L, S)- run Goals, trees to File until Limit,",
                "                      scaled by Scale.",
		""
	 ],
      Cs = [to_context(computation # display(stream, CL,
			[wait(PhReady),	close(BhReady, true)])), ph(BhReady)
	   | Commands]\Commands;

    tuple(Command),
    arg(1, Command, atrace), arity(Command) > 1 |
	utils # tuple_to_dlist(Command, [_, Goals | TheRest], []),
	utils#list_to_tuple([run, Run | TheRest], Runner),
	ambient_list(Goals, Run, ambient_list#Runner?, Cs);

/* Override spifcp and shell_server: system_macros */

    tuple(Command),
    arg(1, Command, rpc),
    arity(Command, Arity),
    2 =< Arity, Arity =< 4 |
	ambient_timing;

    tuple(Command),
    arg(1, Command, time),
    arity(Command, Arity),
    2 =< Arity, Arity =< 4 |
	ambient_timing;

    tuple(Command),
    arg(1, Command, trace), arity(Command) > 1 |
	utils # tuple_to_dlist(Command, [_, Goals | TheRest], []),
	utils#list_to_tuple([run, Run | TheRest], Runner),
	ambient_run(Goals, Run, _Control, _Events, _No, spi_trace#Runner?, Cs);

    Command =?= run(Goals) :
      Command' = run(Goals, []) |
	self;

    Command =?= run(Goals, Arg) :
      Command' = run(Goals, File, Limit) |
	choose_argument(Arg, Limit, TIME_LIMIT_VALUE, File, []),
	self;

    Command =?= run(Goals, File, Limit) :
      Command' = run(Goals, File, Limit, "%.7G") |
	self;

    Command =?= run(Goals, File, Limit, Arg) |
	choose_argument(Arg, Scale, 1.0, Sprint, "%.7G"),
	ambient_run(Goals, Run, _Control, Events, _No,
	  spi_record#run(Run, File, Limit, Sprint, none, Scale, Events), Cs);

    Command =?= run(Goals, File, Limit, Arg, Format) |
	choose_argument(Arg, Scale, 1.0, Sprint, "%.7G"),
	ambient_run(Goals, Run, _Control, Events, _No,
	  spi_record#run(Run, File, Limit, Sprint, Format, Scale, Events), Cs);
/*
** Obsolescent
*/
    tuple(Command),
    arg(1, Command, record), arity(Command) > 1 |
	utils # tuple_to_dlist(Command, [_ | TheRest], []),
	utils # list_to_tuple([run | TheRest], Command'),
	self;

% override spifcp user macros.

    Command =?= (#_) |
	ambient_run(Command, Run, _Control, _Events, _No, Run, Cs);

    Command =?= (_#_) |
	ambient_run(Command, Run, _Control, _Events, _No, Run, Cs);

    /* Obsolescent */
    Command =?= rtr :
      Command' = rtr("") |
	self;

    Command =?= rtr(Selector) :
      Cs = [state(No,Goal,_,_),
	    to_context([utils # append_strings(["<",No,">"], IdG),
			spi_monitor # options(Options, Options),
			spi_utils#Requests?,
			spi_debug # stream_out(Out?, IdG, Commands, Commands1)]),
			ExtraCommand | Commands]\Commands1,
      Aux = resolvent(Options, Requests, []) |
	ambient_tree;

    /***************/

    string_to_dlist(Command,[X, CHAR_t, CHAR_a], []),
    CHAR_b =< X, X =< CHAR_d :
      Cs = [state(No,_,_,_),
            to_context([utils # append_strings(["<",No?,"> "], IdG),
                        spi_monitor # status(Status),
                        spi_debug # spi_channels(X, Status?, Out),
                        spi_debug # stream_out(Out?, IdG?, Commands, Commands1)
                       ])
           | Commands]\Commands1;

    string_to_dlist(Command,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      Command' = Command("") |
        self;

    Command =?= Xtr(Selector),
    string_to_dlist(Xtr,[X, CHAR_t, CHAR_r], []),
    CHAR_a =< X, X =< CHAR_d :
      ExtraCommand = _,
      Aux = channels(X, [], []),
      Cs = [state(No,Goal,_,_),
            to_context([utils # append_strings(["<",No,">"], IdG),
                        spi_debug # stream_out(Out?, IdG, Commands, Commands1)
                       ])
           | Commands]\Commands1 |
        ambient_tree;

/* Override logix user_macros */

/************* Signals ****************/

    Command =?= r :
      Command' = resolvent("") |
	self;
    Command =?= r(Selector) :
      Command' = resolvent(Selector) |
	self;
    Command =?= resolvent :
      Command' = resolvent("") |
	self;
    Command =?= resolvent(Selector) :
      Cs = [state(No, Goal, _, _),
	    forward(request(suspend, No?, Suspended)),
	    to_context([utils # append_strings(["<",No,">"], IdG),
			spi_monitor # options(Options, Options),
			spi_utils#Requests?,
		 	spi_debug # stream_out(Out?, IdG, Commands, Commands1)]),
		ExtraCommand | Commands]\Commands1,
      Aux = resolvent(Options, Requests, []) |
	ambient_resolvent;

/************* help macro *****************/

    Command =?= "_complete_names"(Head, Tail) :
      Tail = [atrace, forward, rtr | Tail'],
      Cs = ["_complete_names"(Head, Tail') | Commands]\Commands;

    Command =?= "_all_functors"(Head, Tail) :
      Head' = [atr, btr, ctr, dtr | Head],
      Tail = [atrace, forward, rtr | Tail'],
      Cs = ["_all_functors"(Head', Tail') | Commands]\Commands;

    otherwise :
      Cs = [Command | Commands]\Commands .


ambient_resolvent(Selector, Aux, Goal, No, ExtraCommand, Suspended, Out,
				Requests
) :-

    Suspended =\= true, Suspended =\= false(waiting),
    Suspended =\= false(done) :
      Aux = _,
      Goal = _,
      No = _,
      Selector = _,
      ExtraCommand = true,
      Out = [(resolvent: Suspended)],
      Requests = [];

    otherwise :
      Requests = _,
      Suspended = _ |
	ambient_tree.

ambient_tree(Selector, Aux, Goal, No, ExtraCommand, Out) :-

    Goal =?= ambient_server#Run,
    arity(Run) =< 5,
    arg(3, Run, Root),
    arg(1, Aux, Type),
    channel(Root) :
      No = _,
      write_channel(tree(Type, Tree), Root),
      ExtraCommand = true |
	ambient_tree1 + (Out1 = []);

    otherwise,
    Aux =?= _Type(_Arg, Requests, NextRequests) :
      Goal = _,
      Selector = _,
      Requests = NextRequests,
      ExtraCommand = resolvent(No),
      Out = [].

  ambient_tree1(Selector, Aux, Tree, Out, Out1) :-

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= "",
    Id =?= system |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= "",
    Id =\= system :
      Out ! "+" |
	format_tree_parts + (Out1 = ["-" | Out1]);

    Tree =?= _Type(Id, List, SubTree),
    Selector =?= Id |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= Selector(_Index) |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= _Name(Selector) :
      Selector' = "" |
	format_tree_parts;

    Tree =?= _Type(Id, List, SubTree),
    Id =?= _Name(I),
    I =:= -Selector |
	format_tree_parts;

    Tree =?= _Type(_Id, _List, SubTree),
    otherwise |
	ambient_tree2.

  ambient_tree2(Selector, Aux, SubTree, Out, Out1) :-

    SubTree ? Tree,
    Aux = Type(Arg, Requests, Requests1) :
      Aux' = Type(Arg, Requests', Requests1) |
	ambient_tree1(Selector, Type(Arg, Requests, Requests'?), Tree, Out, Out'?),
	self;

    SubTree =?= [],
    Aux =?= _Type(_Arg, Requests, NextRequests) :
      Requests = NextRequests,
      Selector = _,
      Out = Out1.

format_tree_parts(Selector, Aux, SubTree, Out, Out1, Id, List) :-

    Aux =?= channels(Kind, _Requests, _NextRequests) :
      Out = [Id, "+", "+" | Out'?] |
	spi_debug # format_channels(Kind, List, Out', ["-", "-" | Out''?]),
	ambient_tree2(Selector, Aux, SubTree, Out'', Out1);

    Aux =?= resolvent(Options, Requests, NextRequests) :
      Requests ! show_resolvent(List, Options, Vs),
      Out = [Id, "+", "+" | Out'?],
      Aux' = resolvent(Options, Requests', NextRequests) |
	copy_list(Vs?, Out', ["-", "-" | Out''?]),
	ambient_tree2(Selector, Aux', SubTree, Out'', Out1).


ambient_list(Goals, Run, AList, Cs)  :-

    unknown(Goals) :
      Run = repeat#run(Goals),
      Cs = [reset, forward(AList) | Commands] \ Commands;

    Goals =?= (# Call) :
      Run = repeat#run(Service? # Call),
      Cs = [service(Service), reset, forward(AList) , service(Service?)
	   | Commands] \ Commands;

    Goals =?= (Service # _Call),
    unknown(Service) :
      Run = repeat#run(Goals),
      Cs = [forward(AList) | Commands] \ Commands;

    Goals =?= (Service # _Call),
    string(Service) :
      Run = repeat#run(Goals),
      Cs = [reset, forward(AList), service(Service) | Commands]\Commands;

    otherwise :
      Run = repeat#run(Goals),
      Cs = [reset, forward(AList) | Commands] \ Commands.	


ambient_run(Goals, Run, Control, Events, No, Action, Cs) :-

    Goals =?= (# Call) :
      Run = repeat#run(Service # Call, true),
      Cs = [reset, service(Service),
	    forward(ambient_server#run(Action, Control, Events)),
	    state(No, _Goal, _Events, _Status), service(Service)
	   | Commands] \ Commands;

    Goals =?= (Service' # _Call),
    unknown(Service') :
      Run = repeat#run(Goals, true),
      Cs = [reset, service(Service),
	    forward(ambient_server#run(Action, Control, Events)),
	    state(No, _Goal, _Events, _Status), service(Service)
           | Commands] \ Commands;

    Goals =?= (Service # _Call),
    string(Service) :
      Run = repeat#run(Goals, true),
      Cs = [reset, forward(ambient_server#run(Action, Control, Events)),
	    state(No, _Goal, _Events, _Status), service(Service)
           | Commands] \ Commands;

    otherwise :
      Run = repeat#run(Goals, true),
      Cs = [reset, forward(ambient_server#run(Action, Control, Events)),
	    state(No, _Goal, _Events, _Status)
           | Commands] \ Commands.

/*
ambient_signal(Signal, No, Goal, Commands, Commands1) :-

    Goal =?= ambient_server#Run,
    arg(1, Run, run),
    arity(Run) =< 5,
    arg(3, Run, Root),
    channel(Root) :
      write_channel(Signal(Reply), Root),
      Commands = [to_context([utils # append_strings(["<",No,">"], IdG),
		  computation#display(term, Reply,
				      [prefix(IdG),known(IdG),known(Reply)])])
		 | Commands1];

    otherwise :
      Goal = _,
      Commands = [Signal(No) | Commands1].
*/

ambient_timing(Command, Cs) :-

    Command =?= Process(RPC) :
      Limit = TIME_LIMIT_VALUE,
      Out = _,
      Display = true |
	remote_call(RPC, RPC', S, S'),
	ambient_timer(Process, RPC', Limit, Display, Out, S, S', Cs);

    Command =?= Process(RPC, Limit) :
      Out = _,
      Display = true |
	ambient_timing_limit;
    
    Command =?= Process(RPC, Limit, Out) :
      Display = false |
	ambient_timing_limit.

  ambient_timing_limit(Command, Process, RPC, Limit, Display, Out, Cs) :-

    convert_to_real(Limit, Limit'),
    Limit' >= 0 :
      Command = _ |
	remote_call(RPC, RPC', S, S'),
	ambient_timer(Process, RPC', Limit, Display, Out, S, S', Cs);

    unknown(Limit) :
      Display = _,
      Out = _,
      Process = _,
      RPC = _,
      Cs = Commands\Commands |
	computation # shell(defer(Limit, Command));

    otherwise :
      Command = _,
      Display = _,
      Out = _,
      Process = _,
      RPC = _,
      Cs = Commands\Commands |
	computation # event({invalid_limit(Limit)}).


ambient_timer(Process, RPC, Limit, Display, Out, Service, Service', Cs) :-

    RPC =?= _ # _ :
      Configure = configure(_Actions, Events, Display, No, Out),
      Public = run(Run, Control, Events), 
      Server = ambient_server # Public,
      Cs = [reset, service(Service),
	    forward(computation#
			[Server, 
			 repeat # service(Service, RPC, RPC', true),
		 	 ambient_timer # starter(Process, RPC', Limit,
						 Configure, Public)
			]
		   ),
	    new_goal(No, ambient_server#run(Process(Run,Limit,Out), Control)),
	    service(Service')
	   | Commands]\Commands.


copy_list(In, Out, Next) :-

    In ? Item :
      Out ! Item |
	self;

    In =?= [] :
      Out = Next .


choose_argument(Arg, Number, DefaultNumber, String, DefaultString) :-

    number(Arg) :
      DefaultNumber = _,
      Number = Arg,
      String = DefaultString;

    otherwise :
      DefaultString = _,
      String = Arg,
      Number = DefaultNumber.
