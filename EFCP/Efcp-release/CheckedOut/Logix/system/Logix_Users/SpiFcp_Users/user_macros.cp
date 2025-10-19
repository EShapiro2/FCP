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
       SpiFcp Shell macros
       2005
*/

-export([expand/2]).
-mode(trust).
-language([evaluate,compound,colon]).
-include(spi_constants).

/***	expand/2

**	Expand macro commands.

**	expand(Command, Commands\Commands1)
**	->	Command  is a console command.
**	<=	Commands\Commands1  is a difference list of commands to the
**		system-macro processor and the user shell.

**	Commands which are not expanded here are relayed to the Logix_Users
**	user_macros.
*/

procedure expand(Any, [Any]\[Any]).

expand(Command, Cs) :-

    Cs = SCs\LCs :
      Cs' = Commands\[] |
	expand_spifcp,
	relay_derived.

  relay_derived(Commands, SCs, LCs) :-

    Commands ? Command |
	super # super # user_macros # expand(Command, SCs\SCs'),
	self;

    Commands =?= [] :
      SCs = LCs.

expand_spifcp(Command, Cs) :-

    unknown(Command) |
	defer_command(Command, Command, Cs);

    tuple(Command),
    arg(1, Command, V),
    unknown(V) |
	defer_command(Command, V, Cs);

    tuple(Command),
    arg(1, Command, M), arg(2, Command, V),
    known(M), unknown(V) |
	possibly_deferred;

    otherwise |
	expand_all.

  possibly_deferred(Command, M, V, Cs) :-

    M =\= "|",
    M =\= pdb,
    M =\= profile,
    M =\= record,
    M =\= rpc,
    M =\= run,
    M =\= time,
    M =\= trace :
      V = _ |
	expand_all;

    otherwise :
      M = _ |
	defer_command(Command, V, Cs).

  defer_command(M, V, ([shell(defer(V, M)) | Commands]\Commands)^).


expand_all(Command, Cs) :-

% Spi Calculus macros

    Command =?= spifcp(Command') |
	expand_spifcp;

    Command = psc :
      Cs = [to_context(spi_monitor # spifunctions([])) | Commands]\Commands;

    Command = psc(FS) :
      Cs = [to_context(spi_monitor # spifunctions(FS)) | Commands]\Commands;

    Command = options(New) :
      Command' = options(New, _) |
	self;

    Command = options(New, Old) :
      Cs = [to_context(spi_monitor # options(New, Old)) | Commands]\Commands;

    Command = pc(C) :
      Cs = [to_context(spi_utils # make_channel(C)) | Commands]\Commands;

    Command = pc(C, S, B) :
      Cs = [to_context(spi_utils # make_channel(C, S, B)) | Commands]\Commands;

    Command = pc(C, S, B, W) :
      Cs = [to_context(spi_utils # make_channel(C,S,B,W)) | Commands]\Commands;

    Command =?= ph :
      Command' = ph(true) |
	self;

    Command = ph(PhReady) :
	CL = [  " spifcp(Command)    - execute Command",
		" cta                - display communicating channels",
		" options(New, Old)  - default Spi Options",
		" pc(C)              - make spi channel C",
		" pdb(RPC, Options)  - debug(RPC)",
		" ph                 - get this list",
		" pr(C,M)            - receive M from spi channel C",
		" ps(M,C)            - send M on spifcp channel C",
		" randomize          - set all new channels to randomize",
		" serialize          - set all new channels to serialize",
		" spc(C)             - Spi channel",
		" spg / spg(No)      - Spi goal of computation No",
		" spr / spr(No)      - Spi resolvent of computation No",
		" ctree(Tree)        - Close a vanilla tree",
		" ptree(Tree)        - Spi execution tree",
		" run(G, F, L, S, O) - run Goals, record to File until Limit,",
		"                      scaled by Scale (or reals converted by",
		"                      Sprint) with format Option.",
		" reset              - reset Spi monitor",
		" profile(RPC, L, Out)- run RPC until Limit -",
                "                      compute => Out = values (list)",
		" rpc(RPC, L, Out)   - run RPC until Limit -",
                "                      compute => Out = reductions per cycle",
		" time(RPC, L, Out)  - run RPC until Limit -",
                "                      compute => Out = values (list)",
		" trace(G,F,L,S,O)   - run Goals - trace to File until Limit,",
		"                      scaled by Scale, with format Option",
		" vtree(Co, G, Tree) - Call vanilla#tree(Co, G, Tree)",
		" ctree(Tree)        - Close Tree",
		" ptree(Tree)        - Print Tree",
		" weighter(W)        - set the default weighter",
		"",
		"        name options for channel display",
		"             short/base/creator/full",
		"        annotation options for channel display:",
		"             none/active/note",
		"        additional option for ptree:",
		"           process replaces base above",
		"           goal order:",
		"             prefix/execute"
		
	 ],
      Cs = [to_context(computation # display(stream, CL,
			[wait(PhReady)]))
	   | Commands]\Commands ;

    Command = pr(C, M) :
      Cs = [to_context(spi_utils # receive(C, M)) | Commands]\Commands;

    Command = pr(C, M, N) :
      Cs = [to_context(spi_utils # receive(C, M, N)) | Commands]\Commands;

    Command = ps(M, C) :
      Cs = [to_context(spi_utils # send(M, C)) | Commands]\Commands;

    Command = ps(M, C, N) :
      Cs = [to_context(spi_utils # send(M, C, N)) | Commands]\Commands;

    Command = randomize :
      Cs = [to_context([spi_monitor # randomize]) 
	   | Commands]\Commands;

    Command = serialize :
      Cs = [to_context([spi_monitor # serialize]) 
	   | Commands]\Commands;

    Command = spc(C) :
      Cs = [to_context([spi_monitor # options(O, O),
			spi_utils # show_channel(C, O?, Channel),
			computation # display(term, Channel, known(Channel))]) 
	   | Commands]\Commands;

    Command = spc(C, Options) :
      Cs = [to_context([spi_utils # show_channel(C, Options, Channel)]),
			computation # display(term, Channel, known(Channel))
	   | Commands]\Commands;

    Command = spg :
      Command' = spg(_) |
	self;

    /* Obsolescent (?) */
    Command = spg(No) :
      Cs = [state(No, Goal, _, _),
	    to_context([spi_monitor # options(O, O),
			spi_utils # show_goal(Goal, O?, Term),
			computation # display(term, Term, known(Term))]) 
	   | Commands]\Commands;

    Command = spg(No, Options) :
      Cs = [state(No, Goal, _, _),
	    to_context([spi_utils # show_goal(Goal, Options, Term),
			computation # display(term, Term, known(Term))]) 
	   | Commands]\Commands;

    Command = spr :
      Command' = spr(_) |
	self;

    Command = spr(No) :
      Cs = [resolvent(No, Resolvent),
	    to_context([spi_monitor # options(O, O),
			spi_utils # show_resolvent(Resolvent, O?, Stream),
			computation # display(stream, Stream, [])])
	   | Commands]\Commands;

    Command = spr(No, Options) :
      Cs = [resolvent(No, Resolvent),
	    to_context([spi_utils # show_resolvent(Resolvent, Options, Stream),
			computation # display(stream, Stream)])
	   | Commands]\Commands;
    /***************/

    Command = ctree(Tree) :
      Cs = [to_context(spi_utils # close_tree(Tree)) | Commands]\Commands;

    Command = ptree(Tree) :
      Cs = [to_context([spi_monitor # options(O, O),
			spi_utils # show_tree(Tree, [none | O?], Stream),
			computation # display(stream, Stream)])
	   | Commands]\Commands;

    Command = ptree(Tree, Options) :
      Cs = [to_context([computation # display(stream, Stream, []),
			spi_utils # show_tree(Tree, Options, Stream)]) 
	   | Commands]\Commands;

    Command = vtree(Context, Conjunction, Tree) :
      Cs = [vanilla # tree(Context'?, Conjunction'?, Tree) 
	   | Commands]\Commands |
	repeat # path(Context, Context'),
	repeat # path(Conjunction, Conjunction');

    Command = vtree(Context, Conjunction, Tree, Depth) :
      Cs = [vanilla # tree(Context'?, Conjunction'?, Tree, Depth) 
	   | Commands]\Commands |
	repeat # path(Context, Context'),
	repeat # path(Conjunction, Conjunction');

    Command = weighter(Weighter) :
      Cs = [to_context([spi_utils # weighter(Weighter)])
	   | Commands]\Commands;

    /* bta - dta */
    string_to_dlist(Command,[X, CHAR_t, CHAR_a], []),
    CHAR_b =< X, X =< CHAR_d :
      Cs = [state(No,_,_,_),
	    to_context([utils # append_strings(["<",No?,"> "], IdG),
			spi_monitor # status(Status),
			spi_debug # spi_channels(X, Status?, Out),
			spi_debug # stream_out(Out?, IdG?, Commands, Commands1)
		       ])
	   | Commands]\Commands1;


/* Override Logix_Users: user_macros */

   Command = (# _) |
	spi_remote_run(run, Command, Run, spi_record#run(Run), Cs, false);

   Command = (_ # _) |
	spi_remote_run(run, Command, Run, spi_record#run(Run), Cs, false);
 
   Command = r(No, Options) :
      Cs = [resolvent(No, Resolvent),
	    to_context([spi_utils # show_resolvent(Resolvent, Options, Stream),
			computation # display(stream, Stream)])
	   | Commands]\Commands;

    Command = reset :
      Cs = [to_context(spi_monitor # reset) | Commands]\Commands;

    Command = '^' :
      Cs = [display_stream(Bindings',[]),
	    to_context([computation # dictionary(bindings, Bindings, 0),
			spi_monitor # options(O, O),
			spi_utils # show_list(Bindings?, O?, Bindings')])
	   | Commands]\Commands;

    Command = {Hat, X}, Hat =?= '^',
    X =?= '^' :
      Cs = [to_context([computation # dictionary(bindings, Bindings, 1),
			spi_monitor # options(O, O),
			spi_utils # show_list(Bindings?, O?, Bindings')]),
	    display_stream(Bindings'?, [])
	   | Commands]\Commands;

    Command = {Hat, X}, Hat =?= '^',
    X =\= '^' :
      Cs = [to_context([spi_monitor # options(O, O) | Stuff])
	   | Commands]\Commands |
	display_variables(X, O?, Stuff);

/* Mutiple format remote call commands */

    tuple(Command),
    arg(1, Command, record), arity(Command) > 1 |
	tuple_to_dlist(Command, [_, Goals | TheRest], []),
	list_to_tuple([run, Run | TheRest], Runner),
	spi_remote_run(record, Goals, Run, spi_record#Runner?, Cs);

    tuple(Command),
    arg(1, Command, run), arity(Command) > 1 |
	tuple_to_dlist(Command, [_, Goals | TheRest], []),
	list_to_tuple([run, Run | TheRest], Runner),
	spi_remote_run(run, Goals, Run, spi_record#Runner?, Cs);

    tuple(Command),
    arg(1, Command, trace), arity(Command) > 1 |
	tuple_to_dlist(Command, [_, Goals | TheRest], []),
	list_to_tuple([run, Run | TheRest], Runner),
	spi_remote_run(trace, Goals, Run, spi_trace#Runner?, Cs);

    tuple(Command),
    arg(1, Command, profile),
    arity(Command, Arity),
    2 =< Arity, Arity =< 4 |
	spi_timing;

    tuple(Command),
    arg(1, Command, rpc),
    arity(Command, Arity),
    2 =< Arity, Arity =< 4 |
	spi_timing;

    tuple(Command),
    arg(1, Command, time),
    arity(Command, Arity),
    2 =< Arity, Arity =< 4 |
	spi_timing;

    Command = pdb(Service) :
      Cs = [to_context(spi_monitor # options(O, O)),
	    spidbg # interpret(Name?, Service'?, O?)
	   | Commands]\Commands |
	repeat # path(Service, Service'),
	parse_rpc(Service'?, Name);

    Command = pdb(Service, Options) :
      Cs = [spidbg # interpret(Name?, Service'?, Options)
	   | Commands]\Commands |
	repeat # path(Service, Service'),
	parse_rpc(Service'?, Name);


/*************************************/
 
   Command = "_complete_names"(Head, Tail) :
      Tail = [ctree, forward, options, pdb, ptree,
	      profile, record, rpc, run, time,
	      randomize, reset, spr, serialize, trace, vtree,
	      weighter,
	      active, base, creator, execute, full, none, note, prefix, short,
	      spi2cmp, spi2fcp,
	      bta, cta, dta
             | Tail'?],
      Cs = ["_complete_names"(Head, Tail') | Commands]\Commands;

    Command = "_all_functors"(Head, Tail) :
      Head' = [bta, cta, dta, pc, pdb, ph, pr, ps, psc, spc, spg, spr | Head], 
      Tail = [ctree, forward, options, pdb, ptree,
	      profile, record, rpc, run, time,
	      randomize, reset, spr, serialize, trace, vtree,
	      weighter,
	      active, base, creator, execute, full, none, note, prefix, short
             | Tail'],
     Cs = ["_all_functors"(Head'?, Tail') | Commands]\Commands;

/********* For testing only! *********/

    Command = spi2cmp(N) :
      Command' = spi2cmp(N, []) |
	self;

    Command = spi2cmp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
		       spi_macros # spi2cmp(N, Options, Results)
	   | Commands]\Commands;

    Command = spi2fcp(N) :
      Command' = spi2fcp(N, []) |
	self;

    Command = spi2fcp(N, Options) :
      Cs = [to_context(computation # display(stream,Results, [type(unparse)])),
		       spi_macros # spi2fcp(N, Options, Results)
	   | Commands]\Commands;

/*************************************/

    otherwise :
      Cs = [Command | Commands]\Commands .


parse_rpc(RPC, Name) :-

    RPC =?= Module # _ :
      Name = Module;

    otherwise :
      RPC = _,
      Name = "?".

display_variables(X, Options, Xs) :-

    X =  `VarName,
    string(VarName) :
      Xs ! computation # dictionary(find, VarName, Value, Reply) |
	display_variable(Reply, VarName, Value, Options, Xs');

    otherwise :
      Xs ! computation # dictionary(find, X, Value, Reply) |
	display_variable(Reply, X, Value, Options, Xs').

display_variable(Reply, Id, Value, Options, Xs) :-

    Reply = true,
    string(Id), known(Value) :
      Xs = [spi_utils # show_value(Value, Options, Value'),
	    computation # display(term, Id = Value', known(Value'))];

    integer(Id) :
      Xs ! processor # link(execute(concatenate,{['_X',Id], Id', 0, 10000})) | 
	self;

    Reply = true,
    unknown(Value), string(Id) : Options = _,
      Xs = [computation # display(term, uninstantiated_variable(Id))] ;

    Reply = false,
    unknown(Value) , string(Id) : Options = _,
      Xs = [computation # display(term, undeclared_variable(Id))] ;

    otherwise :
      Reply = _, Value = _, Options = _,
      Xs = [computation # display(term, invalid_variable_name(Id))].

spi_remote_run(Process, Goals, Run, Action, Cs) + (NameGoals = true) :-
	remote_call(Goals, RPC, Service, Service'),
	spi_run(Process, RPC, Run, Action, NameGoals,
		Service, Service', Cs
	).

spi_run(Process, RPC, Run, Action, NameGoals, Service, Service', Cs) :- 
    RPC = _ # _ :
      Cs = [service(Service),
	    to_context(repeat#service(Service, RPC, Run, NameGoals)),
	    to_context(spi_monitor#reset), 
	    forward(Action),
	    new_goal(_N, Process(Run)),
	    system_macro(defer(Service', service(Service')))
	   | Commands]\Commands.


spi_timing(Command, Cs) :-

    Command =?= Process(RPC) :
      Limit = TIME_LIMIT_VALUE,
      Out = _ |
	remote_call(RPC, RPC', Service, Service'),
	spi_timer(Process, RPC', Limit, true, Out, Service, Service', Cs);

    Command =?= Process(RPC, Limit) :
      Out = _,
      Display = true |
	spi_timing_limit;
    
    Command =?= Process(RPC, Limit, Out) :
      Display = false |
	spi_timing_limit.

  spi_timing_limit(Command, Process, RPC, Limit, Display, Out, Cs) :-

    convert_to_real(Limit, Limit'),
    Limit' >= 0 :
      Command = _ |
	remote_call(RPC, RPC', Service, Service'),
	spi_timer(Process, RPC', Limit, Display, Out, Service, Service', Cs);

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

spi_timer(Process, RPC, Limit, Display, Out, Service, Service', Cs) :-
    RPC = _ # _ :
      Cs = [service(Service),
	    to_context(repeat#service(Service, RPC, RPC', true)),
	    to_context(spi_monitor # [reset, scheduler(Scheduler)]),
	    to_context(computation # events(Events)),
	    forward(spi_timer # starter(Scheduler?, Limit,
		Process(RPC'?, configure(_Actions, Events?, Display, No, Out)))
	    ),
	    new_goal(No, Process(RPC', Limit, Out)),
	    system_macro(defer(Service', service(Service')))
	   | Commands]\Commands.
