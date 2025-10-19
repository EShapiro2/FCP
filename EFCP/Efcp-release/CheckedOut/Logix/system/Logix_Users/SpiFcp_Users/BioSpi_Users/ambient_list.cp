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
       User Ambient list
       2005
*/

-language([evaluate,compound,colon]).
-mode(failsafe).
-export([run/2, run/3, run/4]).

-include(spi_constants).

run(Goal, File) :-
        run(Goal, File, "1.0e100", "1.0").

run(Goal, File, Limit) + (Scale = "1.0") :-

    Goal =?= _#_,
    string(File), File =\= "",
    convert_to_real(Limit, Limit'),
    0 =< Limit',
    convert_to_real(Scale, Scale'),
    0 < Scale' |
	start_ambient;

    otherwise |
	fail("Bad argument" - run(Goal, File, Limit, Scale)).

start_ambient(Goal, File, Limit, Scale) :-

        /* Last N is the best we can do */
	repeat # path(Goal, Goal'),
        computation #
	    [shell(new_goal(_N, ambient_server # run(Goal'?, System?))),
             ambient_server # run(
		[spi_monitor # scheduler(S), computation # Start?
	                     ], System, Events)
	    ],
        write_channel(cutoff(Limit, State), S, S'),
        write_channel(record(Stream), S', Scheduler),
        file#put_file(File, Out?, write, Ok),
        filter_data + (Tree = [{system,[{public(1),[]}]}], Last = 0),
	synchronize_start,
        run_ok.

  synchronize_start(Scheduler, Goal, Start) :-
    channel(Scheduler) :
      Goal = Start.

  run_ok(Events, File, State, Ok) :-

    Ok = true :
      Events = _,
      File = _,
      State = _;

    otherwise :
      Events = _,
      State = _ |
	fail(("
		write"(File) - Ok));

    Events ? Event,
    Event =\= aborted |
	self;

    Events ? aborted :
      Events' = _,
      File = _,
      Ok = _,
      State = _;

    Events =?= [] :
      File = _,
      Ok = _,
      State = _;

    known(State) :
      Events = _,
      File = _,
      Ok = _.


filter_data(Stream, Events, Scale, Last, Tree, Out) :-

    Stream ? Number, number(Number),
    Number > 0,
    Last' := Scale*Number :
      Last = _ |
	self;

    Stream ? start(_Name) |
	self;

    Stream ? end(_Name(_ChannelName, _Action, _FileId)) |
	self;

    Stream ? reset(_Prefix) |
	self;

    Stream ? ambient(F(A1, A2)),
    string(F) |
	update_tree(F, A1, A2, Tree, Tree'),
	output_tree(Last, Tree'?, Out, Out'?),
	self;

    Stream ? note(_) |
	self;

    Stream ? item(_) |
	self;

    Stream ? pausing(_, _) |
	self;

    otherwise :
      Last = _,
      Tree = _ |
	filter_end;

    Events ? Event,
    Event =\= aborted |
	self;

    unknown(Stream),
    Events =?= [] :
      Last = _,
      Scale = _,
      Tree = _,
      Out = [];

    unknown(Stream),
    Events ? aborted :
      Events' = _,
      Last = _,
      Scale = _,
      Tree = _,
      Out = [].


update_tree(Event, A1, A2, Tree, NewTree) :-

    Event = new |
	insert({A1, []}, A2, Tree, NewTree);

    Event = done :
      A2 = _ |
	extract(A1, node, Tree, NewTree, {A1, []});

    Event = "enter" |
	extract(A1, node, Tree, Tree', Node),
	insert(Node?, A2, Tree'?, NewTree);

    Event = "exit" |
	extract(A1, node, Tree, Tree', Node),
	insert(Node, A2, Tree'?, NewTree);

    Event = merge |
	extract(A1, subtree, Tree, Tree', Extracted),
	insert(Extracted?, A2, Tree'?, NewTree).

  insert(Node, TargetId, Tree, NewTree) + (Found = true, InSubTree = false) :-

    InSubTree =\= true,
    Tree ? {ParentId, SubTree}, ParentId =?= TargetId,
    Node =?= {Id, _SubTree}, Id =\= [] :
      NewTree = [{ParentId, [Node | SubTree]} | Tree'],
      Found = true;

    InSubTree =\= true,
    Tree ? {SiblingId, SiblingSubTree}, SiblingId =?= TargetId,
    Node =?= {Id, SubTree}, Id =?= [] :
      NewTree = [{SiblingId, Merged?} | Tree'],
      Found = true |
	merge(SubTree, SiblingSubTree, Merged);

    InSubTree =\= true,
    Tree ? {OtherId, SubTree}, OtherId =\= TargetId :
      NewTree ! {OtherId, NewSubTree} |
	insert(Node, TargetId, SubTree, NewSubTree, InSubTree', false),
	self;

    InSubTree =?= true :
      Node = _,
      TargetId = _,
      Found = true,
      NewTree = Tree;

    InSubTree =\= true,
    Tree =?= [] :
      Node = _,
      TargetId = _,
      Found = false,
      NewTree = Tree.

  merge(T1, T2, T) :-

    T2 =\= [],
    T1 ? N :
      T ! N |
	self;

    T2 = [] :
      T1 = T;

    T1 =?= [] :
      T2 = T.

  extract(NodeId, Type, Tree, NewTree, Node)
		+ (Found = true, InSubTree = false) :-

    InSubTree =\= true,
    Tree ? NextNode, arg(1, NextNode, NodeId),
    Type =?= node :
      Node = NextNode,
      NewTree = Tree',
      Found = true;

    InSubTree =\= true,
    Tree ? NextNode, arg(1, NextNode, NodeId),
    Type = subtree,
    arg(2, NextNode, SubTree) :
      Node = {[], SubTree},
      NewTree = [{NodeId, []} | Tree'],
      Found = true;

    InSubTree =\= true,
    Tree ? NextNode, arg(1, NextNode, OtherId), OtherId =\= NodeId,
    arg(2, NextNode, SubTree) :
      NewTree ! {OtherId, SubTree'?} |
	extract(NodeId, Type, SubTree, SubTree', Node, InSubTree', false),
	self;

    InSubTree =?= true :
      Node = _,
      NodeId = _,
      Type = _,
      Found = true,
      NewTree = Tree;

    InSubTree =\= true,
    Tree =?= [] :
      Node = _,
      NodeId = _,
      Type = _,
      Found = false,
      NewTree = Tree.


output_tree(Last, Tree, Out, NextOut) :-

    Tree =?= [system(SubTree)] |
	output_subtree([{system(Last), SubTree}], Out, "[", ["]
"		       | NextOut]).

  output_subtree(Tree, Out, Prefix, NextOut) :-

    Tree ? {Name(Index), SubTree},
    convert_to_string(Index, IS),
    string_to_dlist(IS, IL, [CHAR_RIGHT_PAREN]),
    string_to_dlist(Name, NL, [CHAR_LEFT_PAREN | IL]),
    string_to_dlist(Prefix, PL, NL),
    SubTree =?= [] :
      Out ! PS?,
      Prefix' = "," |
	list_to_string(PL, PS),
	self;

    Tree ? {Name(Index), SubTree},
    convert_to_string(Index, IS),
    string_to_dlist(IS, IL, [CHAR_RIGHT_PAREN]),
    string_to_dlist(Name, NL, [CHAR_LEFT_PAREN | IL]),
    string_to_dlist(Prefix, PL, NL),
    SubTree =\= [] :
      Out ! PS?,
      Prefix' = "," |
        list_to_string(PL, PS),
	output_subtree(SubTree, Out', ",[", ["]" | Out'']),
        output_subtree;

    Tree =?= [] :
      Prefix = _,
      Out = NextOut.


filter_end(Stream, Events, Out, Scale) :-

    Stream ? ambient(terminated(system, system)) |
	self;

    Stream ? idle(Number),
    Number' := Scale*Number :
      Events = _,
      Stream' = _,
      Out = [Number', "
"];

    Stream ? done(Number),
    Number' := Scale*Number :
      Events = _,
      Stream' = _,
      Out = [Number', "
"];

    Stream ? Element,
    otherwise,
    list_to_string([CHAR_QUERY, CHAR_EOL], String) :
      Events = _,
      Scale = _,
      Stream' = _,
      Out = [String] |
	fail((data:Element));

    Stream =?= [] :
      Events = _,
      Scale = _,
      Out = [] ;

    otherwise,
    list_to_string([CHAR_QUERY, CHAR_EOL], String) :
      Events = _,
      Scale = _,
      Out = [String] |
	fail((format:Stream)).
