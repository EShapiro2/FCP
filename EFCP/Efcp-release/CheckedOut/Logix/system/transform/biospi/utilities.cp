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
       Precompiler for Stochastic Pi Calculus procedures - utilities.
       December 1999.
*/

-language([evaluate, compound, colon]).
-export([append/3,
	 concatenate_lists/2, find_logix_variables/3, make_lhs_tuple/3,
	 make_communicator/4, make_predicate_list/3,
	 untuple_predicate_list/3, untuple_predicate_list/4,
	 names_to_channel_list/2,
	 remove_duplicate_strings/3, sort_out_duplicates/3,
	 remove_item/3, subtract_list/3,
	 tuple_to_atom/2, update_process_mode/3,
	 verify_channel/7, verify_communication_channel/9]).

-include(spi_constants).
-include(bio_constants).


update_process_mode(Mode, GuardMode, NewMode) :-

    GuardMode =?= none :
      NewMode = Mode;

    Mode =?= none,
    GuardMode =?= send :
      NewMode = communicate;

    Mode =?= none,
    GuardMode =?= receive :
      NewMode = communicate;

    Mode =?= none,
    GuardMode =\= send, GuardMode =\= receive :
      NewMode = GuardMode;

    Mode =?= GuardMode :
      NewMode = GuardMode;

    Mode =?= send,
    GuardMode =?= communicate :
      NewMode = communicate;

    Mode =?= receive,
    GuardMode =?= communicate :
      NewMode = communicate;

    Mode =?= communicate,
    GuardMode =?= send :
      NewMode = communicate;

    Mode =?= communicate,
    GuardMode =?= receive :
      NewMode = communicate;

    Mode =?= compare,
    GuardMode =?= otherwise :
     NewMode = compared;

    otherwise :
      Mode = _,
      GuardMode = _,
      NewMode = conflict.

/**** Utilities ****/

tuple_to_atom(LHS, Atom) :-

    LHS = {String} :
      Atom = String;

    otherwise :
      Atom = LHS.


append(List, Tail, NewList) :-

    List ? Item :
      NewList ! Item |
	self;

    List = [] :
      NewList = Tail;

    List =\= [_|_], List =\= [] |
      NewList = [List | Tail].


concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =\= [_ | _] :
      Out ! Item,
      Lists' = [List' | Rest] |
	self;

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =?= [_ | _] :
      Lists' = [Item, List' | Rest] |
	self;

    Lists = [List | Rest],
    List ?  [] :
      Lists' = [List' | Rest] |
	self;

    Lists ? [] |
	concatenate_lists;

    Lists =?= [] :
      Out = [].


subtract_list(List1, List2, List3) :-

    List2 ? Item |
	remove_item(Item, List1, List1'),
	self;

    List2 = [] :
      List3 = List1.


remove_item(Item, ListIn, ListOut) :-

    ListIn ? I,
    Item =?= I |
	self;

    ListIn ? I,
    Item =\= I :
      ListOut ! I |
	self;

    ListIn =?= [] :
      Item = _,
      ListOut = [].


remove_duplicate_strings(List1, List2, Reply) :-

	ordered_merger(Merger, Duplicates, []),
	utils#binary_sort_merge(List1, _, Merger),
	removed_some(List1, List2, Duplicates, Reply).

  removed_some(List1, List2, Duplicates, Reply) :-

    Duplicates =?= [] :
      Reply = [],
      List2 = List1;

    Duplicates =\= [] |
	/* Delete "duplicate" duplicates. */
	remove_duplicates(Duplicates, Reply),
	remove_duplicates(List1, List2).

  remove_duplicates(List1, List2) :-

    List1 ? Item,
    Item =\= NULL :
      List2 ! Item |
	remove_item(Item, List1', List1''),
	self;

    List1 ? Item,
    Item =?= NULL :
      List2 ! Item |
	self;

    List1 = [] :
      List2 = [].


make_lhs_tuple(Name, ChannelNames, Tuple) :-

    ChannelNames =?= [] :
      Tuple = {Name};

   ChannelNames =\= [] |
	names_to_arguments,
	utils#list_to_tuple([Name | Arguments], Tuple).

  names_to_arguments(ChannelNames, Arguments) :-

    ChannelNames ? ChannelName :
      Arguments ! `ChannelName |
	self;

    ChannelNames =?= [] :
      Arguments = [].


untuple_predicate_list(Operator, Predicates, List) + (NextList = []) :-

    Predicates =?= {Operator, Predicate, Predicates'} |
	untuple_predicate_list(Operator, Predicate, List, List'?),
	self;

    otherwise :
      Operator = _,
      List = [Predicates | NextList].


make_predicate_list(Operator, List, Predicates) :-

    List =?= [] |
      Operator = _,
      Predicates = true;

    List ? true,
    List' =\= [] |
      self;

    List ? Predicate, Predicate =\= true,
    List' ? true :
      List''' = [Predicate | List''] |
	self;

    List ? Predicate, Predicate =\= true,
    List' =\= [], List' =\= [true | _] :
      Predicates = {Operator, Predicate, Predicates'?} |
	self;

    List =?= [Predicate] :
      Operator = _,
      Predicates = Predicate.


sort_out_duplicates(InLists, Out, Reply) :-

	concatenate_lists(InLists, List),
	ordered_merger(Merger, Duplicates, []),
	utils#binary_sort_merge(List, Out, Merger),
	/* Remove duplicate "duplicates" */
	utils#binary_sort_merge(Duplicates, Reply).

ordered_merger(In, Left, Right) :-
    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out, Left, Left'),
	ordered_merger;

    In = [] :
      Left = Right.

ordered_merge(In1, In2, Out, Left, Right) :-

    In1 ? I1, In2 = [I2 | _],
    string(I1), string(I2),
    I1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), string(I2),
    I2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I | _], In2 ? I,
    string(I) :
      Left ! I |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    arg(1, I1, A1), arg(1, I2, A2),
    A1 @< A2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A1), arg(1, I2, A2),
    A2 @< A1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A), arg(1, I2, A) :
      Left ! A |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    string(I1), arg(1, I2, A2),
    I1 @< A2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), arg(1, I2, A2),
    A2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    string(I1), arg(1, I2, A),
    I1 =?= A :
      Left ! A |
	ordered_merge;

    In1 ? I1, In2 = [I2 | _],
    arg(1, I1, A1), string(I2),
    A1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A1), string(I2),
    I2 @< A1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    arg(1, I1, A), string(I2),
    A =?= I2 :
      Left ! A |
	ordered_merge;

    In1 = [] :
      In2 = Out,
      Left = Right ;

    In2 = [] :
      In1 = Out,
      Left = Right .


verify_communication_channel(Language, Name, ChannelName, ChannelNames, Locals,
			     OkLocus, OkChannelName, Errors, NextErrors) :-

    string(ChannelName) :
      Language = _,
      OkLocus = LOCAL |
	verify_channel;

    Language =?= biospi,
    ChannelName = `ChannelName',
    string(ChannelName') :
      Language = _,
      OkLocus = LOCAL |
	verify_channel;

    Language =?= biospi,
    ChannelName = S2S(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(S2S) |
	verify_channel;

    Language =?= biospi,
    ChannelName = P2C(ChannelName'),
    string(ChannelName') :
      OkLocus = self(P2C) |
	verify_channel;

    Language =?= biospi,
    ChannelName = C2P(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(P2C) |
	verify_channel;

    Language =?= biospi,
    ChannelName = LOCAL(ChannelName'),
    string(ChannelName') :
      OkLocus = LOCAL |
	verify_channel;

    Language =?= biospi,
    ChannelName = (MERGE + ChannelName'),
    string(ChannelName') :
      OkLocus = parent(MERGE) |
	verify_channel;

    Language =?= biospi,
    ChannelName = (MERGE - ChannelName'),
    string(ChannelName') :
      OkLocus = parent(MERGE) |
	verify_channel;

    Language =?= biospi,
    ChannelName = ACCEPT(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(ENTER) |
	verify_channel;

    Language =?= biospi,
    ChannelName = ENTER(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(ENTER) |
	verify_channel;
    
    Language =?= biospi,
    ChannelName = EXPEL(ChannelName'),
    string(ChannelName') :
      OkLocus = self(EXIT) |
	verify_channel;

    Language =?= biospi,
    ChannelName = EXIT(ChannelName'),
    string(ChannelName') :
      OkLocus = parent(EXIT) |
	verify_channel;

   otherwise,
   Language = spifcp :
      ChannelNames = _,
      Locals = _,
      OkLocus = LOCAL,
      OkChannelName = NULL,
      Errors = [Name - invalid_spifcp_channel(ChannelName) | NextErrors];

   otherwise :
      ChannelNames = _,
      Locals = _,
      OkLocus = LOCAL,
      OkChannelName = NULL,
      Errors = [Language(Name) - invalid_channel(ChannelName) | NextErrors].
	

verify_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    string(ChannelName),
    nth_char(1, ChannelName, C),
    CHAR_a =< C, C =< CHAR_z |
	defined_channel;

    ChannelName = `ChannelName',
    nth_char(1, ChannelName', C),
    CHAR_A =< C, C =< CHAR_Z |
	defined_channel;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = NULL,
      Errors = [Name - invalid_channel(ChannelName) | NextErrors].

  defined_channel(Name, ChannelName, ChannelNames, Locals, OkChannelName,
		Errors, NextErrors) :-

    ChannelNames ? Other, ChannelName =\= Other |
	self;

    ChannelNames ? ChannelName :
      ChannelNames' = _,
      Locals = _,
      Name = _,
      OkChannelName = ChannelName,
      Errors = NextErrors;

    ChannelNames = [], Locals =\= [] :
      ChannelNames' = Locals,
      Locals' = [] |
	self;

    otherwise :
      ChannelNames = _,
      Locals = _,
      OkChannelName = NULL,
      Errors = [Name - undefined_channel(ChannelName) | NextErrors].

names_to_channel_list(MChannels, ChannelList) :-

    MChannels =?= [] :
      ChannelList = [];

    otherwise |
	wrap_channel_names,
	utils#list_to_tuple(Wrapped, ChannelList).

  wrap_channel_names(MChannels, Wrapped) :-

    MChannels ? ChannelName,
    nth_char(1, ChannelName, C),
    CHAR_a =< C, C =< CHAR_z :
      Wrapped ! channel(`ChannelName) |
	self;

    MChannels ? ChannelName,
    otherwise :
      Wrapped ! variable(`ChannelName) |
	self;

    MChannels =?= [] :
      Wrapped = [].


find_logix_variables(Predicate, LogixVars, NextLogixVars) :-

    Predicate ? Element |
	find_logix_variables(Element, LogixVars, LogixVars'?),
	self;

    Predicate =?= `String, string(String) :
      LogixVars = [Predicate | NextLogixVars];

    Predicate =?= `_Other,
    otherwise :
      LogixVars = NextLogixVars;

    Predicate =?= ?String, string(String) :
      LogixVars = [`String | NextLogixVars];

    Predicate =?= ?_Other,
    otherwise :
      LogixVars = NextLogixVars;

    tuple(Predicate),
    Predicate =\= `_, Predicate =\= ?_,
    N := arity(Predicate) |
	find_in_tuple + (Tuple = Predicate);

    otherwise :
      Predicate = _,
      LogixVars = NextLogixVars.

  find_in_tuple(N, Tuple, LogixVars, NextLogixVars) :-

    N-- > 0,
    arg(N, Tuple, Argument) |
	find_logix_variables(Argument, LogixVars, LogixVars'?),
	self;

    N =< 0 :
      Tuple = _,
      LogixVars = NextLogixVars.

make_communicator(Language, Atom, Chooser, CommunicationAtom) :-

    Language =?= biospi :
      Suffix = [BIOSPI_COMMUNICATION_SUFFIX] |
	suffix_communicator;

    Language =?= spifcp :
      Suffix = [SPIFCP_COMMUNICATION_SUFFIX] |
	suffix_communicator.

  suffix_communicator(Suffix, Atom, Chooser, CommunicationAtom) :-

    arg(1, Atom, Prefix),
    string_to_dlist(Prefix, PL, Suffix),
    list_to_string(PL, Goal) :
      Chooser = Goal |
	utils#tuple_to_dlist(Atom, [_ | ChannelVariables],
			     [VAR_CHOSEN, VAR_MESSAGE]),
	utils#list_to_tuple([Chooser | ChannelVariables], CommunicationAtom).
