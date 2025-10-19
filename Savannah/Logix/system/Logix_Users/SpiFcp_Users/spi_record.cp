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
       SpiFcp Record channel activity from monitor record output
       1999
*/

-language([evaluate,compound,colon]).
-mode(trust).
-export([run/1, run/2, run/3, run/4, run/5,
	 run/7]).

-include(spi_constants).
-include(file_constants).

REALTIME => 12.

DEFAULT_LIMIT => TIME_LIMIT_VALUE.
DEFAULT_SCALE => 1.0.
DEFAULT_SPRINT => "%.7G".
DEFAULT_FORMAT => none.
NO_FILE => nil.

/*
** RPC:		Remote Procedure Call - Service # Goal
** Limit:	Maximum internal time
** File:	Name of file, evaluated by convert_to_string with result =\= ""
** Scale:	Multiplies internal time before recording product
** Sprint:	Conversion specification for internal time
**			Letter
**				Letter is one of e,E,f,g,G
**			Letter(Width)
**				Width is non-negative numeric
**			Letter(Width, Precision)
**				Precision is non-negative numeric
**			String
**				String is a legal C format specification,
**				including exactly one double conversion
**
** Numeric arguments, other than Width and Precision, are evaluated by
** convert_to_real, with result > 0.0 .
** Scale is obsolescent, and may be removed in the future.
*/

run(RPC) :-
	Limit = DEFAULT_LIMIT,
	check_arguments([rpc(RPC)], Reply),
	run_check.

run(RPC, Arg) :-

    Arg =?= [] :
      Limit = DEFAULT_LIMIT |
	check_arguments([rpc(RPC)], Reply),
	run_check;

    number(Arg) :
      Limit = Arg |
	check_arguments([rpc(RPC), limit(Limit)], Reply),
	run_check;

    string(Arg) |
	run(RPC, Arg, DEFAULT_LIMIT).

  run_check(RPC, Limit, Reply) :-

    Reply = [] |
	computation # events(Events),
	processor # link(lookup(interface, InterOffset)),
	monitor_run + (Scale = DEFAULT_SCALE, Sprint = DEFAULT_SPRINT,
		    Format = NO_FILE, FileOffset = 0, Fd = 0, Ok = true);

    Reply =\= [] :
      RPC = _,
      Limit = _ |
      	utils # list_to_tuple([errors | Reply], Tuple),
	wait_to_fail + (Arity = 2).

	
run(RPC, File, Limit) :-

	check_arguments([rpc(RPC), file(File), limit(Limit)], Reply),
	record_check + (Scale = DEFAULT_SCALE, Sprint = DEFAULT_SPRINT,
			Format = DEFAULT_FORMAT, Arity = 3).

run(RPC, File, Limit, Arg) :-

    number(Arg) |
	check_arguments([rpc(RPC), file(File), limit(Limit), scale(Arg)],
			Reply),
	record_check + (Scale = Arg, Sprint = DEFAULT_SPRINT,
			Format = DEFAULT_FORMAT, Arity = 4);

    Arg =\= none, Arg =\= process, Arg =\= creator, Arg =\= full,
    Arg =\= ambient |
	check_arguments([rpc(RPC), file(File), limit(Limit),
			 sprint(Arg, Sprint)], Reply),
	record_check + (Scale = 1.0, Format = DEFAULT_FORMAT, Arity = 4);

    otherwise :
      Arg = Format |
	check_arguments([rpc(RPC), file(File), limit(Limit),
			 format(Arg)], Reply),
	record_check + (Scale = 1.0, Sprint = DEFAULT_SPRINT, Arity = 4).

run(RPC, File, Limit, Arg, Format) :-

    number(Arg) |
	check_arguments([rpc(RPC), file(File), limit(Limit),
			 scale(Arg), format(Format)], Reply),
	record_check + (Scale = Arg, Sprint = DEFAULT_SPRINT, Arity = 5);

    otherwise |
	check_arguments([rpc(RPC), file(File), limit(Limit),
			 sprint(Arg, Sprint), format(Format)], Reply),
	record_check + (Scale = 1.0, Arity = 5).

run(RPC, File, Limit, Sprint, Format, Scale, Events) :-

	check_arguments([rpc(RPC), file(File), limit(Limit), sprint(Sprint),
			 format(Format), scale(Scale)], Reply),
	record_check1 + (Arity = 6).


check_arguments(Args, Reply) :-

    Args ? rpc(_ # _) |
	self;

    Args ? file([]) |
	self;

    Args ? file(File),
    convert_to_string(File, String), String =\= "" |
	self;

    Args ? limit(Limit),
    convert_to_real(Limit, Real), Real > 0.0 |
	self;

    Args ? scale(Scale),
    number(Scale),
    convert_to_real(Scale, Real), Real > 0.0 |
	self;

    Args ? sprint(Letter, Sprint),
    string(Letter),
    string_length(Letter) =:= 1 |
	check_sprint_letter(Letter, LetterOk, Reply, Reply'),
	utils # append_strings(["%", LetterOk], Sprint),
	self;

    Args ? sprint(Letter(Width), Sprint) |
	check_sprint_letter(Letter, LetterOk, Reply, Reply'),
	check_sprint_parameter(width, Width, WidthOk, Reply', Reply''),
	utils # append_strings(["%", WidthOk, LetterOk], Sprint),
	self;

    Args ? sprint(Letter(Width, Precision), Sprint) |
	check_sprint_letter(Letter, LetterOk, Reply, Reply'),
	check_sprint_parameter(width, Width, WidthOk, Reply', Reply''),
	check_sprint_parameter(precision, Precision, PrecisionOk,
				Reply'', Reply'''),
	utils # append_strings(["%", WidthOk, ".", PrecisionOk, LetterOk],
				Sprint),
	self;

    Args ? sprint(Convert, Sprint),
    string(Convert),
    string_length(Convert) > 1 :
      Sprint = Convert |
	self;

    Args ? sprint(Sprint),
    string(Sprint) |
	self;

    Args ? format(Format),
    Format =?= none |
	self;

    Args ? format(Format),
    Format =?= process |
	self;

    Args ? format(Format),
    Format =?= creator |
	self;

    Args ? format(Format),
    Format =?= full |
	self;

    Args ? format(Format),
    Format =?= ambient |
	self;

    Args ? Type(Value),
    otherwise :
      Reply ! Type(Value) |
	self;

    Args ? Type(Value),
    otherwise :
      Reply ! Type(Value) |
	self;

    Args ? Type(Value, Value'),
    otherwise :
      Reply ! Type(Value),
      Value' = "" |
	self;

    Args =?= [] :
      Reply = [].

  check_sprint_letter(Letter, LetterOk, Reply, Reply') :-

    Letter =\= "f", Letter =\= "e", Letter =\= "E",
    Letter =\= "g", Letter =\= "G" :
      LetterOk = "",
      Reply ! invalid_conversion_letter(Letter);

    otherwise :
      Letter = LetterOk,
      Reply = Reply'.

  check_sprint_parameter(Type, Parameter, ParameterOk, Reply, Reply') :-

    integer(Parameter), Parameter >= 0 :
      Type = _,
      Parameter = ParameterOk,
      Reply = Reply';

    otherwise :
      ParameterOk = "",
      Reply ! invalid_parameter(Type, Parameter).


record_check(RPC, File, Limit, Scale, Sprint, Format, Arity, Reply) :-
	computation#events(Events),
	record_check1.

record_check1(RPC, File, Limit, Scale, Sprint, Format, Arity, Events, Reply) :-

    Reply =?= [],
    convert_to_real(Limit, Limit'),
    Limit' > 0.0,
    convert_to_real(Scale, Scale'),
    File =?= [] :
      Arity = _,
      Format = _ |
	processor # link(lookup(interface, InterOffset)),
	monitor_run + (Format = NO_FILE, FileOffset = 0, Fd = 0, Ok = true);

    Reply =?= [],
    convert_to_real(Limit, Limit'),
    Limit' > 0.0,
    convert_to_real(Scale, Scale'),
    File =\= [],
    convert_to_string(File, File') :
      Arity = _ |
	file # pwd(UID),
	make_absolute(File', UID?, FileName),
	processor # link(lookup(interface, InterOffset)),
	processor # link(lookup(file, FileOffset)),
	recordit;

    otherwise :
      Events = _,
      File = _,
      Format = _,
      RPC = _,
      Limit = _,
      Scale = _,
      Sprint = _ |
	/* should wait here */
	utils # list_to_tuple([errors | Reply], Tuple),
	wait_to_fail.

  wait_to_fail(Arity, Tuple) :-
    known(Tuple) |
	fail(run/Arity, Tuple).

  make_absolute(Name, UID, NewName) :-

    string_to_dlist(Name, [First | _], []),
    First =:= ascii('/') : UID = _,
    Name = NewName ;

    otherwise,
    string_to_dlist(Name, NL, []),
    string_to_dlist(UID, UIDNL, NL) |
	list_to_string(UIDNL, NewName);

    otherwise,
    string(UID) : Name' = '?' |
	fail("bad file name" - Name),
	self;

    UID = false(Reason) :
      NewName = '?' |
	fail(Reason - record(Name)).

  recordit(RPC, FileName, Limit, Scale, Sprint, Format,
		Events, InterOffset, FileOffset) :-
    
    known(FileName) :
      execute(FileOffset, {BOPEN, FileName, write, Fd}) |
	check_open,
	monitor_run.

  check_open(Fd, Ok) :-
    
    integer(Fd),
    Fd > 0 :
      Ok = true;

    integer(Fd),
    Fd =< 0 :
      Status = Fd /* for now */,
      Ok = false(Status).

monitor_run(RPC, Fd, Limit, Scale, Sprint, Format, Events,
		 InterOffset, FileOffset, Ok
) :-

    Ok = true,
    info(REALTIME, StartTime) :
      execute(InterOffset, sprint(Sprint, 1.0, _Output)),
      Internal = 0.0,
      Last = 0,
      State = running,
      Then = 0.0,
      Unpause = continue |
	write_channel(cutoff(Limit, _MonitorState), Scheduler, Scheduler'),
	write_channel(record(Stream), Scheduler'),
	computation # [ shell(filter_user_macros(In, Out)),
			shell("_complete_names"(N,N)), 
			spi_monitor # scheduler(Scheduler), RPC],
	serve_macros,
	serve_stream.

  serve_stream(Stream, Events, Add, Format, FileOffset, Fd, State, InterOffset,
		 Scale, Sprint, Internal, Then,	StartTime, Last) :-

    Format =?= ambient |
 	serve_ambient;

    otherwise |
	serve_transmissions.


serve_ambient(Stream, Events, Add, Format, FileOffset, Fd, State, InterOffset,
		Scale, Sprint, Internal, Then, StartTime, Last) :-

    Last =\= 0 :
      Events = _,
      Scale = 1.0,
      State = _,
      Stream = _,
      Then = _,
      Finished = file_error(Last) |
	serve_end;

    Stream =?= [] :
      Events = _,
      State = _,
      Then = _,
      Finished = closed |
	serve_end;

    Stream ? Internal',
    number(Internal'),
    Then' := Scale*Internal' :
      Internal = _,
      Then = _ |
	self;

    Stream ? start(_Name) |
	self;

    Stream ? end(_Descriptor) |
	self;

    Last =?= 0,
    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Then > 0 :
      execute(InterOffset, sprint(Sprint, Then, SL)),
      Then' = -1.0 |
	string_to_dlist(SL, DThen, [CHAR_EOL | List?]),
	ambient_line_to_list,
	ambient_line_out(DThen, Format, FileOffset, Fd, Last'),
	self;

    Last =?= 0,
    Stream ? ambient(F(A1(N1), A2(N2))),
    string(F), string(A1), number(N1), string(A2), number(N2),
    Then =< 0.0 |
	ambient_line_to_list,
	ambient_line_out(List, Format, FileOffset, Fd, Last'),
	self;

/* For ambient merge */
    Stream ? reset(_Ambient) |
	self;

    Last =?= 0,
    Stream ? ambient(terminated(system, system)) |
	self;

    Last =?= 0,
    Stream ? idle(N),
    N >= 0 |
	serve_idle;

    Last =?= 0,
    Stream ? pausing(N, Continue),
    N >= 0 :
      Data = pause(N, Continue) |
	serve_delay;

    Last =?= 0,
    Stream ? Finished, Finished =?= done(_Number) :
      Events = _,
      State = _,
      Stream' = _,
      Then = _ |
	serve_end;

    Last =?= 0,
    Stream ? note(Note) :
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
      	     to_context(spi_utils # show_value(Note, SpiOptions?, SpiValue)),
	     to_context(computation # display(term, #SpiValue,
				[wait(SpiValue), close(Stream', Stream'')]))
	    | Add'] |
	self;

    Last =?= 0,
    Stream ? item(Item) :
      Last' = _,
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
	     to_context(spi_utils # show_value(Item, SpiOptions?, SpiValue)),
	     to_context(computation#display(term, #SpiValue,
				[list, wait(SpiValue),
				 close(Stream', Stream''), copy(WriteData)]))
	    | Add'] |
	write_data,
	self;

    /* Everything which is not permitted is diagnosed here. */
    Last =?= 0,
    otherwise,
    unknown(Events),
    Format =\= NO_FILE,
    Stream ? Term :
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
	     to_context(spi_utils # show_value(Term, SpiOptions?, SpiValue)),
	     to_context(computation # display(term, ~SpiValue,
				[wait(SpiValue), close(Stream', Stream'')]))
	    | Add'] |
	self;

    Last =?= 0,
    otherwise,
    Stream ? _Any |
	self;

    Events =?= [] :
      Events' = [terminated]  |
	self;

    Events ? Event,
    Event =\= aborted,
    Event =\= terminated |
	self;

    Events ? aborted :
      Events' = _,
      State = _,
      Stream = _,
      Then = _,
      Finished = aborted |
	serve_end;

    Events =?= [terminated]:
      Events' = _,
      State = _,
      State' = terminated |
	self.


 ambient_line_to_list(F, A1, N1, A2, N2, List) :-

    convert_to_string(N2, SN2),
    string_to_dlist(SN2, DN2, [CHAR_RIGHT_PAREN]),
    string_to_dlist(A2, DA2, [CHAR_LEFT_PAREN | DN2]),
    convert_to_string(N1, SN1),
    string_to_dlist(SN1, DN1, [CHAR_RIGHT_PAREN, CHAR_SPACE | DA2]),
    string_to_dlist(A1, DA1, [CHAR_LEFT_PAREN | DN1]),
    string_to_dlist(F, DF, [CHAR_SPACE | DA1]) :
      List = DF.

  ambient_line_out(Lines, Format, FileOffset, Fd, Last) :-
    Format =\= NO_FILE,
    list_to_string(Lines, OutString) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, OutString, Last));
    otherwise :
      FileOffset = _,
      Fd = _,
      Format = _,
      Last = _,
      Lines = _ .


serve_transmissions(Stream, Events, Add, Format, FileOffset, Fd, State,
		InterOffset, Scale, Sprint, Internal, Then, StartTime, Last) :-

    Last =\= 0,
    unknown(Events) :
      Scale = 1.0,
      State = _,
      Stream = _,
      Then = _,
      Finished = file_error(Last) |
	serve_end;

    Stream =?= [] :
      Events = _,
      State = _,
      Then = _,
      Finished = closed |
	serve_end;

    Last =?= 0,
    Stream ? Internal',
    number(Internal'),
    Number := Scale*Internal',
    Format =\= NO_FILE :
      Internal = _,
      execute(InterOffset, sprint(Sprint, Number, WriteData)),
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last')) |
	self;

    Last =?= 0,
    Stream ? Internal',
    number(Internal'),
    Format =?= NO_FILE :
      Internal = _ |
	self;

    Last =?= 0,
    Stream ? start(Name),
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_PLUS | CP], WriteData),
    Format =\= NO_FILE :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(_ChannelName, _Action, _FileId)),
    Format =?= none,
    string_to_dlist(Name, CP, []),
    list_to_string([CHAR_MINUS | CP], WriteData),
    Format =\= NO_FILE :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last')) |
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, _Id)),
    Format =?= process :
      Last' = _ |
	serve_process_end,
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, Id)),
    Format =?= creator :
      Last' = _ |
	serve_creator_end,
	self;

    Last =?= 0,
    Stream ? end(Name(ChannelName, Action, Id)),
    Format =?= full :
      Last' = _ |
	serve_full_end,
	self;

    Last =?= 0,
    Stream ? ambient(_) |
	/* Just ignore it for now */
	self;

/* For ambient merge */
    Last =?= 0,
    Stream ? reset(Prefix),
    string(Prefix),
    string_to_dlist(Prefix, LPrefix, []),
    list_to_string([CHAR_BANG | LPrefix], WriteData),
    Format =\= NO_FILE :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last')) |
	self;

    Last =?= 0,
    Stream ? reset(AmbientName(UniqueId)),
    convert_to_string(UniqueId, UniqueId'),
    string_to_dlist(UniqueId', CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, CN, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CN, Prefix) :
      Stream'' = [reset(Prefix) | Stream'] |
	self;

    Last =?= 0,
    Stream ? idle(N),
    N >= 0 |
	serve_idle;

    Last =?= 0,
    Stream ? pausing(N, Continue),
    N >= 0 :
      Data = pause(N, Continue) |
	serve_delay;

    Last =?= 0,
    Stream ? Finished, Finished =?= done(Number),
    number(Number) :
      Events = _,
      State = _,
      Stream' = _,
      Then = _ |
	serve_end;

    Last =?= 0,
    Stream ? note(Note) |
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
	     to_context(spi_utils # show_value(Note, SpiOptions?, SpiValue)),
	     to_context(computation#display(term, #SpiValue,
				[wait(SpiValue), close(Stream', Stream'')]))
	    | Add'],
	self;

    Last =?= 0,
    Stream ? item(Item) :
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
	     to_context(spi_utils # show_value(Item, SpiOptions?, SpiValue)),
	     to_context(computation # display(term, #SpiValue,
				[list, wait(SpiValue),
				 close(Stream', Stream''), copy(WriteData)]))
	     | Add'] |
	write_data,
	self;

    /* Everything which is not permitted is diagnosed here. */
    Last =?= 0,
    otherwise,
    unknown(Events),
    Format =\= NO_FILE,
    Stream ? Term :
      Add = [to_context(spi_monitor # options(SpiOptions, SpiOptions)),
	     to_context(spi_utils # show_value(Term, SpiOptions?, SpiValue)),
	     to_context(computation # display(term, ~SpiValue,
				[wait(SpiValue), close(Stream', Stream'')]))
	    | Add'] |
	self;

    Last =?= 0,
    otherwise,
    Stream ? _Any |
	self;

    Events =?= [] :
      Events' = [terminated]  |
	self;

    Events ? Event ,
    Event =\= aborted,
    Event =\= terminated |
	self;

    Events ? aborted :
      Events' = _,
      State = _,
      Stream = _,
      Then = _,
      Finished = aborted |
	serve_end;

    Events =?= [terminated] :
      Events' = _,
      State = _,
      State' = terminated |
	self.


 serve_process_end(Name, ChannelName, Action, Format, FileOffset, Fd, Last) :-

    string(ChannelName),
    string_to_dlist(ChannelName, CN, []),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], WriteData),
    Format =\= NO_FILE :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last));

    ChannelName =?= self(p2c ChannelName') |
	process_inter_comm + (Capability = " p2c ");

    ChannelName =?= parent(p2c ChannelName') |
	process_inter_comm + (Capability = " c2p ");

    ChannelName =?= _parent(s2s ChannelName') |
	process_inter_comm + (Capability = " s2s ");

    ChannelName =?= _self(exit ChannelName'),
    Action =?= RECEIVED_ARROW  |
	process_inter_comm + (Capability = " expel ");

    ChannelName =?= _parent(exit ChannelName'),
    Action =?= SENT_ARROW |
	process_inter_comm + (Capability = " exit ");

    ChannelName =?= _parent(enter ChannelName'),
    Action =?= RECEIVED_ARROW |
	process_inter_comm + (Capability = " accept ");

    ChannelName =?= _parent(enter ChannelName'),
    Action =?= SENT_ARROW |
	process_inter_comm + (Capability = " enter ");

    ChannelName =?= _parent(merge(ChannelName')),
    Action =?= RECEIVED_ARROW |
	process_inter_comm + (Capability = " merge+ ");

    ChannelName =?= _parent(merge(ChannelName')),
    Action =?= SENT_ARROW |
	process_inter_comm + (Capability = " merge- ").

  process_inter_comm(Name, ChannelName, Action, Capability,
			Format, FileOffset, Fd, Last) :-

    Format =\= NO_FILE,
    string_to_dlist(ChannelName, CN, []),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], WriteData) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last)).


 serve_creator_end(Name, ChannelName, Action, Id, Format,
			FileOffset, Fd, Last) :-

    Format =\= NO_FILE,
    string(Id),
    string_to_dlist(Id, CI, []),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], WriteData) :
      ChannelName = _,
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last));

    Format =\= NO_FILE,
    Id =?= CreatedId(UniqueId),
    number(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CI]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], WriteData) :
      ChannelName = _,
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last));

    ChannelName =?= self(_InterAmbientId),
    Id =?= p2c FileId |
	creator_inter_comm + (Capability = " p2c ");

    ChannelName =?= parent(_InterAmbientId),
    Id =?= p2c FileId |
	creator_inter_comm + (Capability = " c2p ");

    Id =?= s2s FileId :
      ChannelName = _ |
	creator_inter_comm + (Capability = " s2s ");

    Id =?= exit FileId,
    Action =?= RECEIVED_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " expel ");

    Id =?= exit FileId,
    Action =?= SENT_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " exit ");

    Id =?= enter FileId,
    Action =?= RECEIVED_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " accept ");

    Id =?= enter FileId,
    Action =?= SENT_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " enter ");

    Id =?= merge(FileId),
    Action =?= RECEIVED_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " merge+ ");

    Id =?= merge(FileId),
    Action =?= SENT_ARROW :
      ChannelName = _ |
	creator_inter_comm + (Capability = " merge- ").

  creator_inter_comm(Name, Action, FileId, Capability, Format, 
			FileOffset, Fd, Last) :-

    string(FileId),
    string_to_dlist(FileId, CI, []),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	write_end_data;

    FileId =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(Capability, CC, CI),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	write_end_data.


 serve_full_end(Name, ChannelName, Action, Id, Format, FileOffset, Fd, Last) :-

    Format =\= NO_FILE,
    string(ChannelName),
    string_to_dlist(Id, CI, []),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Action, CA, [CHAR_SPACE | CN]),
    string_to_dlist(Name, CP, [CHAR_SPACE | CA]),
    list_to_string([CHAR_MINUS | CP], WriteData) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last));

    Id =?= CreatedId(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(CreatedId, CI, [CHAR_LEFT_PAREN | CU]),
    list_to_string(CI, Id') | 
	self;

    ChannelName =?= _parent(s2s ChannelName'),
    Id =?= s2s FileId |
	full_inter_comm + (Capability = " s2s ");

    ChannelName =?= self(p2c ChannelName'),
    Id =?= p2c FileId |
	full_inter_comm + (Capability = " p2c ");

    ChannelName =?= parent(p2c ChannelName'),
    Id =?= p2c FileId |
	full_inter_comm + (Capability = " c2p ");

    ChannelName =?= _parent(exit ChannelName'),
    Id =?= exit FileId,
    Action =?= RECEIVED_ARROW |
	full_inter_comm + (Capability = " expel ");

    ChannelName =?= _parent(exit ChannelName'),
    Id =?= exit FileId,
    Action =?= SENT_ARROW |
	full_inter_comm + (Capability = " exit ");

    ChannelName =?= _parent(enter ChannelName'),
    Id =?= enter FileId,
    Action =?= RECEIVED_ARROW |
	full_inter_comm + (Capability = " accept ");

    ChannelName =?=_parent(enter ChannelName'),
    Id =?= enter FileId,
    Action =?= SENT_ARROW |
	full_inter_comm + (Capability = " enter ");

    ChannelName =?= _parent(merge(ChannelName')),
    Id =?= merge(FileId),
    Action =?= RECEIVED_ARROW |
	full_inter_comm + (Capability = " merge+ ");

    ChannelName =?= _parent(merge(ChannelName')),
    Id =?= merge(FileId),
    Action =?= SENT_ARROW |
	full_inter_comm + (Capability = " merge- ").

  full_inter_comm(Name, ChannelName, Action, FileId, Capability,
			  Format, FileOffset, Fd, Last) :-

    string(FileId),
    string_to_dlist(FileId, CI, []),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	write_end_data;

    FileId =?= Id(UniqueId),
    convert_to_string(UniqueId, UniqueIdString),
    string_to_dlist(UniqueIdString, CU, [CHAR_RIGHT_PAREN]),
    string_to_dlist(Id, CI, [CHAR_LEFT_PAREN | CU]),
    string_to_dlist(ChannelName, CN, [CHAR_COLON, CHAR_SPACE | CI]),
    string_to_dlist(Capability, CC, CN),
    string_to_dlist(Action, CA, CC),
    string_to_dlist(Name, List, [CHAR_SPACE | CA]) |
	write_end_data.


write_end_data(List, Format, FileOffset, Fd, Last) :-
    Format =\= NO_FILE ,
    list_to_string([CHAR_MINUS | List], WriteData) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last)).

write_data(Format, FileOffset, Fd, WriteData, Last) :-

    Format =\= NO_FILE,    
    string(WriteData) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, WriteData, Last));

    otherwise :
      Fd = _,
      FileOffset = _,
      Format = _,
      WriteData = _,
      Last = 0.


serve_idle(Stream, Events, Add, Format, FileOffset, Fd, State, InterOffset,
		Scale, Sprint, Internal, Then, StartTime, Last, N) :-

    unknown(Events) :
      Continue = Stream,
      Data = idle(N, Stream) |
	serve_delay;

    Events =?= [] :
      Events' = [terminated]  |
	self;

    Events ? Event,
    Event =\= aborted,
    Event =\= terminated |
	self;

    Events ? Event,
    otherwise :
      Events' = _,
      State = _,
      Stream = _,
      Then = _,
      Finished = Event(N) |
	serve_end.

/*
** serve_delay: prepare to continue after pause or idle.
**
** If already finished (state "terminated", "aborted", "closed"), end!
** Otherwise, display delay and wait for continuation action.
**
*/

serve_delay(Stream, Events, Add, Format, FileOffset, Fd, State, InterOffset,
	Scale, Sprint, Internal, Then, StartTime, Last, Data, Continue) :-

    State =\= terminated, State =\= closed, State =\= aborted,
    known(InterOffset),
    Data =?= Action(Number, Continue),
    Number' := Number*Scale,
    info(REALTIME, CurrentTime),
    UsedTime := CurrentTime - StartTime :
      execute(InterOffset, sprint(Sprint, Number', NumberString)),
      Add = [to_context(utils # convert_seconds(UsedTime, Time)),
	     to_context(computation # display(term,
					(Action @ NumberString: time = Time),
					 wait(Time))) | Add'] |
	serve_continue;

    Data =?= _Action(Number, Continue),
    unknown(Add),
    otherwise :
      Events = _,
      Stream = _,
      Then = _,
      Finished = State(Number) |
	serve_end;

    Add =?= [],
    Data =?= _Action(Number, Continue) :
      Events = _,
      State = _,
      Stream = _,
      Then = _,
      Finished = closed(Number) |
	serve_end.


serve_continue(Stream, Events, Add, Format, FileOffset, Fd, State, InterOffset,
		Scale, Sprint, Internal, Then, UsedTime, Last, Continue) :-

    Events ? Event,
    Event =\= aborted,
    Event =\= closed,
    Event =\= started,
    Event =\= terminated |
	self;

    Events ? started |
	self;

    Events ? Event,
    otherwise,
    info(REALTIME, RestartTime),
    StartTime := RestartTime - UsedTime:
      Continue = _,
      Events' = _,
      State = _,
      Stream = _,
      Then = _,
      Finished = Event |
	serve_end;

    Events =?= [] :
      Events' = [terminated] |
	self;

    unknown(Events),
    Continue =?= [] :
      Continue' = _,
      Events' = [closed] |
	self;

    unknown(Events),
    Continue =\= [],
    info(REALTIME, RestartTime),
    StartTime := RestartTime - UsedTime |
	serve_stream.
      

serve_end(Finished, Add, Format, FileOffset, Fd, InterOffset, Scale, Sprint,
		 Internal, StartTime, Last) :-
	time_finished,
	end_file.


 time_finished(Finished, Add, InterOffset, Scale, Sprint, Internal,
		StartTime, Strings) :-

    string(Finished) :
      Finished' = Finished(Internal) |
	self;

    known(InterOffset),
    Finished =?= Status(Number),
    Number' := Number*Scale,
    info(REALTIME, EndTime),
    Seconds := EndTime - StartTime :
      Internal = _,
      execute(InterOffset, sprint(Sprint, Number', NumberString)),
      execute(InterOffset, sprint("seconds(%i)", Seconds, Elapsed)),
      Strings = [Finished'?, Elapsed?],
      Add = [to_context(utils # convert_seconds(Seconds, Time)),
	     to_context(computation # display(term, Status(NumberString),
				[list, copy(Finished')])),
	     to_context(computation # display(term,
					(Status @ NumberString: time = Time),
					 wait(Time)))].

  end_file(Strings, Format, FileOffset, Fd, Last) :-

    Format =\= NO_FILE |
	end_file_strings;

    otherwise :
      FileOffset = _,
      Fd = _,
      Format = _,
      Last = _,
      Strings = _ .

  end_file_strings(Strings, FileOffset, Fd, Last) :-

    Last =?= 0,
    Strings ? String,
    string(String) :
      execute(FileOffset, BWRITE(Fd, CHAR_EOL, String, Last')) |
	self;

    Last =?= 0,
    Fd > 0,
    Strings =?= [] :
      execute(FileOffset, {BCLOSE, Fd});

    otherwise :
      Fd = _,
      FileOffset = _,
      Last = _,
      Strings = _ .


/*
**  serve_macros: Monitor "recording" state shell requests to spi_monitor.
**
**  Input
**
**  In		Request stream filtered from user_macros.
**  Scheduler   Channel to inner server of spi_monitor.
**  Add         Request stream to merge (via Out) to user_macros.
**              Tail becomes [] when recording completed.
**
**  Output
**
**  Out		Filtered/merged request stream.
**  Unpause	Signal (when unknown) to spi_monitor to resume normal service.
**  Add         Set to [] when channel to spi_monitor has been closed.
**
**  Requests
**
**  pause	request spi_monitor to defer input requests.
**  continue    request spi_monitor to serve all requests, until next pause.
**  record_data(Data)
**		request spi_monitor to flatten Data and add string to record.
**  record_item(Item)
**		request spi_monitor to flatten Item and add string, prefixed
**              "#" to record.
**  record_note(Note)
**		request spi_monitor to display Note, prefixed "#".
**
**  A request may have an additional argument, Reply, which is set "true"
**  when the request is queued to spi_monitor.
*/

serve_macros(In, Out, Add, Scheduler, Unpause) :-

    channel(Scheduler),
    In ? pause :
      Unpause = continue,
      write_channel(pause(Unpause'?), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? pause(true^) :
      Unpause = continue,
      write_channel(pause(Unpause'?), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? continue :
      Unpause = continue |
	check_monitor(Scheduler, Scheduler', Add, Add'),
	self;

    channel(Scheduler),
    In ? continue(true^) :
      Unpause = continue |
	check_monitor(Scheduler, Scheduler', Add, Add'),
	self;

    channel(Scheduler),
    In ? record_data(Data) :
      write_channel(record_data(Data), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? record_data(Data, true^) :
      write_channel(record_data(Data), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? record_item(Item) :
      write_channel(record_item(Item), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? record_item(Item, true^) :
      write_channel(record_item(Item), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? record_note(Note) :
      write_channel(record_note(Note), Scheduler, Scheduler') |
	self;

    channel(Scheduler),
    In ? record_note(Note, true^) :
      write_channel(record_note(Note), Scheduler, Scheduler') |
	self;

    In ? "_complete_names"(Head, Tail) :
      Head' = [continue, pause, record_data, record_item, record_note | Head],
      Out ! "_complete_names"(Head', Tail) |
	check_monitor(Scheduler, Scheduler', Add, Add'),
	self;	

    In ? "_all_functors"(Head, Tail) :
      Head' = [continue, pause, record_data, record_item, record_note | Head],
      Out ! "_all_functors"(Head', Tail) |
	check_monitor(Scheduler, Scheduler', Add, Add'),
	self;	

    In ? Other,
    otherwise :
      Out ! Other |
	check_monitor(Scheduler, Scheduler', Add, Add'),
	self;

    unknown(Add),
    In = [] :
      Scheduler = _,
      Out = [],
      Unpause = continue;

    Add ? Command :
      Out ! Command |
	self;

    Add =?= [] :
      Scheduler = _,
      In = Out,
      Unpause = continue.


  /* If channel to monitor has closed, signal end of recording - []. */

  check_monitor(Scheduler, Scheduler', Add, Add') :-

    true :
      Add = Add',
      write_channel(debug(_), Scheduler, Scheduler');

    otherwise :
      Add = _,
      Scheduler = Scheduler',
      Add' = [].
