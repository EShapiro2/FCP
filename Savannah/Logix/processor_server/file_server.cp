/*
** This module is part of EFCP.
**

     Copyright 2007 Daniel Szoke, William Silverman
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
       file_server - manages file access via foreign kernels.
       1987
*/

-language([evaluate, compound]).
-mode(trust).
-export([server/4]).
-include(file_constants).

/*
** file_server assumes that all output variables are locked .
**
** These are:
**
**    get_file, get_module: Data, Ok
**    file_info: FileDate, Ok
**    All Others: Ok
**
*/

server(In, Offset, Interface, Defer) :-

    In ? {Goal, Ok, Common},    
    Goal = append(FileName, Data) |
	data_command(Offset, Interface, APPEND_FILE, FileName, Data, Ok, Common),
	self;

    In ? {Goal, Ok, Common},    
    Goal = directory(DirName) |
	directory(Offset, Interface, DirName, Ok, Common),
	self;

    In ? {Goal, Ok, Common},    
    Goal = get(FileName, Data') :
      Common = reply(Ok, Data, Data', Common') |
	data_command(Offset, Interface, GET_FILE, FileName, Data, Ok, Common'),
	self;


    In ? {Goal, Ok, Common},    
    Goal = get_module(FileName, Data') :
      Common = reply(Ok, Data, Data', Common') |
	data_command(Offset, Interface, GET_MODULE, FileName, Data, Ok, Common'),
	self;

    In ? {Goal, Ok, Common},    
    Goal = info(FileName, FileDate') :
      Common = reply(FileDate, FileDate, FileDate', Common')  |
	info(Offset, Interface, FileName, FileDate, Ok, Common'),
	self;

    In ? {Goal, Ok, Common},    
    Goal = put(FileName, Data) |
	data_command(Offset, Interface, PUT_FILE, FileName, Data, Ok, Common),
	self;

    In ? {Goal, Ok, Common},    
    Goal = put_module(FileName, Data) |
	data_command(Offset, Interface, PUT_MODULE, FileName, Data, Ok, Common),
	self;

    In ? {Goal, Ok, Common},
    Goal = working_directory(WorkingDirectory) :
      Common = reply(WD, WD, WorkingDirectory, Common') |
	working_directory(Offset, Interface, WD, Ok, Common'),
	self;

    In ? {Goal, Ok, Common},    
    Goal = read(FileName, Eol, Data) |
	read(Offset, Interface, FileName, Eol, Data, Ok, Common),
	self;

    In ? {Goal, Ok, Common},    
    Goal = write(FileName, Eol, DataStream) |
	write(Offset, Interface, FileName, Eol, DataStream, Ok, Common),
	self;

    In ? {Goal, Ok, Common},    
    Goal = append(FileName, Eol, DataStream) |
	append(Offset, Interface, FileName, Eol, DataStream, Ok, Common),
	self;

    otherwise,
    In ? _Other(Ok, Common) :
      Common = done,
      Ok = false(unknown) |
	self;

    In ? _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    In ? {Goal, Ok, Common},    
    Goal =\= working_directory(_),
    arg(1, Goal, Name), unknown(Name) :
      write_channel(defer(Name, {file(Goal, Ok), Common}), Defer) |
	self;

% copied:

    In ? Request,
    Request = _(Ok, Abort), known(Abort) : Offset = _,
      Ok = false(aborted) |
	self;

    In = [] : Offset = _, Interface = _, Defer = _ .


reply_with_errno(Reply, Interface, Status) :-

    Reply >= 0 :
      execute(Interface, errno(Reply, Status)) ;

    Reply < 0,
    Reply' := -Reply :
      execute(Interface, errno(Reply', Status)) .


/*
**
** Requests are:
**
**    get, get_module: Data
**    put, put_module, append : no assumption
**    info: FileDate
**    directory: Status
**    working_directory: no assumption
**
*/


procedure data_command(Offset, Interface, Command, FileName, Data, Ok, Common).

data_command(Offset, Interface, Command, FileName, Data, Ok, Common) :-

    known(FileName) :
      execute(Offset,{Command, FileName, Data, Reply}),
      Common = done |
	check_reply(Reply, 0, Interface, Ok);

    known(Common) : Offset = _, Interface = _, Command = _,
			FileName = _, Data = _,
      Ok = false(aborted) .

check_reply(Reply, Good, Interface, Ok) :-

    Reply = Good : Interface = _,
      Ok = true ;

    Reply =\= Good,
    Good = 1 : Interface = _,		% special case
      Ok = false(not_directory);

    Reply =\= Good,
    Good = 0 :
      Ok = false(Status?) |
	reply_with_errno.


procedure working_directory(Offset, Interface, WorkingDirectory).

working_directory(Offset, Interface, WorkingDirectory, Ok, Common) :-
    true :
      execute(Offset, {GETWD, _, WD, _}),
      Ok = true | Interface = _,
	unify_without_failure(Common, done),
	add_slash(WD, WorkingDirectory).

procedure add_slash(WD, WorkingDirectory).

add_slash(WD, WorkingDirectory) :-

    WD = '/' :
      WorkingDirectory = WD ;

    string_to_dlist(WD, LeadingChars, TrailingSlash),
    LeadingChars = [Slash, _ | _] :
      TrailingSlash = [Slash] |
	list_to_string(LeadingChars, WorkingDirectory).


procedure info(Offset, Interface, FileName, FileDate, Ok, Common).

info(Offset, Interface, FileName, FileDate, Ok, Common) :-

    known(FileName) :
      execute(Offset,{FILEINFO, FileName, FileDate, _}),
      Common = done |
	check_filedate;

    known(Common) : Offset = _, Interface = _, FileName = _, FileDate = _,
      Ok = false(aborted) .

check_filedate(FileDate, Interface, Ok) :-

    string(FileDate) : Interface = _,
      Ok = true ;

    otherwise :
      Ok = false(Status?) |
	reply_with_errno(FileDate, Interface, Status).


procedure directory(Offset, Interface, DirName, Ok, Common).

directory(Offset, Interface, DirName, Ok, Common) :-

    known(DirName) :
      execute(Offset,{ISDIRECTORY, DirName, Status, _}),
      Common = done |
	check_reply(Status, 1, Interface, Ok);

    known(Common) : Offset = _, Interface = _, DirName = _,
      Ok = false(aborted) .


procedure read(Offset, Interface, FileName, Access, DataStream, Ok, Common).

read(Offset, Interface, FileName, Eol, Data, Ok, Common) :-

    known(FileName) :
      execute(Offset,{BOPEN, FileName, read, Fd}) |
	check_open_and_read(Offset, Interface, Fd, Eol, Data, Ok, Common).


check_open_and_read(Offset, Interface, Fd, Eol, Data, Ok, Common) :-

    integer(Fd),
    Fd > 0 :
      Ok = true |
        read_file(Offset, Interface, Fd, Eol, Data, Common);

    otherwise : Offset = _, Eol = _, Data = _,
      Ok = false(Status?),
      Common = done |
	reply_with_errno(Fd, Interface, Status);

    known(Common) : Offset = _, Interface = _, Fd = _, Eol = _, Data = _,
      Ok = false(aborted) .


fix_status(Status, Interface, NewStatus) :-

    Status = 0 : Interface = _,
      NewStatus = true;

    Status < 0,
    Status' := -Status : Interface = _ |
	fix_status1;

    Status > 0 :
      NewStatus = false(Status'?) |
	reply_with_errno(Status, Interface, Status').

  fix_status1(Status, NewStatus) :-

    Status = 1 :
      NewStatus = exception(eof);

    Status = 2 :
      NewStatus = exception(unexpected_eof);

    Status = 3 :
      NewStatus = exception(invalid_input_data);

    Status = 4 :
      NewStatus = exception(invalid_output_data);

    Status = 10 :
      NewStatus = exception(conversion_failed);

    otherwise,
    Status' := -Status :
      NewStatus = exception(Status').




read_file(Offset, Interface, Fd, Eol, Data, Common) :-

    Data ? Info :
      execute(Offset,{BREAD, Fd, Eol, Data_Item, Status}) |
	fix_status(Status, Interface, Status'),
	unify_if_possible(Info, {Data_Item, Status'}, Common, Common'),
        self;

    Data = [] : Eol = _, Interface = _,
      Common = done |
	close_file(Offset, Fd);

    known(Common) : Eol = _, Interface = _, Data = _ |
	close_file(Offset, Fd).


unify_if_possible(Var, Val, Common, NewCommon) :-

    true :
      Var = Val,
      NewCommon = Common;

    otherwise :
      Var = _, Val = _,
      Common = fork(exception( invalid_parameter_format,
			      received(Var) - expected(Val) ),
		   NewCommon);

    known(Common) : Var = _, Val = _,
      NewCommon = Common .


close_file(Offset, Fd) :-

    true :
      execute(Offset, {BCLOSE, Fd}) ;

    otherwise : Offset = _, Fd = _ .


procedure write(Offset, Interface, FileName, Access, DataStream, Ok, Common).

write(Offset, Interface, FileName, Eol, DataStream, Ok, Common) :-
	do_open_and_write(Offset, Interface, FileName, write, Eol, DataStream,
				Ok, Common
	).


procedure append(Offset, Interface, FileName, Access, DataStream, Ok, Common).

append(Offset, Interface, FileName, Eol, DataStream, Ok, Common) :-
	do_open_and_write(Offset, Interface, FileName, append, Eol, DataStream,
				Ok, Common
	).


do_open_and_write(Offset, Interface, FileName, Access, Eol, DataStream,
			Ok, Common
) :-

    known(FileName) :
      execute(Offset, {BOPEN, FileName, Access, Fd}) |
	check_open_and_write(Offset, Interface, Fd, Eol, DataStream,
				Ok, Common
	);

    known(Common) : Offset = _, Interface = _, FileName = _,
		    Access = _, Eol = _, DataStream = _,
      Ok = false(aborted) .


check_open_and_write(Offset, Interface, Fd, Eol, Data, Ok, Common) :-

    integer(Fd),
    Fd > 0 :
      Ok = true |
        write_file(Offset, Interface, Fd, Eol, Data, Common);

    integer(Fd),
    Fd =< 0,
    otherwise : Offset = _, Eol = _, Data = _,
      Ok = false(Status?),
      Common = done |
	reply_with_errno(Fd, Interface, Status);

    known(Common) : Eol = _, Interface = _, Data = _,
      Ok = false(aborted) |
	close_file(Offset, Fd).


write_file(Offset, Interface, Fd, Eol, Data, Common) :-

    Data ? {Data_Item, Status} :
      execute(Offset, {BWRITE, Fd, Eol, Data_Item, StatusOut}) |
	fix_status(StatusOut, Interface, StatusOut'),
	unify_if_possible(Status, StatusOut', Common, Common'),
        write_file;

    Data = [] : Eol = _, Interface = _,
      Common = done |
	close_file(Offset, Fd);

    Data ? Junk,
    Junk =\= {_,_} :
      Common = fork(exception(invalid_parameter_format), Common') |
	write_file;

    known(Common) : Eol = _, Interface = _, Data = _|
	close_file(Offset, Fd).
