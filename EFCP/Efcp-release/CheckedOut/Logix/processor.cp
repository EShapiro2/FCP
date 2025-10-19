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
      Utility Procedures for Logix Monitors.
      1987
*/

-monitor(serve).
-mode(system).
-language([evaluate, compound]).
-include(file_constants).

serve(In) :-
    In = [] | true;

    list(In) :
      link(["file.o"], Offset) |
	processor_server # start(Queue?, Ready),
	server(In, Ready?, Offset, Queue).

server(In, Ready, Offset, Queue) :-

    unknown(Ready),
    In ? {file(directory(Name), Ok'), Common} :
      Common = reply(Ok, Ok, Ok', done) |
	server,
	directory(Offset, Name, Ok);

    unknown(Ready),
    In ? {file(get_module(FileName, Data'), Ok'), Common} :
      Common = reply(Ok, Data, Data', Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	get_module(Offset, FileName, Data, Ok);

    unknown(Ready),
    In ? {file(info(FileName, FileDate'), Ok'), Common} :
      Common = reply(Ok, FileDate, FileDate', Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	info(Offset, FileName, FileDate, Ok);

    unknown(Ready),
    In ? {file(working_directory(WorkingDirectory), Ok'), Common} :
      Common = reply(WD, WD, WorkingDirectory, Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	working_directory(Offset, WD, Ok);

    unknown(Ready),
    otherwise,
    In ? {Other, Common} :
      Queue ! {Other, Common} |
	server;

    Ready = ready : Offset = _,
      Queue = In .


info(Offset, FileName, FileDate, Ok) :-
    known(FileName) :
      execute(Offset,{FILEINFO, FileName, FileDate, _}) |
	check_filedate(FileDate, Ok).

get_module(Offset, FileName, Data, Ok) :-
    known(FileName) :
      execute(Offset,{GET_MODULE, FileName, Data, Reply}) |
	check_reply(Reply, 0, Ok).

directory(Offset, DirName, Ok) :-
    known(DirName) :
      execute(Offset,{ISDIRECTORY, DirName, Status, _}) |
	check_reply(Status, 1, Ok).

check_reply(Reply, Good, Ok) :-

    Reply = Good :
      Ok = true ;

    Reply =\= Good :
      Ok = false(Reply) .

check_filedate(FileDate, Ok) :-

    string(FileDate) :
      Ok = true ;

    otherwise :
      Ok = false(FileDate) .

working_directory(Offset, WorkingDirectory, Ok) :-
    true :
      execute(Offset, {GETWD, _, WD, _}),
      Ok = true |
	add_slash(WD, WorkingDirectory).

add_slash(WD, WorkingDirectory) :-

    WD = '/' :
      WorkingDirectory = WD ;

    string_to_dlist(WD, LeadingChars, TrailingSlash),
    LeadingChars = [Slash, _ | _] :
      TrailingSlash = [Slash] |
	list_to_string(LeadingChars, WorkingDirectory).
