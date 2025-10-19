/*
** This module is part of EFCP.
**

     Copyright 2007 Michael Hirsch, William Silverman
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
       Interface Monitor - Communicate with UNIX
       Feb 1986
*/

-export([start/1]).
-mode(trust).
-language(compound).

procedure convert_date(String, String, String).
procedure date(String).
procedure date_time(String, String, String).
procedure errno(Integer, String).
procedure getenv(String, String).
procedure gmtime(String).
procedure gmt2date(String, String).
/* procedure date2gmt(String, String). */
procedure gmdate(String).
procedure gm2local(String, String).
procedure home_dir(String, String).
procedure net_host(String, String, String).
procedure processid(String).
procedure unix_call(String).
procedure user_data({String, String}).
procedure whoami(String).

SystemReply ::= true | false(String).


start(In) :-
	processor # link(lookup(interface, Offset)),
	server(In, Offset).


server(In, Offset) :-

    In ? {convert_date(FullDate, Date?, Time?), Ok, Common} |
	convert_date(FullDate, DateString, Ok, Common),
	date_time(DateString, Date, Time),
	server;

    In ? {date(Date?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, date(Date)),
	server;

    In ? {date_time(FullDate?, Date?, Time?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, date(FullDate)),
	date_time(FullDate?, Date, Time),
	server;

    In ? {errno(Errno, Diagnostic?), Ok, Common} |
	errno(Offset, Errno, Diagnostic, Ok, Common),
	server;

    In ? {getenv(Name, Value?), Ok, Common} |
	str2val(getenv, Name, Value, Offset, Ok, Common),
	server;

    In ? {gmtime(Time?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, gmtime(Time)),
	server;

    In ? {gmt2date(Time, Date?), Ok, Common} |
	str2val(gmt2date, Time, Date, Offset, Ok, Common),
	server;

    In ? {gmdate(Date?), Ok, Common} :
      Common = done,
      Ok = true |
	execute(Offset, gmdate(Date)),
	server;

    In ? {gm2local(Gm, Local?), Ok, Common} |
	str2val(gm2local, Gm, Local, Offset, Ok, Common),
	server;

    In ? {home_dir(Name, Directory?), Ok, Common} |
	home_directory(Offset, Name, Directory, Ok, Common),
	server;

    In ? {net_host(Name?, Id?, Domain?), Ok, Common} |
	net_host(Offset, Name, Id, Domain, Ok, Common),
	server;

    In ? {processid(Name?), Ok, Common} :
      Common = done,
      Ok = true |
	processid(Offset, Name),
	server;

    In ? {sprint(Format, Value, OutputString?), Ok, Common} |
	format_value(Format, Value, OutputString, Offset, Ok, Common),
	server;

    In ? {unix_call(String), Ok, Common} |
	unix_call(String, Offset, Ok, Common),
	server;

    In ? {user_data({Name?, Directory?}^), Ok, Common} :
      Common = done,
      Ok = true |
	whoami(Offset, Name),
	home_directory(Offset, Name, Directory, Ok, Common),
	server;

    In ? {whoami(Name?), Ok, Common} :
      Common = done,
      Ok = true |
	whoami(Offset, Name),
	server;

    otherwise ,
    In ? _Other(Ok, Common) :
      Ok = false(unknown),
      Common = done |
	server;

    In ? _Any(Ok, Common),
    known(Common) :
      Ok = false(aborted) |
	self;

    In = [] : Offset = _ .


convert_date(GivenDate, FullDate, Ok, Common) :-

    string(GivenDate),
    string_length(GivenDate) =:= 12 :
      FullDate = GivenDate,
      Ok = true,
      Common = done;

    string(GivenDate),
    string_length(GivenDate) =:= 14 :
      FullDate = GivenDate,
      Ok = true,
      Common = done;

    otherwise : GivenDate = _,
      FullDate = yymmddhhmmss,
      Ok = false(wrong_length),
      Common = done ;

    known(Common) : GivenDate = _,
      FullDate = "            ",
      Ok = false(aborted) .
      

execute(Offset, Tuple) :-
    true :
      execute(Offset, Tuple) .

str2val(Function, String, Value, Offset, Ok, Common) :-

    string(String) :
      Ok = true,
      Common = done,
      execute(Offset, Function(String, Value)) ;

    otherwise,
    unknown(Common) : Function = _, String = _, Value = _, Offset = _,
      Ok = false(not_a_valid_string),
      Common = done ;

    known(Common) : Function = _, String = _, Value = _, Offset = _,
      Ok = false(aborted) .


date_time(FullDate, Date, Time) :-
    string(FullDate),
    string_to_dlist(FullDate, [Y1, Y2, M1, M2, D1, D2,
			       H1, H2, I1, I2, S1, S2
			      ],
		    []
    ) :
      ascii('/', S),
      ascii(':', C) |
	list_to_string([D1, D2, S, M1, M2, S, Y1, Y2], Date),
	list_to_string([H1, H2, C, I1, I2, C, S1, S2], Time);

    string(FullDate),
    string_to_dlist(FullDate, [_Y1, _Y2, Y3, Y4, M1, M2, D1, D2,
			       H1, H2, I1, I2, S1, S2
			      ],
		    []
    ) :
      ascii('/', S),
      ascii(':', C) |
	list_to_string([D1, D2, S, M1, M2, S, Y3, Y4], Date),
	list_to_string([H1, H2, C, I1, I2, C, S1, S2], Time).

errno(Offset, Errno, Diagnostic, Ok, Common) :-

    integer(Errno) :
      execute(Offset, errno(Errno, Diagnostic)),
      Common = done,
      Ok = true ;

    otherwise : Offset = _,
      Common = done,
      Diagnostic = "",
      Ok = false(invalid - Errno) ;

    known(Common) : Offset = _, Errno = _,
      Diagnostic = "",
      Ok = false(aborted).

unix_call(Call, Offset, Ok, Common) :-

    string(Call) :
      Common = Done?,
      execute(Offset, system(Call, Terminate, ErrorNumber)) |
	unix_call_reply;

    otherwise : Call = _, Offset = _,
      Ok = false(not_a_string),
      Common = done ;

    known(Common) : Call = _, Offset = _,
      Ok = false(aborted) .

unix_call_reply(Terminate, Offset, Ok, ErrorNumber, Done) :-

    Terminate = 0 : Offset = _, ErrorNumber = _,
      Done = done,
      Ok = true ;

    Terminate < 0 :
      execute(Offset, errno(ErrorNumber, ErrorString)) |
	unix_call_error;

    Terminate > 0,
    Code := Terminate/256 : Offset = _, ErrorNumber = _,
      Done = done,
      Ok = false(terminate(Code)).

unix_call_error(ErrorNumber, ErrorString, Ok, Done) :-

    ErrorString =\= "" : ErrorNumber = _,
      Done = done,
      Ok = false(ErrorString) ;

    ErrorString = "" :
      Done = done,
      Ok = false(errno(ErrorNumber)) .

whoami(Offset, Name) :-
    true :
      execute(Offset, whoami(Name)) .


processid(Offset, Name) :-
    true :
      execute(Offset, processid(Name)) .


home_directory(Offset, Name, Directory, Ok, Common) :-

    known(Name) :
      execute(Offset, homedir(Name, Directory)),
      Ok = true,
      Common = done ;

    known(Common) : Offset = _, Name = _, Directory = _,
      Ok = false(aborted) .

net_host(Offset, Name, Id, Domain, Ok, Common) :-

    true :
      execute(Offset, nethost(Name, Id, Domain)),
      Ok = true,
      Common = done ;

    known(Common) : Offset = _, Name = _, Id = _, Domain = _,
      Ok = false(aborted) .


format_value(Format, Value, OutputString, Offset, Ok, Common) :-

    unknown(Common),
    string(Format),
    Value @< [] :
      execute(Offset, sprint(Format, Value, OutputString)),
      Ok = true,
      Common = done;

    unknown(Common),
    string(Format),
    Value @< [],
    otherwise :
      Offset = _,
      OutputString = _,
      Ok = false(improper_format(Format)),
      Common = done;      

    unknown(Common),
    Value @< [],
    otherwise :
      Offset = _,
      OutputString = _,
      Ok = false(not_a_string(Format)),
      Common = done;

    unknown(Common),
    string(Format),
    otherwise :
      Offset = _,
      OutputString = _,
      Ok = false(not_a_number_or_string(Value)),
      Common = done;

    known(Common) : Format = _, Offset = _, OutputString = _, Value = _,
      Ok = false(aborted) .
