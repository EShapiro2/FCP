/*
** This module is part of EFCP.
**

     Copyright 2007 Ehud Shapiro
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
-mode(failsafe).
-language([compound, colon]).

/*	expand/2

	Expand macro commands.

	expand(Command, Commands\Commands1)

	->	Command  is a console command.
	<=	Commands\Commands1  is a difference list of commands to the
		system-macro processor and the user shell.

	Commands which are not expanded here are forwarded for system-macro
	expansion and user shell processing.
*/

expand(Command, Cs) :-

    unknown(Command) |
	defer_command(Command, Command, Cs);

    tuple(Command),
    arg(1, Command, V),
    unknown(V) |
	defer_command(Command, V, Cs);

    otherwise |
	expand_all.

  defer_command(Command, V, ([shell(defer(V, Command)) | Commands]\Commands)^).

expand_all(Command, Cs) :-

    Command = hi :
      Cs = Commands\Commands |
	computation # display(term, hello);

% add your macros here....
% To retain system-macro and normal shell capabilities, forward generated
% commands to the shell via the  Commands  difference list.

    Command = forward(Any) :
      Cs = [Any | Commands]\Commands;

    Command = macros :
      Cs = [close(user_macros),
	    to_context([self # service_id(SID),
	    		computation # shell(change_context, SID)])
	   | Commands]\Commands;

    Command = "_complete_names"(Head, Tail) :
      Tail = [forward,  hi, macros | Tail'],
      Cs = ["_complete_names"(Head, Tail') | Commands]\Commands;

    Command = "_all_functors"(Head, Tail) :
      Tail = [forward, hi, macros | Tail'?],
      Cs = ["_all_functors"(Head, Tail') | Commands]\Commands;

    otherwise :
      Cs = [Command | Commands]\Commands |
	true.
