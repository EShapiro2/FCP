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
       Quiet server
       1992
*/

-export(io).
-mode(trust).
-language([inherit, dfcp]).

/*
** io - Copy terminal input; intercept terminal output.
**
** Close input when no more characters and device control finishes.
** Log terminal output to processor_room.
**
*/

procedure io([Integers], [Integer], Vector).

io(Bytes, TCH) :-
      TCH = TC!,
      processor # [device(create(tty, Chars)),
		   device(open(tty)),
		   device(signals(Ds)) | Out?],
      copy_input(Chars?, Ds?, Bytes, TC!),
      serve_output(TC?, Out).

/*
** copy_input - copy terminal input.
**
** When input is closed and device tream is closed:
** close (copied) input stream and close terminal output channel.
*/

copy_input(Chars, Ds, Bytes, TC) :-

    Chars ? C |
	Bytes ! C,
	self;

    Ds ? _ |
	self;

    Chars = [], Ds = [] |
	Bytes = [],
	close_channel(TC).

/*
** serve_output - format & log output lines.
*/

serve_output(In, Out) :-

  In ? outputln(Operand, Common) |
      compose_message([Operand, "
"		      ], Message),
      log_message;

  In ? Opcode(Operand, Common),
  Opcode =\= outputln |
      compose_message(Operand, Message),
      log_message;

  In ? _Opcode(_Operand, Abort),
  known(Abort) |
      self;

  In = [] |
      Out = [] .

compose_message(Operand, Message) :-

  string(Operand) |
      Message = Operand ;

  list(Operand) |
      operand_strings(Operand, Strings),
      utils # append_strings(Strings?, Message);

  otherwise |
      operand_strings([Operand], Strings),
      utils # append_strings(Strings?, Message).

operand_strings(Os, Strings) :-

  Os ? bytes(Bs) |
      list_to_string(Bs, String),
      Strings ! String?,
      self;

  Os ? String,
  string(String) |
      Strings ! String,
      self;

  Os ? I,
  integer(I) |
      integer_to_string(I, String),
      Strings ! String?,
      self;

  Os ? List,
  List = [Car | Cdr] |
      Os'' = [Car, Cdr | Os'?],
      self;

  Os ? confirm(true^) |
      self;

  Os ? _,
  otherwise |
      self;

  Os = [] |
      Strings = [].

log_message(In, Out, Common, Message) :-

  Message = "" |
      Common = true,
      serve_output;

  writable(Common),
  Message =\= "" |
      Out ! room(log(Message, Ok)),
      serialize;

  known(Common) |
      Message = _,
      serve_output.

serialize(In, Out, Common, Ok) :-

  known(Ok) |
      Common = done,
      serve_output;

  known(Common) |
      Ok = _,
      serve_output.
