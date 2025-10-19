/*
** This module is part of EFCP.
**

     Copyright 2007 Norm Mccain, Phil Thrift
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
       sio line_editor.cp
       Texas Instruments Inc. 1986
*/

-export([requests/1]).
-mode(interpret).
-language([compound, colon]).

requests(In) :-
	ring#ring(Iring,20),
	ring#ring(Kring,10),
	edit(In,Iring,Kring,[]).
	 
% The line editor maintains streams to an input ring and a kill ring.
% It handles the edit_line message, returning one line at a time.  It
% receives bytes from the keyboard (via sio) and responds to the editing
% keystrokes described below.  Other keystrokes insert characters into
% the line.
	 
edit([edit_line(KbdIn,Out,Echo,Done)|In],Iring,Kring,Extends) :-
	edit_line(KbdIn,Out,Echo,Iring\Iring1,Kring\Kring1,Extends\Extends1,
				Done),
	edit(In,Iring1,Kring1,Extends1).
edit([complete_names(List)|In],Iring,Kring,[]^) :-
	extend_names(Extends,List),
	edit(In,Iring,Kring,Extends).
edit([],[]^,[]^,[]^).

% In these descriptions "point" refers to the position in a line
% represented by the left edge of the keyboard cursor.  It is always
% between characters (never on them).

% - <backspace> deletes the character to the left of point.
% - Control-D deletes the character to the right of point.

% - Control-A moves point to the beginning of a line.
% - Control-E moves point to the end of a line.

% - Control-U kills all input to the left of point.
% - Control-K kills all input to the right of point.

% - Control-B moves point left one character in a line.
% - Control-F moves point right one character in a line.

% - Control-T swaps the characters just to the left and right of point.

% - Control-X interprets the following sequence of keystrokes (^P, ^N, and
%   <esc> are acceptable) as directives aimed at the kill ring rather than the
%   input ring (which is the default.)

% - <tab> completes the current name (if it is a prefix of any  predefined
%    name).

% - <esc> initializes either the input or kill ring.

% - Control-N retrieves the next entry in the input or kill ring.  Repeating
%   ^N (or ^P) gives successive next (or previous) entries in the same ring.
% - Control-P retrieves previous entries in the input or kill ring.  Repeating
%   ^P (or ^N) gives successive previous (or next) entries in the same ring.

% - Control-R re-displays the current line; point is unchanged.

% - Control-G terminates input and output.

edit_line(KbdIn\KbdIn1,Out,Echo,Iring,Kring,Ndiff,Done) :-
	edit_line(KbdIn,[],[],Echo,Out,KbdIn1,Iring,Kring,Ndiff,Done).

edit_line([],_Left,_Right,[]^,(Eos\_/Eos)^,[]^,
		(Iring\Iring)^,(Kring\Kring)^,(Extends\Extends)^,true^
).
edit_line([Control_G|_],_Left,_Right,[]^,(Eos\_/Eos)^,[]^,
		(Iring\Iring)^,(Kring\Kring)^,(Extends\Extends)^,true^
) :-
    Control_G =:= ascii('') |
	true.
edit_line([LF|Cs],[],[],[LF]^,(Out\_/Out)^,Cs^,
		(Iring\Iring)^,(Kring\Kring)^,(Extends\Extends)^,true^
) :-
    LF =:= ascii(lf) |
	true.
edit_line([LF|Cs],Left,Right,Echo,(Out\Out1/_)^,Cs^,
		Iring,(Kring\Kring)^,(Extends\Extends)^,Done
) :-
    otherwise,
    LF =:= ascii(lf) |		  % (Left==[_|_]; Right==[_|_])
	shift_rear(Right,Left,Echo,Left1,[LF]),
	reverse_line(Left1,Out\Out1,Iring,Done).
edit_line([BS|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done) :-
    BS =:= ascii(bs) :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([BS|Cs],[_Erased|Left],Right,[BS|Echo]^,Out,Rs,Iring,Kring,Ndiff,
				Done
) :-
    BS =:= ascii(bs) |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Delete|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Delete =:= ascii(del) :
       ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Delete|Cs],[_Erased|Left],Right,[BS|Echo]^,Out,Rs,
			Iring,Kring,Ndiff,Done
) :-
    Delete =:= ascii(del) :
      ascii(bs,BS) |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_D|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    Control_D =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_D|Cs],Left,[_Erased|Right],Echo,Out,Rs,Iring,Kring,Ndiff,
				Done
) :-
    Control_D =:= ascii('') |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,Left,Right,Echo1,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_U|Cs],Left,Right,Echo,Out,Rs,
		Iring,([put(Left1)|Kring]\Kring1)^,Ndiff,Done
) :-
    Control_U =:= ascii('') |
	delete_left(Left,[],Left1,Right,Echo\Echo1),
	edit_line(Cs,[],Right,Echo1,Out,Rs,Iring,Kring\Kring1,Ndiff,Done).
edit_line([Control_K|Cs],Left,Right,Echo,Out,Rs,
		Iring,([put(Right)|Kring]\Kring1)^,Ndiff,Done
) :-
    Control_K =:= ascii('') |
	delete_right(Right,Echo\Echo1),
	edit_line(Cs,Left,[],Echo1,Out,Rs,Iring,Kring\Kring1,Ndiff,Done).
edit_line([Control_B|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    Control_B =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_B|Cs],[C|Left],Right,[BS|Echo]^,Out,Rs,Iring,Kring,Ndiff,
				Done
) :-
    Control_B =:= ascii('') :
      ascii(bs,BS) |
	edit_line(Cs,Left,[C|Right],Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_F|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    Control_F =:= ascii('') :
       ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_F|Cs],Left,[C|Right],[C|Echo]^,Out,Rs,Iring,Kring,Ndiff,
				Done
) :-
    Control_F =:= ascii('') |
	edit_line(Cs,[C|Left],Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_T|Cs],[],Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    Control_T =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,[],Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_T|Cs],Left,[],[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    Control_T =:= ascii('') :
      ascii(bel,Bell) |
	edit_line(Cs,Left,[],Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_T|Cs],[C1|Left],[C2|Right],[BS,C2,C1,BS|Echo]^,Out,Rs,
		Iring,Kring,Ndiff,Done
) :-
    Control_T =:= ascii('') :
       ascii(bs,BS) |
	edit_line(Cs,[C2|Left],[C1|Right],Echo,Out,Rs,Iring,Kring,Ndiff,Done
).
edit_line([Control_A|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Control_A =:= ascii('') |
	shift_front(Left,Right,Echo,Right1,Echo1),
	edit_line(Cs,[],Right1,Echo1,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_E|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Control_E =:= ascii('') |
	shift_rear(Right,Left,Echo,Left1,Echo1),
	edit_line(Cs,Left1,[],Echo1,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_P|Cs],Left,Right,Echo,Out,Rs,
	([previous(Cs \Cs1,Right,Echo\Echo1,Line)|Iring]\Iring1)^,Kring,
				Ndiff,Done
) :-
    Control_P =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring\Iring1,Kring,Ndiff,Done
).
edit_line([Control_N|Cs],Left,Right,Echo,Out,Rs,
	([next(Cs \Cs1,Right,Echo\Echo1,Line)|Iring]\Iring1)^,Kring,Ndiff,
				Done
) :-
    Control_N =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring\Iring1,Kring,Ndiff,Done
).
edit_line([Control_X|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Control_X =:= ascii('') |
	kill_ring_entry(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Control_R|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Control_R =:= ascii('') |
	display_left(Left,Echo,Echo1),
	display_right(Right,Echo1,Echo2),
	edit_line(Cs,Left,Right,Echo2,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([Esc|Cs],Left,Right,Echo,Out,Rs,	% Iring,Kring,Ndiff,Done) :-
		([initialize|Iring]\Iring1)^,Kring,Ndiff,Done
) :-
    Esc =:= ascii('') |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring\Iring1,Kring,Ndiff,Done).

/* Complete name (on Left) */
edit_line([Control_I|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,
		([extend(Left,LeftR)|Extends]\Extends1)^,Done
) :-
    Control_I =:= ascii('	') |
	list_to_string(LeftR,LeftRS),
	string_to_dlist(LeftRS,Cs',Cs),
	edit_line(Cs'?,Left,Right,Echo,Out,Rs,Iring,Kring,
				Extends\Extends1,Done
).
/* Unless no completions are supplied */
edit_line([Control_I|Cs],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
    Control_I =:= ascii('	'),
    otherwise |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).

edit_line([C|Cs],Left,[],[C|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done) :-
    otherwise |
	edit_line(Cs,[C|Left],[],Echo,Out,Rs,Iring,Kring,Ndiff,Done).
edit_line([C|Cs],Left,Right,[C|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done) :-
    otherwise |
	display_right(Right,Echo,Echo1),
	edit_line(Cs,[C|Left],Right,Echo1,Out,Rs,Iring,Kring,Ndiff,Done).

kill_ring_entry([Control_P|Cs],Left,Right,Echo,Out,Rs,
	Iring,([previous(Cs\Cs1,Right,Echo\Echo1,Line)|Kring]\Kring1)^,Ndiff,
				Done
) :-
    Control_P =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring,Kring\Kring1,Ndiff,Done
).
kill_ring_entry([Control_N|Cs],Left,Right,Echo,Out,Rs,
	Iring,([next(Cs\Cs1,Right,Echo\Echo1,Line)|Kring]\Kring1)^,Ndiff,Done
) :-
    Control_N =:= ascii('') |
	insert_left(Line,Left,Left1),
	edit_line(Cs1,Left1,Right,Echo1,Out,Rs,Iring,Kring\Kring1,Ndiff,Done
).
kill_ring_entry([Esc|Cs],Left,Right,Echo,Out,Rs,
		Iring,([initialize|Kring]\Kring1)^,Ndiff,Done
) :-
    Esc =:= ascii('') |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring\Kring1,Ndiff,Done).
kill_ring_entry([_|Cs],Left,Right,[Bell|Echo]^,Out,Rs,Iring,Kring,Ndiff,Done
) :-
    otherwise :
       ascii(bel,Bell) |
	edit_line(Cs,Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).
kill_ring_entry([],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done) :-
	edit_line([],Left,Right,Echo,Out,Rs,Iring,Kring,Ndiff,Done).


display_left(Left,Echo,Echo1) :-
    true :
      ascii(bs, BS) |
	display_left(Left,BS,Echo,Echo1).

display_left([C|Left],BS,[BS|BSs]^,Echo) :-
	display_left(Left,BS,BSs,[C|Echo]).
display_left([],_,Echo^,Echo).

display_right(Right,Echo,Echo1) :-
    true : ascii(bs,BS) |
	display_right(Right,Echo,BS,Echo1).

display_right([C|Right],[C|Echo]^,BS,BSs) :-
	display_right(Right,Echo,BS,[BS|BSs]).
display_right([],[SP,BS|BSs]^,BS,BSs) :-
    true :
      ascii(' ', SP) |
	true.

reverse_line(Left,Out,Ring,Done) :-
	reverse_line(Left,Out,[],Ring,Done).

reverse_line([],(E\E)^,List,([put(List)|Ring]\Ring)^,true^).
reverse_line([C|Cs],E\Line,List,Ring,Done) :-
	reverse_line(Cs,E\[C|Line],[C|List],Ring,Done).

delete_left([],Out,Out^,_Right,(Echo\Echo)^).
delete_left([C|Cs],In,Out,Right,([BS|Echo]\Echo2)^) :-
    true :
      ascii(bs,BS) |
	display_right(Right,Echo,Echo1),
	delete_left(Cs,[C|In],Out,Right,Echo1\Echo2).

delete_right(Right,Echo\Echo1) :-
	delete_right(Right,Echo,Echo1).

delete_right([_|Right],[SP|Echo]^,BSs) :-
    true :
      ascii(bs,BS), ascii(' ',SP) |
	delete_right(Right,Echo,[BS|BSs]).
delete_right([],BSs^,BSs).

shift_front([C|Left],Right,[BS|Echo]^,Right1,Echo1) :-
    true :
      ascii(bs,BS) |
	shift_front(Left,[C|Right],Echo,Right1,Echo1).
shift_front([],Right,Echo,Right^,Echo^).

shift_rear([C|Right],Left,[C|Echo]^,Left1,Echo1) :-
	shift_rear(Right,[C|Left],Echo,Left1,Echo1).
shift_rear([],Left,Echo,Left^,Echo^).

insert_left([C|Line],Left,Left1) :-
	insert_left(Line,[C|Left],Left1).
insert_left([],Left^,Left).


extend_names(In, List) :-

  In ? extend(Chars, Extension?^) |
        reverse_chars(Chars, false, Tail, Chars'),
        extend_name(Chars'?, List, Tail, Extension),
        self;

  In = [] :
    List = _.


reverse_chars(Chars, IsString, Tail, NewChars) :-

  Chars ? C, integer(C),
  ascii('a') =< C, C =< ascii('z') :
    IsString = _,
    IsString' = true,
    Tail' = [C | Tail] |
	self;

  Chars ? C, integer(C),
  C =?= ascii('$') :
    IsString = _,
    IsString' = true,
    Tail' = [C | Tail] |
	self;

  Chars ? C, integer(C),
  ascii('A') =< C, C =< ascii('Z') :
    IsString = _,
    IsString' = false,
    Tail' = [C | Tail] |
	self;

  Chars ? C, integer(C),
  C =:= ascii('_') :
    IsString = _,
    IsString' = false,
    Tail' = [C | Tail] |
	self;

  Chars ? C, integer(C),
  ascii('0') =< C, C =< ascii('9') :
    IsString = _,
    IsString' = false,
    Tail' = [C | Tail] |
	self;

  otherwise,
  IsString =?= true :
    Chars = _,
    NewChars = Tail;

  otherwise :
    Chars = _,
    IsString = _,
    Tail = _,
    NewChars = [].
    

extend_name(Chars, List, Tail, Extended) :-

  List =?= [String | _],
  string_to_dlist(String, SList, []) :
    Chars = SList,
    Tail = Extended;

  List ? _String,
  otherwise |
	self;

  List =\= [_|_], List =\= [] :
    List' = [List] |
	self;

  otherwise :
    Chars = _,
    List = _,
    Tail = _,
    Extended = [].
