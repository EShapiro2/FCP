/*
** This module is part of EFCP.
**

     Copyright 2007 William SIlverman
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
       Prepare/Produce c-format specs/outut, using extension interface:sprint
       02/08/2007
*/

-language(compound).
-export([sprint/2, sprint_list/3, prepare/3, prepare/4]).
-mode(interrupt).

/*        Initial Predicate

prepare(<specification>, <format>, <value>)

 <specification> ::= <letter>(<cspec_value>)
                     <value>

 <letter>        ::= c | d | e | E | f | g | G | i | o | s | u | x | X

 <cspec_value>   ::= <value>
                     <cspec> , <value>

 <cspec>         ::= <parameters>
                     "<flags>"
                     "<flags>" , <parameters>

 <parameters>    ::= <integer>
                     <integer> , <integer>

 <flag>          ::= - | + | <space> | 0 | #

 <flags>         ::= <flag>
                     <flag> <flags>

 <value>         ::= <number>
                     <string>

**        Interface

sprint(<specification> , <output_string>)

sprint_list([<specifications>], [<strings>]^, [<errors>]^)

prepare(<specification>, <format>^, <value>^)

prepare(<specification>, <format>^, <value>^, [<errors>]^)

*/

sprint(Specification, Display) :-
    tuple(Specification) :
      Display = Display'? |
	processor#link(lookup(interface, Offset)),
	prepare,
	screen#display_stream(Errors, prefix(Specification)),
	execute_sprint.

sprint_list(Specifications, Displays, ErrorList) :-
    true :
      Displays = Displays'?,
      ErrorList = ErrorList'? |
	processor#link(lookup(interface, Offset)),
	sprints.

sprints(Specifications, Displays, ErrorList, Offset) :-
    Specifications ? Specification :
      Displays ! Display,
      ErrorList ! Errors? |
	prepare,
	execute_sprint,
	self;

    Specifications =\= [_|_],
    Specifications =\= [] :
      Specifications' = [Specifications] |
	self;

    Specifications =?= [] :
      Offset = _,
      Displays = [],
      ErrorList = [].

prepare(Specification, Format, Value) + (Errors = _) :-
    true :
      Errors = Errors'?,
      Format = Format'?,
      Value = Value'? |
	generate_string_list,
	utils # append_strings(["%" | StringList], Format').

  generate_string_list(Specification, StringList, Value, Errors) :-

    integer(Specification) :
      Errors = [],
      StringList = [i],
      Value = Specification;

    real(Specification) :
      Errors = [],
      StringList = ["G"],
      Value = Specification;

    string(Specification) |
      Errors = [],
      StringList = ["s"],
      Value = Specification; 

    Specification =?= Functor(Constant),
    Constant @< [] :
      StringList = [Letter?] |
	validate_functor_value(Functor, Constant, Letter, Value, Errors);

    Specification =?= Functor(Flags, Constant),
    Constant @< [],
    string(Flags),
    string_to_dlist(Flags, MDL, []) :
      StringList = [Flags'?, Letter?] |
	validate_flags(MDL, Flags', Errors, Errors'?),
	validate_functor_value(Functor, Constant, Letter, Value, Errors');

    Specification =?= Functor(Flags, Width, Constant),
    string(Flags),
    string_to_dlist(Flags, MDL, []) :
      StringList = [Flags'?, Width'?, Letter?] |
	validate_flags(MDL, Flags', Errors, Errors'),
	validate_parameter(Width, Width', Errors', Errors''?),
	validate_functor_value(Functor, Constant, Letter, Value, Errors'');

    Specification =?= Functor(Flags, Width, Precision, Constant),
    string(Flags),
    string_to_dlist(Flags, MDL, []) :
      StringList = [Flags'?, Width'?, ".", Precision'?, Letter?] |
	validate_flags(MDL, Flags', Errors, Errors'),
	validate_parameter(Width, Width', Errors', Errors''?),
	validate_parameter(Precision, Precision', Errors'', Errors'''?),
	validate_functor_value(Functor, Constant, Letter, Value, Errors''');

    Specification =?= Functor(Width, Constant),
    number(Width) :
      StringList = [Width'?, Letter?] |
	validate_parameter(Width, Width', Errors, Errors'?),
	validate_functor_value(Functor, Constant, Letter, Value, Errors');

    Specification =?= Functor(Width, Precision, Constant),
    number(Width) :
      StringList = [Width'?, ".", Precision'?, Letter?] |
	validate_parameter(Width, Width', Errors, Errors'?),
	validate_parameter(Precision, Precision', Errors', Errors''?),
	validate_functor_value(Functor, Constant, Letter, Value, Errors'');

    otherwise :
      Specification = _,
      Errors = [unrecognised_specification],
      StringList = [],
      Value = 0.

  validate_flags(MDL, OkFlags, Errors, NextErrors) + (Flags = []) :-

    MDL = [] :
      Errors = NextErrors |
	list_to_string(Flags, OkFlags);

    MDL ? F,
    F =:= ascii("+") :
      Flags' = [F | Flags] |
	self;

    MDL ? F,
    F =:= ascii("-") :
      Flags' = [F | Flags] |
	self;

    MDL ? F,
    F =:= ascii(" ") :
      Flags' = [F | Flags] |
	self;

    MDL ? F,
    F =:= ascii("0") :
      Flags' = [F | Flags] |
	self;

    MDL ? F,
    F =:= ascii("#") :
      Flags' = [F | Flags] |
	self;

    MDL ? O,
    otherwise,
    list_to_string([O],Other) :
      Errors ! unrecognised_flag(Other) |
	self.

  validate_parameter(Item, OkItem, Errors, Errors') :-

    convert_to_integer(Item, Item'),
    0 =< Item' :
      Errors = Errors',
      OkItem = Item';

    otherwise :
      Errors ! invalid_parameter(Item),
      OkItem = 0.

  validate_functor_value(Functor, Constant, Letter, Value, Errors) :-

    Functor = d,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = i,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = o,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = x,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = "X",
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = u,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = c,
    convert_to_integer(Constant, I) :
      Letter = Functor,
      Value = I,
      Errors = [];

    Functor = s,
    convert_to_string(Constant, S) :
      Letter = Functor,
      Value = S,
      Errors = [];

    Functor = f,
    convert_to_real(Constant, R) :
      Letter = Functor,
      Value = R,
      Errors = [];

    Functor = e,
    convert_to_real(Constant, R) :
      Letter = Functor,
      Value = R,
      Errors = [];

    Functor = "E",
    convert_to_real(Constant, R) :
      Letter = Functor,
      Value = R,
      Errors = [];

    Functor = g,
    convert_to_real(Constant, R) :
      Letter = Functor,
      Value = R,
      Errors = [];

    Functor = "G",
    convert_to_real(Constant, R) :
      Letter = Functor,
      Value = R,
      Errors = [];

    otherwise :
      Letter = i,
      Value = 0,
      Errors = [cannot_convert-Functor(Constant)].

/*************************** utilities **************************************/

execute_sprint(Format, Value, Offset, Display) :-

    string(Format),
    Value @< [],
    integer(Offset), Offset < 0 |
      execute(Offset, sprint(Format, Value, Display));

    otherwise :
      Offset = _,
      Display = "" |
	screen#display(cant_sprint(Format, Value), type(ground)).
