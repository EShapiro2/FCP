/*
** This module is part of EFCP.
**

     Copyright 2007 Ehud Shapiro, William Silverman
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
       Utilities
       1985
*/

-export([	append_strings/2, chars_to_lines/2, evaluate/2,
		ground/2, ground_stream/2, unparse/2,
		binary_merge/2,binary_sort_merge/2,
		binary_merge/3,binary_sort_merge/3,
		tuple_to_dlist/3, list_to_tuple/2,
		integer_to_dlist/3, list_length/2, make_safe_tuple/3,
		convert_milliseconds/2, convert_seconds/2,
		convert_time/3, convert_time/4,
		freeze_term/3, ground_list/2
	]
).
-mode(interrupt).
-language([evaluate, compound, colon]).

/***************************** Constants ************************************/

CHAR_DOUBLE_QUOTE => 34.
CHAR_DOLLAR => 36.
CHAR_PRIME => 39.
CHAR_SINGLE_QUOTE => 39.
CHAR_COLON => 58.
CHAR_QUESTION_MARK => 63.
CHAR_UNDERSCORE => 95.

CHAR_0 => 48.
CHAR_9 => 57.

CHAR_a => 97.
CHAR_z => 122.

CHAR_A => 65.
CHAR_Z => 90.

/****************************************************************************/

send(Utility, Done, Value, Result) :-

    known(Done) : Utility = _,
      Result = Value ;

    otherwise : Done = _ |
	fail(Utility, Value =\= Result).

procedure chars_to_lines([Integer], [String]).

chars_to_lines(Chars, Lines) :-
	chars_to_lines(Chars, Lines1, Done),
	send(chars_to_lines, Done, Lines1, Lines).

chars_to_lines(Chars, Lines, Done) :-

    Chars ? C |
	chars_to_lines(C, Chars', Lines, Done);

    Chars = [] :
      Lines = [],
      Done = done ;

    otherwise :
      Lines = [],
      Done = done |
	fail(chars_to_lines, not_a_list(Chars)).

chars_to_lines(Char, Chars, Lines, Done) :-

    Char =:= ascii(cr) |
	chars_to_lines(Chars, Lines, Done);

    Char =:= ascii(lf) |
	chars_to_lines(Chars, Lines, Done);

    otherwise :
      Lines ! String? |
	one_line(Char, Chars, List, Chars', List, ClosedList),
	list_to_string(ClosedList, String),
	chars_to_lines(Chars', Lines', Done).

one_line(Chars, List, Chars1, OpenList, ClosedList) :-

    Chars ? Char |
	one_line(Char, Chars', List, Chars1, OpenList, ClosedList);

    Chars = [] :
      List = [],
      Chars1 = [],
      OpenList = ClosedList ;

    otherwise :
      List = [],
      Chars = Chars1,
      OpenList = ClosedList .

one_line(Char, Chars, List, Chars1, OpenList, ClosedList) :-

    -128 =< Char, Char =< 127 |
	one_char(Char, Chars, List, Chars1, OpenList, ClosedList);

    otherwise :
      ascii('?', Q), 
      List ! Q |
	fail(chars_to_lines, not_a_character(Char)),
	one_line(Chars, List', Chars1, OpenList, ClosedList).

one_char(Char, Chars, List, Chars1, OpenList, ClosedList) :-

    Char =\= 13 /* 13 =:= ascii(cr) */,
    Char =\= 10 /* 10 =:= ascii(lf) */:
      List ! Char |
	one_line(Chars, List', Chars1, OpenList, ClosedList);

    otherwise : Char = _,
      List = [],
      Chars = Chars1,
      OpenList = ClosedList .


procedure append_strings([StIn], String).

StIn ::= String ; Number.

append_strings(Strings, String) :-
	append_strings(Strings, List, List, String'),
	send(append_strings, String', String', String).

append_strings(Strings, List1, List2, String) :-
    Strings ? SN |
	append_strings(SN, Strings', List1, List2, String);

    Strings = [],
    list(List1) :
      List2 = [] |
	list_to_string(List1, String);

    Strings = [],
    List1 = List2 :
      String = '' ;    

    otherwise :
      Strings' = [Strings] |
	append_strings.

append_strings(SN, Strings, List1, List2, String) :-

    string(SN),
    string_to_dlist(SN, List2^, List3) |
	append_strings(Strings, List1, List3, String);
	
    number(SN),
    convert_to_string(SN, IS), string_to_dlist(IS, List2^, List3) |
	append_strings(Strings, List1, List3, String);

    SN = [] |
	append_strings(Strings, List1, List2, String);

    invalid(SN) : Strings = _, List1 = _, List2 = _,	% The result is
      String = SN ;	% invalid in the same way as the first invalid element.

    otherwise :
      ascii('?', Q),
      List2 ! Q |
	fail(append_strings, not_a_string(SN)),
	append_strings(Strings, List1, List2', String).

procedure ground_stream([Any], [Any]).

ground_stream([], []^).
ground_stream(Xs, Ys) :-

    Xs ? X |
	ground(X, done, Done),
	send(ground_stream, Done, [X | Ys'?], Ys),
	ground_stream;

    otherwise |
	ground(Xs, done, Done),
	send(ground_stream, Done, Xs, Ys).


procedure ground(Any, Any).

ground(Term, Grounded) :-
	ground(Term, done, Done),
	send(ground, Done, Term, Grounded).

ground(Term, Done1, Done2) :-

    Term ? Car |
	ground(Car, Done1, Done1'),
	ground;

    tuple(Term), arity(Term, N) |
	ground_args(Term, N, Done1, Done2);

    otherwise : Term = _,
      Done1 = Done2 .


ground_args(_, 0, Done, Done^).
ground_args(Term, N, Done1, Done2) :-
    N > 0,
    arg(N, Term, A),
    N' := N - 1 |
	ground(A, Done1, Done1'),
	ground_args.


procedure unparse(Any, Any).

/*
** Return parsed term to pre-parsed form:
**
** Variables: replace "_var"(X) and "_ro"(Y) by X and Y?, respectively;
**
** Strings: requote as necessary;
**
** Tuples: quote first argument, if necessary.
**
** Call fcp syntax server to identify infix, prefix, postfix operators,
** appearing as the first argument of 2-tuples and 3-tuples.
**
** The ouput, when flattened to a string (e.g. by terms_to_string) and
** re-parsed, agrees with the input.
*/

unparse(Term, Result?^) :-
    known(Term) :
      make_channel(Operator, Strings) |
	test_strings(Strings?, Syntax),
	computation#parse#syntax#fcp#serve(Syntax?, False),
	not_fixop(False?),
	unparse_term(Term, Operator, Unparsed, Unparsed, Grounded),
	close_and_send(unparse, Operator, Grounded, Grounded, Result).

  test_strings(Strings, Syntax) :-

    Strings ? infix(String, Reply) :
      Syntax ! infix_operator(String, _, _, _, Reply) |
	self;

    Strings ? onefix(String, Reply) :
      Syntax = [prefix_operator(String, _, _, Reply1),
		postfix_operator(String, _, _, Reply2)
	       | Syntax'?] |
	test_replies(Reply1, Reply2, Reply),
	self;

    Strings ? anyfix(String, Reply) :
      Syntax = [prefix_operator(String, _, _, Reply1),
		postfix_operator(String, _, _, Reply2),
		infix_operator(String, _, _, _, Reply3)
	       | Syntax'?] |
	test_replies(Reply1, Reply2, Reply3, Reply),
	self;

    Strings =?= [] :
      Syntax = [].

  test_replies(true, _, true^).
  test_replies(_, true, true^).
  test_replies(_, _, false^) :- otherwise | true.

  test_replies(true, _, _, true^).
  test_replies(_, true, _, true^).
  test_replies(_, _, true, true^).
  test_replies(_, _, _, false^) :- otherwise | true.

not_fixop(False) :-

    False ? {_, _, _, _, false^} |
      self;

    False ? {_, _, _, _, _, false^} |
      self;

    False = [] : true.

close_and_send(Utility, Operator, Done, Value, Result) :-
    known(Done) :
      close_channel(Operator) |
	send.

unparse_term(Term, Operator, Unparsed, Left, Right) :-

    Term ? Car :
      Unparsed ! Car' |
	unparse_term(Car, Operator, Car', Left, Left'),
	self;

    tuple(Term) |
	unparse_tuple(Term, Operator, Unparsed, Left, Right);

    string(Term), Term =\= '' :
      write_channel(anyfix(Term, Reply), Operator) |
	unparse_string_reply;

    string(Term), Term =?= '' :
      Operator = _,
      Unparsed = "''",
      Left = Right;

    otherwise :
      Operator = _,
      Term = Unparsed,
      Left = Right .

  unparse_string_reply(Term, Reply, Unparsed, Left, Right) :-

    /* This is incorrect if the name includes a double-quote! */

    Reply =?= true,
    string_to_dlist(Term, DS, [CHAR_DOUBLE_QUOTE]),
    list_to_string([CHAR_DOUBLE_QUOTE | DS], String) :
      Unparsed = String,
      Left = Right;

    Reply =?= false |
	unparse_string + (String = Term).

  unparse_string(String, Unparsed, Left, Right) :-

    /* Uppercase letter, digit, underscore or Single quote (Prime), is ok
       within an unquoted alphanumeric string, but NOT as first character. */

    string_to_dlist(String, List, Tail),
    List =?= [UpperCase | _],
    CHAR_A =< UpperCase, UpperCase =< CHAR_Z :
      Tail = [CHAR_DOUBLE_QUOTE] |
	alphanumeric_string,
	requote_string;

    string_to_dlist(String, List, Tail),
    List =?= [Digit | _],
    CHAR_0 =< Digit, Digit =< CHAR_9 :
      Tail = [CHAR_DOUBLE_QUOTE] |
	alphanumeric_string,
	requote_string;

    string_to_dlist(String, List, Tail),
    List =?= [CHAR_UNDERSCORE | _] :
      Tail = [CHAR_DOUBLE_QUOTE] |
	alphanumeric_string,
	requote_string;

    string_to_dlist(String, List, Tail),
    List =?= [CHAR_SINGLE_QUOTE | _] : 
      Tail = [CHAR_DOUBLE_QUOTE] |
	alphanumeric_string,
	requote_string;

    string_to_dlist(String, List, Tail),
    otherwise |
	/* First character is not Uppercase, digit, underscore or
           single quote. */
	alphanumeric_string,
	requote_string.

  requote_string(String, Unparsed, Left, Right, NewList, Tail) :-

    Tail =?= [] :
      NewList = _,
      String = Unparsed,
      Left = Right;

    Tail =?= [Quote],
    list_to_string([Quote | NewList], QuotedString) :
      String = _,
      Unparsed = QuotedString,
      Left = Right.

alphanumeric_string(List, Tail, NewList) :-

    List ? C,
    CHAR_a =< C, C =< CHAR_z :
      NewList ! C |
	self;

    List ? C,
    CHAR_A =< C, C =< CHAR_Z :
      NewList ! C |
	self;

    List ? C,
    CHAR_0 =< C, C =< CHAR_9 :
      NewList ! C |
	self;

    List ? C,
    C =?= CHAR_UNDERSCORE :
      NewList ! C |
	self;

    List ? C,
    C =?= CHAR_DOLLAR :
      NewList ! C |
	self;

    List ? C,
    C =?= CHAR_PRIME :
      NewList ! C |
	self;

    List =?= [CHAR_DOUBLE_QUOTE | _] |
	double_quote;
	
    List ? C,	/* non-alphanumeric in list */
    otherwise :
      NewList ! C,
      Tail = [CHAR_DOUBLE_QUOTE] |
	self;

    unknown(List) :
      Tail = [],
      NewList = [].

  double_quote(List, Tail, NewList) :-

    List =?= Tail :
      NewList = Tail;

    List =\= Tail,
    List ? _ :
      NewList = [CHAR_DOUBLE_QUOTE, CHAR_DOUBLE_QUOTE | NewList'] |
	alphanumeric_string;

    unknown(Tail),
    List ? C :
      Tail = [C],
      NewList = [C, C | NewList'] |
	alphanumeric_string.
	
unparse_tuple(Term, Operator, Unparsed, Left, Right) :-

    Term = `Name :
      Functor = '"_var"' |
	unparse_variable;

    Term = ?Name :
      Functor = '"_ro"' |
	unparse_variable;

    Term = Functor(_Operand),
    string(Functor), Term =\= `_, Term =\= ?_ :
      write_channel(onefix(Functor, Reply), Operator, Operator') |
	unparse_tuple_reply;

    Term = Functor(_Operand1, _Operand2),
    string(Functor) :
      write_channel(infix(Functor, Reply), Operator, Operator') |
	unparse_tuple_reply;

    otherwise :
      Reply = false |
	unparse_tuple_reply.

  unparse_tuple_reply(Term, Operator, Unparsed, Left, Right, Reply) :-

    Reply = true,
    Term = Functor(Operand) :
      Unparsed = Functor(Unparsed') |
	unparse_term + (Term = Operand);

    Reply = true,
    Term = Functor(Operand1, Operand2) :
      Unparsed = Functor(UnparsedOperand1, UnparsedOperand2) |
	unparse_term(Operand1, Operator, UnparsedOperand1, Left, Right'),
	unparse_term(Operand2, Operator, UnparsedOperand2, Right', Right);

    Reply = false,
    N := arity(Term),
    make_tuple(N, Tuple) :
      Tuple = Unparsed |
	unparse_args.

  unparse_args(Term, Operator, Tuple, Left, Right, N) :-

    N =:= 0 :
      Operator = _,
      Term = _,
      Tuple = _,
      Left = Right;

    N =?= 1,
    arity(Term) > 1,
    arg(1, Term, String),
    string(String),
    arg(1, Tuple, Unparsed) :
      Operator = _ |
	unparse_string;

    otherwise,
    arg(N, Term, Arg),
    arg(N, Tuple, Arg'),
    N-- |
	unparse_args,
	unparse_term(Arg, Operator, Arg', Right', Right).


unparse_variable(Name, Operator, Unparsed, Left, Right, Functor) :-

    string(Name),
    string_to_dlist(Name, List, Tail),
    List =?= [CHAR_UNDERSCORE | _] :
      Operator = _ |
	alphanumeric_string,
	requote_variable;

    string(Name),
    string_to_dlist(Name, List, Tail),
    List =?= [UpperCase | _],
    CHAR_A =< UpperCase, UpperCase =< CHAR_Z :
      Operator = _ |
	alphanumeric_string,
	requote_variable;

    otherwise :	/* First character is not underscore or capitalized 
		   or Name is not a string */
      Unparsed = Functor(Unparsed') |
	unparse_term + (Term = Name).

  requote_variable(Name, Unparsed, Left, Right, Functor, NewList, Tail) :-

    Tail =?= [],
    Functor = '"_var"' :
      NewList = _,
      Name = Unparsed,
      Left = Right;

    Tail =?= [],
    Functor = '"_ro"',
    string_to_dlist(Name, DN, [CHAR_QUESTION_MARK]),
    list_to_string(DN, Name') :
      NewList = _,
      Name' = Unparsed,
      Left = Right;

    Tail =?= [Quote],
    list_to_string([Quote | NewList], QuotedString) :
      Name = _,
      Unparsed = Functor(QuotedString),
      Left = Right.


procedure evaluate(Any, Number).

evaluate(Expression, Result) :-
	compute_value(Expression, Value),
	send(evaluate, Value, Value, Result).

compute_value(Expression, Value) :-

    Expression = Expression' + Addend |
	self,
	compute_value(Addend, AValue),
	Value := Value' + AValue;

    Expression = Expression' - Subtrahend |
	self,
	compute_value(Subtrahend, SValue),
	Value := Value' - SValue;

    Expression = Expression' * Multiplicand |
	self,
	compute_value(Multiplicand, MValue),
	Value := Value' * MValue;

    Expression = Expression' / Divisor |
	self,
	compute_value(Divisor, DValue),
	Value := Value' / DValue;

    Expression = div(Expression', Divisor) |	% alias
	self,
	compute_value(Divisor, DValue),
	Value := Value' / DValue;

    Expression = Expression' \ Modulus |
	self,
	compute_value(Modulus, MValue),
	check_integer(Value', AInt),
	check_integer(MValue, BInt),
	Value := AInt \ BInt;

    Expression = mod(Expression', Modulus) |	% alias
	self,
	compute_value(Modulus, MValue),
	compute_value(Modulus, MValue),
	check_integer(Value', AInt),
	check_integer(MValue, BInt),
	Value := AInt \ BInt;

    Expression = Expression' /\ Andend |
	self,
	compute_value(Andend, BValue),
	check_integer(Value', AInt),
	check_integer(BValue, BInt),
	Value := AInt /\ BInt;

    Expression = Expression' \/ Orend |
	self,
	compute_value(Orend, OValue),
	check_integer(Value', AInt),
	check_integer(OValue, BInt),
	Value := AInt \/ BInt;

    Expression = ~Expression' |
	self,
	check_integer(Value', AInt),
	Value := ~AInt;

    Expression = bitwise_and(A, B) |	% alias
	compute_value(A /\ B, Value);

    Expression = bitwise_or(A, B) |	% alias
	compute_value(A \/ B, Value);

    Expression = bitwise_not(A) |	% alias
	compute_value(~A, Value);

    Expression = max(A, B) |
	compute_value(A, Aval),
	compute_value(B, Bval),
	Value := max(Aval, Bval);

    Expression = min(A, B) |
	compute_value(A, Aval),
	compute_value(B, Bval),
	Value := min(Aval, Bval);

    Expression = abs(A) |
	compute_value(A, Aval),
	Value := abs(Aval);

    Expression = round(A), A >= 0 |
	compute_value(A, Aval),
	Value := integer(Aval + 0.5);

    Expression = round(A), A < 0 |
	compute_value(A, Aval),
	Value := integer(Aval - 0.5);

    Expression = real(A) |
	compute_value(A, Aval),
	Value := real(Aval);

    Expression = sin(A) |
	compute_value(A, Aval),
	Value := sin(Aval);

    Expression = cos(A) |
	compute_value(A, Aval),
	Value := cos(Aval);

    Expression = tan(A) |
	compute_value(A, Aval),
	Value := tan(Aval);

    Expression = sqrt(A) |
	compute_value(A?,Aval),
	Value := sqrt(Aval);

    Expression = asin(A) |
	compute_value(A?,Aval),
	Value := asin(Aval);

    Expression = acos(A) |
	compute_value(A?,Aval),
	Value := acos(Aval);

    Expression = atan(A) |
	compute_value(A?,Aval),
	Value := atan(Aval);

    Expression = ln(A) |
	compute_value(A?,Aval),
	Value := ln(Aval);

    Expression = log(A) |
	compute_value(A?,Aval),
	Value := ln(Aval);

    Expression = exp(A) |
	compute_value(A?,Aval),
	Value := exp(Aval);

    Expression = pow(A,B) |
	compute_value(A,Aval),
	compute_value(B,Bval),
	Value := pow(Aval?,Bval?);

    Expression = log(A,B) |
	compute_value(A,Aval),
	compute_value(B,Bval),
	Value := log(Aval?,Bval?);

    Expression = random |
	Value := random;

    Expression = integer(A) |
	compute_value(A, Aval),
	Value := integer(Aval);

    number(Expression) :
      Expression = Value ;

    Expression = +Expression' |
	self;

    Expression = -Expression' |
	self,
	Value := -Value';

    Expression = ascii(S),
    string(S) |
	ascii_value(S, Value);

    Expression = arity(T),
    Value^ := arity(T) |
	true;

    Expression = arity(L),
    list(L) :
      Value = 2 ;

    Expression = string_hash(S),
    Value^ := string_hash(S) |
	true;

    Expression = string_length(S),
    Value^ := string_length(S) |
	true;

    Expression = nth_char(N, S),
    nth_char(N, S, V) :
      Value = V ;

    Expression = length(S),
    string(S),
    Value^ := string_length(S) |
	true;

    Expression = length(L),
    list(L) |
	list_length(L, 0, Value);

    otherwise :
      Value = 0 |
	fail(evaluate,can't_evaluate(Expression)).

check_integer(AInt, AInt^) :-
    integer(AInt) |
	true.
check_integer(Error, 1^) :-
    otherwise |
	fail(evaluate, not_an_integer(Error)).

ascii_value(S, Value) :-

    string_to_dlist(S, [Value]^, []) |
	true;

    S = bel :
      ascii(bel, Value) ;

    S = bs :
      ascii(bs, Value) ;

    S = lf :
      ascii(lf, Value) ;

    S = cr :
      ascii(cr, Value) ;

    S = esc :
      ascii(esc, Value) ;

    S = del :
      ascii(del, Value) ;

    otherwise :
      Value = 0 |
	fail(evaluate,can't_convert_to_ascii(S)).


List ::= [Any].

procedure binary_merge([List], List).

binary_merge(In, Out) :-
	ordered_merger(Merger),
	binary_merge(In, Out, Merger).


Merger ::= [ordered_merge(List, List, List)].

procedure binary_merge([List], List, Merger).

binary_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_merge_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_merge_server(In, Odds, Evens, PairList) :-

    In ? Odds^ |
	level1_merge_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_merge_server(In, Evens, PairList) :-

    In ? Evens^ :
      PairList ! {One, Two} |
	level1_merge_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .

procedure binary_sort_merge(List, List).

binary_sort_merge(In, Out) :-
	ordered_merger(Merger),
	binary_sort_merge(In, Out, Merger).

procedure binary_sort_merge(List, List, Merger).

binary_sort_merge(In, Out, Merger) :-
    true :
      make_channel(CH, Merger) |
	level1_sort_server(In, Odds, Evens, PairList),
	binary_merger(Odds, Evens, PairList, Out, CH, done, Done),
	close_merger(Done, CH).

level1_sort_server(In, Odds, Evens, PairList) :-

    In ? Odd :
      Odds = [Odd] |
	level1_sort_server(In', Evens, PairList);

    otherwise : In = _,
      Odds = [], Evens = [],
      PairList = [] .

level1_sort_server(In, Evens, PairList) :-

    In ? Even :
      Evens = [Even],
      PairList ! {One, Two} |
	level1_sort_server(In', One, Two, PairList');

    otherwise : In = _,
      Evens = [],
      PairList = [] .


binary_merger(In1, In2, PairList, Out, CH, Left, Right) :-

    list(In2) :
      write_channel(ordered_merge(In1, In2, In1'), CH) |
	binary_merger2(PairList, In2', PairList', CH, Left, Left'),
	binary_merger;

    In2 = [] : PairList = _, CH = _, 
      In1 = Out,
      Left = Right .

binary_merger1(PairList, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      UpList ! {Out1, Out2},
      write_channel(ordered_merge(In1, In2, Out1), CH) |
	binary_merger2(PairList', Out2, UpList', CH, Left, Right);

    PairList = [] : CH = _,
      UpList = [],
      Left = Right .

binary_merger2(PairList, Out, UpList, CH, Left, Right) :-

    PairList ? {In1, In2} :
      write_channel(ordered_merge(In1, In2, Out), CH) |
	binary_merger1(PairList', UpList, CH, Left, Right);

    PairList = [] : CH = _,
      Out = [],
      UpList = [],
      Left = Right .
/*
** Basic ordered merge - merge ground terms without duplication.
*/

ordered_merger(In) :-
    In ? ordered_merge(In1, In2, Out) |
	ordered_merge(In1, In2, Out),
	ordered_merger.
ordered_merger([]).

ordered_merge(In1, In2, Out) :-

    In1 ? I1, In2 = [I2 | _],
    I1 @< I2 :
      Out ! I1 |
	ordered_merge;

    In1 = [I1 | _], In2 ? I2,
    I2 @< I1 :
      Out ! I2 |
	ordered_merge;

    In1 = [I | _], In2 ? I |
	ordered_merge;

    In1 = [] :
      In2 = Out ;

    In2 = [] :
      In1 = Out .

close_merger(done, CH) :-
    true :
      close_channel(CH) .

/*
** Convert integer to a list of digits.
*/

procedure integer_to_dlist(Integer, [Integer], Any).

integer_to_dlist(I, List, Tail) :-
    integer(I),
    convert_to_string(I, String),
    string_to_dlist(String, List^, Tail) |
	true.

/*
** Make two tuples, one with read-only arguments and one with writable
** arguments, where corresponding arguments are the same variable.
*/

procedure make_safe_tuple(Integer, Tuple, Tuple).

make_safe_tuple(Arity, RoTuple, VarTuple) :-

    make_tuple(Arity, RT),
    make_tuple(Arity, VT) :
      RoTuple = RoTuple'?,
      VarTuple = VarTuple'? |
	fill_safe_tuple.

  fill_safe_tuple(Arity, RT, VT, RoTuple, VarTuple) :-

    Arity-- > 0,
    arg(Arity, RT, A?^),
    arg(Arity, VT, A^) |
	self;

    Arity =< 0 :
      RT = RoTuple,
      VT = VarTuple .


procedure freeze_term(Any, String, [{String, Any}]).

freeze_term(Term, FrozenTerm, Dictionary) :-
    true :
      Dictionary = Head?,
      FrozenTerm = Term'? |
	processor # link(execute(freeze_term,
				 {Term, Term', 1000000, 1000000,
				 2, _, Head, [], _, 1,
				 2, '_var', '_ro', '_1000000_', '' }
			 )
		    ).

procedure list_length(List, Integer).

list_length(List, Length) :-
	list_length(List, 0, Total),
	send(list_length, Total, Total, Length).

list_length(List, Counter, Length) :-

    List ? _,
    Counter' := Counter + 1 |
	list_length;

    otherwise : List = _,
      Counter = Length .


/*
** ground_list(List, Grounded)
**
** Ground each element of list List; add grounded element to list Grounded.
*/

procedure ground_list(List, List).

ground_list(List, Grounded) :-

    List ? Element,
    ground(Element) :
      Grounded ! Element |
	self;

    otherwise,
    ground(List) :
      List = Grounded.


procedure tuple_to_dlist(Tuple, List, Any).

tuple_to_dlist(Tuple, List, End) :-
    Arity := arity(Tuple) |
	tuple_to_dlist(0, Arity, Tuple, FinalList, {End, FinalList, List1}),
	send(tuple_to_dlist, List1, List1, List).

tuple_to_dlist(N, Arity, Tuple, List, Lists) :-

    N < Arity,
    N' := N + 1, arg(N', Tuple, Arg) :
      List ! Arg |
	tuple_to_dlist;

    N = Arity : Tuple = _,
      Lists = {List, FinalList, FinalList} .


procedure list_to_tuple(List, Tuple).

list_to_tuple(List,Tuple) :-
	list_length(List, 0, N),
	make_tuple(N, Tuple1),
	instantiate(List, Tuple1, 0, Tuple2),
	send(list_to_tuple, Tuple2, Tuple2, Tuple).

instantiate(List, Tuple1, N, Tuple2) :-

    List ? Arg,
    N' := N + 1,
    arg(N', Tuple1, Arg^) |
	instantiate;

    otherwise : List = _, N= _,			% usually List = []
      Tuple1 = Tuple2 .


convert_seconds(Seconds, Converted) :-

    Seconds < 0 :
      Converted = Seconds;

    Seconds >= 0 |
	convert_time(Seconds, "", Converted).

convert_milliseconds(MilliSeconds, Converted) :-

    MilliSeconds < 0 :
      Converted = MilliSeconds;

    MilliSeconds >= 0,
    Time := MilliSeconds/1000,
    mod(MilliSeconds, 1000, MilliPart),
    Fraction := MilliPart/1000.0,
    convert_to_string(Fraction, FractionPart),
    string_to_dlist(FractionPart, [_|FPL], []),
    list_to_string(FPL, Tail) |
	convert_time(Time, Tail, Converted).

convert_time(Time, Tail, Converted) + (Parts = [60, 60, 24]) :-

    Time < 0 :
      Parts = _,
      Tail = Converted;

    Time >= 0,
    Parts =?= [],
    convert_to_string(Time, String) |
	append_strings([String, Tail], Converted);

    Time >= 0,
    Parts ? Part,
    Time' := Time/Part,
    mod(Time, Part, Units),
    convert_to_string(Units, String),
    Time' > 0,
    Units >= 10 |
	append_strings([":", String, Tail], Tail'),
	self;

    Time >= 0,
    Parts ? Part,
    Time' := Time/Part,
    mod(Time, Part, Units),
    convert_to_string(Units, String),
    Time' > 0,
    Units < 10 |
	append_strings([":0", String, Tail], Tail'),
	self;

    Time >= 0,
    Parts ? Part,
    Time' := Time/Part,
    mod(Time, Part, Units),
    convert_to_string(Units, String),
    Time' =< 0 :
      Parts' = _ |
	append_strings([String, Tail], Converted).
