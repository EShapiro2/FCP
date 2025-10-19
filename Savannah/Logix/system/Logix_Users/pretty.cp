/*
** This module is part of EFCP.
**

     Copyright 2008 William Silverman
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

-export([module / 2, context / 3, module / 3, context / 4]).
-mode(interrupt).
-language([evaluate, compound, colon]).

EOL => "
".
FCPTORHS => " :-

".
SHORTORHS => " :-
".
ASPICTORHS => " ::=

".
FCPTOTELL => " :
".
FCPCOMMIT => " |
".
ASPICCOMMIT => ",
".
TONEXTCLAUSE => " ;

".
FCPOR => ",
".
ASPICOR => " |
".
ASPICTOSUB => "<<
".
ASPICFROMSUB => ">>".
SHORTSTOP => ".
".
STOP => ".

".
LONGSTOP => ".


".
CHAR_LF => 10.
CHAR_SPACE => 32.
CHAR_DOUBLE_QUOTE => 34.
CHAR_SINGLE_QUOTE => 39.
CHAR_LEFT_PAREN => 40.
MAXLINE => 79.

procedure context(Any, String, [String]).
procedure context(Any, String, [String], Mode).
procedure module((String; [Any]; Intermediate), [String]).
procedure module((String; [Any]; Intermediate), [String], Mode).
Intermediate ::= module(String, [Procedure]).
Procedure ::= procedure(String/Integer, [Clause]).
Clause ::= {Atom, {[Atom], [Atom]}, [Atom]}.
Atom ::= String; Tuple.
Mode ::= fcp; aspic.

/*
** This module transforms its source file or list into a list of (string)
** lines; each line is terminated by a line-feed (ascii(lf)).
**
** Input to module(Source, Output, Mode)
**
** Source:  A file name (not including the implied suffix, ".cp").
**
**          A list of parsed processes - the list may include attributes,
**          declarations, evaluation predicates (see language "evaluate"),
**          and arbitrary atoms.
**
**          An intermediate output list from the compiler (e.g. produced by
**          compile(FileName,intermediate(List)).
**
** Mode:    "fcp" (default) or "aspic".
**
** Input to context(Context, File, Output, Mode)
**
** Context: The directory within which the file is found - e.g.
**          computation#circadian.
**
** File:    The (string) name of the file to be transformed.
**
** Mode:    As above.
**
**
** Output:  From either: a list of lines, which may be concatenated into
**          a string - e.g. utils#append_strings(Lines, String); the String
**          may be saved as a source file, functionally equivalent to the
**          original - e.g. file#put_file("sourcefile.cp", String, [], _^).
**
** This file, not including the comments, was produced by a call to its
** executable form - pretty#module(pretty, Lines), and saved as above.
*/

module(Data, Strings) + (Mode = fcp) :-

  string(Data) |
      context(computation, Data, Strings, Mode) ;

  Data =?= module(Name, Procedures) |
      computation # display(term, module(Name)),
      intermediate_procedures(Procedures, Source),
      transform(Source, Strings, Mode) ;

  otherwise |
      transform(Data, Strings, Mode).


context(Context, Name, Strings) + (Mode = fcp) :-

  string(Name) |
      computation_utils # call_list(file # execute_in_context(Context,
          get_source(Name, Chars, Status, Source, Date)), Result),
      parse # characters(Chars?, Parsed, Errors),
      check_status,
      computation # display(stream, Display, [prefix(pretty(Name)),
          type(ground)]),
      transform(Parsed, Strings, Mode).


transform(Parsed, Strings, Mode) :-

      terms_to_string # TTS?,
      items_to_strings(Items?, TTS, Strings),
      unparse(Parsed, Items, Mode).


check_status(Result, Status, Errors, Source, Date, Display, Chars) :-

  Result =?= true,
  Status = found,
  Errors =?= [] :
    Chars = _,
    Display = [source(Source), date(Date)] ;

  Result =?= true,
  Status = found,
  Errors =\= [] :
    Chars = _,
    Display = [source(Source), date(Date) | Errors] ;

  Result = true,
  Status =\= found :
    Date = _,
    Errors = _,
    Source = _,
    Chars = [],
    Display = false(Status) ;

  Result =\= true :
    Date = _,
    Errors = _,
    Source = _,
    Status = _,
    Chars = [],
    Display = Result.


items_to_strings(Items, TTS, Strings) :-

  Items ? Indent(Term),
  Indent > MAXLINE / 2,
  Indent' := MAXLINE / 2 :
    Items'' = [Indent'(Term) | Items'] |
      self ;

  Items ? Indent(Term),
  otherwise :
    TTS ! acyclic_grounded_terms_to_string(Term, Indent, 10000000, String) |
      measure_string(Indent, String, Strings, Strings'),
      self ;

  Items =?= [] :
    TTS = [],
    Strings = [].


measure_string(Indent, String, Strings, NextStrings) :-

  string_length(String) =< MAXLINE :
    Indent = _,
    Strings = [String | NextStrings] ;

  otherwise,
  string_to_dlist(String, List, []) :
    Strings ! String' |
      indent_spaces(Indent, List, IndentSpaces, NextList),
      split_list(List, MAXLINE, List1, NextList),
      list_to_string(List1, String'),
      list_to_string(IndentSpaces, NextString),
      measure_string(Indent, NextString, Strings', NextStrings).


indent_spaces(Indent, List, IndentSpaces, NextList) :-

  Indent-- > 0,
  List ? Space,
  Space =?= CHAR_SPACE :
    IndentSpaces ! Space |
      self ;

  Indent =< 0 :
    List = _,
    IndentSpaces = [CHAR_SPACE, CHAR_SPACE, CHAR_SPACE | NextList].


split_list(List, MaxLength, List1, NextList) :-

      list_to_sub_strings(List, Strings, Begin, Begin),
      split_strings(Strings, MaxLength, S1, S2),
      strings_to_list(S1, List1),
      strings_to_list(S2, NextList).


list_to_sub_strings(List, Strings, Begin, Cs) :-

  List ? C,
  C =?= CHAR_SPACE :
    Cs ! C |
      self ;

  List =?= [C | _],
  C =\= CHAR_SPACE |
      complete_sub_string ;

  List =?= [] :
    Begin = _,
    Cs = _,
    Strings = [].


complete_sub_string(List, Strings, Begin, Cs) :-

  List ? C,
  C =\= CHAR_SINGLE_QUOTE,
  C =\= CHAR_DOUBLE_QUOTE :
    Cs ! C |
      complete_unquoted_string ;

  List ? Quote,
  Quote =?= CHAR_SINGLE_QUOTE :
    Cs ! Quote |
      complete_quoted_string ;

  List ? Quote,
  Quote =?= CHAR_DOUBLE_QUOTE :
    Cs ! Quote |
      complete_quoted_string.


complete_unquoted_string(List, Strings, Begin, Cs) :-

  List ? C,
  C =\= CHAR_SPACE,
  C =\= CHAR_DOUBLE_QUOTE,
  C =\= CHAR_LEFT_PAREN :
    Cs ! C |
      self ;

  List ? C,
  C =?= CHAR_LEFT_PAREN :
    Cs = [C],
    Strings ! SubString?,
    Begin' = Cs' |
      list_to_string(Begin, SubString),
      list_to_sub_strings ;

  otherwise :
    Cs = [],
    Strings ! SubString?,
    Begin' = Cs' |
      list_to_string(Begin, SubString),
      list_to_sub_strings.


complete_quoted_string(List, Strings, Begin, Cs, Quote) :-

  List ? C,
  C =\= Quote :
    Cs ! C |
      self ;

  List =?= [Quote, Quote | List'] :
    Cs = [Quote, Quote | Cs'] |
      self ;

  List =?= [Quote, CHAR_LEFT_PAREN | List'] :
    Cs = [Quote, CHAR_LEFT_PAREN],
    Strings ! SubString?,
    Begin' = Cs' |
      list_to_string(Begin, SubString),
      list_to_sub_strings ;

  List ? Quote,
  otherwise :
    Cs = [Quote],
    Strings ! SubString?,
    Begin' = Cs' |
      list_to_string(Begin, SubString),
      list_to_sub_strings ;

  otherwise :
    Cs = [Quote],
    Strings ! SubString?,
    Begin' = Cs' |
      list_to_string(Begin, SubString),
      list_to_sub_strings.


split_strings(Strings, MaxLength, S1, S2) :-

  Strings ? S,
  S =?= CHAR_LF :
    MaxLength = _,
    S1 ! S,
    MaxLength' = 0 |
      self ;

  Strings ? S,
  S =\= CHAR_LF,
  string_length(S, Length),
  Length =< MaxLength,
  MaxLength' := MaxLength - Length :
    S1 ! S |
      self ;

  Strings ? S,
  S =\= CHAR_LF,
  string_length(S, Length),
  Length > MaxLength,
  MaxLength =?= MAXLINE :
    S1 = [S, EOL],
    S2 = Strings' ;

  otherwise :
    MaxLength = _,
    S1 = [EOL],
    S2 = Strings.


strings_to_list(Strings, List) :-

  Strings ? S,
  string_to_dlist(S, L, List') :
    L = List |
      self ;

  Strings =?= [] :
    List = [].


intermediate_procedures(Procedures, Source) :-

  Procedures ? "procedure"(_Ident, Clauses) |
      intermediate_clauses(Clauses, Source, Source'),
      self ;

  Procedures = [] :
    Source = [].


intermediate_clauses(Clauses, Source1, Source2) :-

  Clauses ? Clause :
    Source1 ! Clause' |
      intermediate_clause(Clause, Clause'),
      self ;

  Clauses = [] :
    Source1 = Source2.


intermediate_clause(Clause1, Clause2) :-

  Clause1 = Head({[], []}, Body),
  list(Body) :
    Clause2 = (Head :- Body') |
      intermediate_list(Body, Body') ;

  Clause1 = Head(Ask([]), Body),
  list(Ask) :
    Clause2 = (Head :- Ask' | Body') |
      intermediate_list(Ask, Ask'),
      intermediate_list(Body, Body') ;

  Clause1 = Head(Ask(Tell), Body),
  list(Tell) :
    Clause2 = (Head :- Ask' : Tell' | Body') |
      intermediate_list(Ask, Ask'),
      intermediate_list(Tell, Tell'),
      intermediate_list(Body, Body') ;

  Clause1 = Head({[], []}, []) :
    Clause2 = Head.


intermediate_list(List, Conjunction) :-

  List ? Predicate,
  List' =\= [] :
    Conjunction = (Predicate, Conjunction') |
      self ;

  List = [Predicate] :
    Conjunction = Predicate ;

  List = [] :
    Conjunction = true.


unparse(Parsed, Items, Mode) :-

      utils # unparse(Parsed?, Unparsed),
      unparsed_to_strings(0, Unparsed, Items, Mode, attribute,
          _PreviousStop).


unparsed_to_strings(Indent, Unparsed, Items, Mode, PreviousId, PreviousStop) :-

  Unparsed ? (LHS :- Clauses) :
    Items ! Indent([LHS, FcpToRhs]) |
      previous(LHS, PreviousId, PreviousStop, PreviousId'),
      test_compound,
      flatten_clauses(Indent, Clauses, Items', fcp, PreviousStop', Items''),
      self ;

  Unparsed ? (LHS ::= Clauses),
  Mode =?= aspic :
    Items ! Indent([LHS, ASPICTORHS]) |
      previous(LHS, PreviousId, PreviousStop, PreviousId'),
      flatten_clauses(Indent, Clauses, Items', aspic, PreviousStop', Items''),
      self ;

  Unparsed ? TypeDeclaration,
  TypeDeclaration =?= (_LHS ::= _Clauses),
  Mode =\= aspic :
    PreviousId' = type_declaration,
    Items ! Indent([TypeDeclaration, PreviousStop']) |
      previous_stop(PreviousId', PreviousId, PreviousStop),
      self ;

  Unparsed ? Attribute,
  arg(1, Attribute, "-") :
    PreviousId' = attribute,
    Items ! Indent([Attribute, PreviousStop']) |
      previous_stop(PreviousId', PreviousId, PreviousStop),
      adjust_mode(Attribute, Mode, Mode'),
      self ;

  Unparsed ? Value,
  Value = (_ => _) :
    PreviousId' = value,
    Items ! Indent([Value, PreviousStop']) |
      previous_stop(PreviousId', PreviousId, PreviousStop),
      self ;

  Unparsed ? Declaration,
  Declaration =?= procedure _ :
    PreviousId' = declaration,
    Items ! Indent([Declaration, PreviousStop']) |
      previous_stop(PreviousId', PreviousId, PreviousStop),
      self ;

  Unparsed ? Fact,
  Mode =?= fcp,
  tuple(Fact),
  arg(1, Fact, Name),
  Name =\= "procedure",
  nth_char(1, Name, C),
  ascii(a) =< C,
  C =< ascii(z) :
    Items ! Indent([Fact, PreviousStop']) | 
      previous(Fact, PreviousId, PreviousStop, PreviousId'),
      self ;

  Unparsed ? Atom,
  otherwise :
    PreviousId' = atom,
    Items ! Indent([Atom, PreviousStop']) |
      previous_stop(PreviousId', PreviousId, PreviousStop),
      self ;

  Unparsed =?= [] :
    Indent = _,
    Mode = _,
    PreviousId = _,
    PreviousStop = SHORTSTOP,
    Items = [].


test_compound(Clauses, FcpToRhs) :-

  Clauses =?= (_; _) :
    FcpToRhs = FCPTORHS;

  Clauses =\= (_; _) :
    FcpToRhs = SHORTORHS.


previous(LHS, PreviousId, PreviousStop, CurrentId) :-

  LHS =\= _ + _,
  tuple(LHS),
  arity(LHS, N),
  N--,
  arg(1, LHS, Functor) :
    CurrentId = Functor / N' |
      previous_stop ;

  otherwise :
    CurrentId = LHS / 0 |
      previous_stop.


previous_stop(CurrentId, PreviousId, PreviousStop) :-

  CurrentId =?= PreviousId :
    PreviousStop = SHORTSTOP ;

  CurrentId =\= PreviousId,
  CurrentId =?= _ / _N :
    PreviousStop = LONGSTOP ;

  otherwise :
    CurrentId = _,
    PreviousId = _,
    PreviousStop = STOP.

adjust_mode(Attribute, Mode, NewMode) :-

  Attribute =?= -language(Ls),
  Ls =?= [spifcp | _] :
    Mode = _,
    NewMode = aspic ;

  Attribute =?= -language(Ls),
  Ls =?= [biospi | _] :
    Mode = _,
    NewMode = aspic ;

  Attribute =?= -language(Ls),
  Ls ? Other,
  Other =\= spifcp,
  Other =\= biospi :
    Attribute' = -language(Ls') |
      self ;

  Attribute =?= -language(L),
  L =\= [_ | _],
  L =\= [] :
    Attribute' = -language([L]) |
      self ;

  otherwise :
    Attribute = _,
    Mode = NewMode.


flatten_clauses(Indent, Clauses, Items, Language, Tail, NextItems) :-

  Clauses =?= (Clauses1; Clauses2) |
      flatten_clauses(Indent, Clauses1, Items, Language, TONEXTCLAUSE,
          Items'),
      flatten_clauses(Indent, Clauses2, Items', Language, Tail, NextItems) ;

  Clauses =\= (_; _),
  Language =?= fcp |
      flatten_fcp_clause + (Clause = Clauses) ;

  Clauses =\= (_; _),
  Language =?= aspic |
      parse_aspic_clause + (Clause = Clauses),
      flatten_aspic_clause.


flatten_fcp_clause(Indent, Clause, Items, Tail, NextItems) :-

  Clause =?= (Guard | Body),
  BodyIndent := Indent + 6 |
      flatten_fcp_guard(Indent, Guard, Items, FCPCOMMIT, Items'),
      flatten_fcp_predicates(BodyIndent, Body, Items', Tail, NextItems) ;

  Clause =?= (_ : _) |
      flatten_fcp_guard + (Guard = Clause) ;

  otherwise,
  Indent += 6 |
      flatten_fcp_predicates + (Term = Clause).


flatten_fcp_guard(Indent, Guard, Items, Tail, NextItems) :-

  Guard =?= (Ask : Tell),
  AskIndent := Indent + 2,
  TellIndent := AskIndent + 2 |
      flatten_fcp_predicates(AskIndent, Ask, Items, FCPTOTELL, Items'),
      flatten_fcp_predicates(TellIndent, Tell, Items', Tail, NextItems) ;

  Guard =\= (_ : _),
  Indent += 2 |
      flatten_fcp_predicates + (Term = Guard).


flatten_fcp_predicates(Indent, Term, Items, Tail, NextItems) :-

  Term =?= (LeftTerm, Term'),
  LeftTerm =?= (_, _) |
      flatten_fcp_predicates(Indent, LeftTerm, Items, FCPOR, Items'),
      flatten_fcp_predicates ;

  Term =?= (LeftTerm, Term'),
  LeftTerm =\= (_, _) :
    Items ! Indent([LeftTerm, FCPOR]) |
      flatten_fcp_predicates ;

  otherwise :
    Items = [Indent([Term, Tail]) | NextItems].


parse_aspic_clause(Clause, Guards, Body) :-

  Clause =?= (Mixed | Body'),
  Mixed =?= (_, _) :
    Body = (Goal? | Body') |
      extract_goal(Mixed, Guards, Goal) ;

  Clause =?= (_, _) |
      extract_goal(Clause, Guards, Body) ;

  otherwise :
    Guards = [],
    Body = Clause.


extract_goal(Mixed, Guards, Goal) :-

  Mixed =?= (Guard, Mixed'),
  Mixed' =?= (_, _) :
    Guards = (Guard, Guards') |
      self ;

  Mixed =?= (Guard, Predicate),
  Predicate =\= (_, _) :
    Guards = Guard,
    Goal = Predicate.


flatten_aspic_clause(Indent, Guards, Body, Items, Tail, NextItems) :-

  Guards =\= [],
  Indent += 2,
  Indent' += 2 :
    Items ! Indent'([Guards, ASPICCOMMIT]) |
      flatten_aspic_body ;

  Guards =?= [],
  Indent += 4 |
      flatten_aspic_body.


flatten_aspic_body(Indent, Body, Items, Tail, NextItems) :-

  Body =?= (Body0 | Body3),
  Body0 =?= (Body1 | Body2) |
      flatten_aspic_body + (Body = (Body1, Body2, Body3)) ;

  Body =?= (Goal | Body'),
  Goal =\= (_ | _) |
      flatten_aspic_goal(Indent, Goal, Items, ASPICOR, Items'),
      self ;

  otherwise |
      flatten_aspic_goal + (Goal = Body).


flatten_aspic_goal(Indent, Goal, Items, Tail, NextItems) :-

  Goal =?= [_ | _],
  Indent += 2 :
    Items ! Indent(ASPICTOSUB),
    NextItems' = [Indent([ASPICFROMSUB, Tail]) | NextItems] |
      flatten_clauses(Indent, Clauses?, ClauseItems, aspic, Stop?,
          NestedItems),
      flatten_nest + (Nest = Goal) ;

  Goal =?= Name(Nest),
  Nest =?= [_ | _],
  string(Name),
  Indent += 2 :
    Items ! Indent([Name, "(", ASPICTOSUB]),
    NextItems' = [Indent([ASPICFROMSUB, ")", Tail]) | NextItems] |
      flatten_clauses(Indent, Clauses?, ClauseItems, aspic, Stop?,
          NestedItems),
      flatten_nest ;

  otherwise :
    Items = [Indent([Goal, Tail]) | NextItems].


flatten_nest(Indent, Nest, Items, ClauseItems, Clauses, Stop, NestedItems,
    NextItems) :-

  Nest =?= [Nested] :
    Indent = _,
    Items = ClauseItems,
    Clauses = Nested,
    Stop = EOL,
    NestedItems = NextItems ;

  Nest =?= [Arguments, NestedClauses],
  NestedClauses =\= (_ ::= _) :
    Clauses = NestedClauses,
    Items = [Indent([Arguments, SHORTSTOP]) | ClauseItems],
    Stop = EOL,
    NestedItems = NextItems ;

  Nest =?= [NestedClauses | Procedures],
  Procedures =?= [Procedure | _More],
  Procedure =?= (_ ::= _) :
    Clauses = NestedClauses,
    Stop = STOP,
    Items = ClauseItems |
      flatten_nested_procedures + (ClausesSuffix = _) ;

  Nest =?= [Arguments, NestedClauses | Procedures],
  NestedClauses =\= (_ ::= _),
  Procedures =\= [] :
    Clauses = NestedClauses,
    Items = [Indent([Arguments, SHORTSTOP]) | ClauseItems],
    Stop = STOP |
      flatten_nested_procedures + (ClausesSuffix = _).


flatten_nested_procedures(Indent, Procedures, NestedItems, ClausesSuffix,
    NextItems) :-

  Procedures ? Procedure,
  Procedure =?= (LHS ::= Clauses) :
    NestedItems ! Indent([LHS, ASPICTORHS]),
    ClausesSuffix = STOP |
      flatten_clauses(Indent, Clauses, NestedItems', aspic, ClausesSuffix',
          NestedItems''),
      self ;

  Procedures ? Other,
  Other =\= (_ ::= _) :
    NestedItems ! Indent([Other, ClausesSuffix']),
    ClausesSuffix = STOP |
      self ;

  Procedures =?= [] :
    Indent = _,
    NestedItems = NextItems,
    ClausesSuffix = EOL ;

  Procedures =\= [_ | _],
  Procedures =\= [] :
    NestedItems = [Indent([Procedures, EOL]) | NextItems],
    ClausesSuffix = ASPICOR.
