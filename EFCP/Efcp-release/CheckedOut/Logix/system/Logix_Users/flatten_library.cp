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

-language([compound,colon]).
-mode(interrupt).

Tree ::= lib(Any, Code, Any, Tree, Tree) ; [].
TreeList ::= [procedure(Any, Any, Any)].
Code ::= [{Any, {List, List}, List}].
List ::= [Any].

procedure tree(Tree, TreeList).
procedure tree(Tree, TreeList, TreeList).

tree(Lib, Left) + (Right = []) :-

    Lib = lib(Proc, Code, Xref, Lib', R):
      Right'' = [procedure(Proc, Code, Xref) | Right'] |
	tree,
	tree(R, Right', Right);

    Lib = [] :
      Left = Right |
	true.


procedure procs(Tree, List).
procedure procs(Tree, List, List).

procs(Lib, Left) + (Right = []) :-

    Lib = lib(Proc, _, _, Lib', R) :
      Right'' = [Proc | Right'] |
	procs,
	procs(R, Right', Right);

    Lib = [] :
      Left = Right |
	true.


XrefList ::= [procedure(Any, Any)].

procedure xref(Tree, XrefList).
procedure xref(Tree, XrefList, XrefList).

xref(Lib, Left) + (Right = []) :-

    Lib = lib(Proc, _, Xref, Lib', R) :
      Right'' = [procedure(Proc, Xref) | Right'] |
	xref,
	xref(R, Right', Right);

    Lib = [] :
      Left = Right |
	true.


procedure code(Tree, List).
procedure code(Tree, List, List).

code(Lib, Left) + (Right = []) :-

    Lib = lib(_, Code, _, L, Lib') |
	code(L, Left, Left'),
	clauses(Code, Left', Left''),
	code;

    Lib = [] :
      Left = Right |
	true.

procedure clauses(Code, List, List).

clauses(Code, Left, Right) :-

    Code ? {Head, {Ask, Tell}, Body} :
      Left ! Clause |
	clauses,
	decode(Head, Ask, Tell, Body, Clause);

    Code = [] :
      Left = Right |
	true.

procedure decode(Any, List, List, List, Any).

decode(Head, [], [], [], Head^).
decode(Head, Ask, Tell, Body, Clause) :-

    Ask = [],
    Tell = [],
    list(Body) :
      Clause = (Head :- Body') |
	comma_list(Body, Body');

    list(Ask),
    Tell = [] :
      Clause = (Head :- Guard | Body') |
	comma_list(Ask, Guard),
	comma_list(Body, Body');

    otherwise :
      Clause = (Head :- Ask' : Tell' | Body') |
	comma_list(Ask, Ask'),
	comma_list(Tell,Tell'),
	comma_list(Body, Body').

Commaed ::= Any ; (Any, Commaed).

procedure comma_list(List, Commaed).

comma_list([], true^).
comma_list([End], End^).
comma_list(List, CL) :-
    List ? El,
    list(List') :
      CL = (El, CL') |
	comma_list.
