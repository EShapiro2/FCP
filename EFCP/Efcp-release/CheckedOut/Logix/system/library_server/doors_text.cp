/*
** This module is part of EFCP.
**

     Copyright 2007 Marilyn Safran
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
       Doors library
       1992
*/

-export([string/1]).
-mode(trust).
-language(fcp).

procedure string(String).

string(Text) :- true : Text =

"library.

-language(compound).

copy(X1,X2,X3) :-	% To be replaced by listeners when available.
    ground(X1), var(X2), var(X3) : X2=X1, X3=X1.

ok(Result, Comment) :-
    Result = [] :
	Comment = _;
    Result = true :
	Comment = _;
    list(Result) |
	    and(Result,Ok), ok(Ok?,Comment);
    Result = false(Reason) |
	    self # service_id(Path),
	    extract_module_name_from_path(Path?, ModuleName, Ready),
	    computation # display(term, (""Doors failure"" :
					 ModuleName?(Comment), Reason),
				  known(Ready?));
    Result = false :
	Result' = false(false_result) |
	    self;
    invalid(Result) |
	    self # service_id(Path),
	    extract_module_name_from_path(Path?, ModuleName, Ready),
	    computation # display(term, (""Doors failure"" :
			      ModuleName?(Comment), ""INVALID ok variable""),
				  known(Ready?)).

ok(Result) :-
    Result = true | true ;
    Result = [] | true ;
    list(Result) |
	    and(Result,Ok), ok(Ok?);
    Result = false(Reason) |
	    self # service_id(Path),
	    extract_module_name_from_path(Path?, ModuleName, Ready),
	    computation # display(term, (""Doors failure"" :
					 ModuleName?(Reason)),
				  known(Ready?));
    Result = false :
	Result' = false(false_result) |
	    self;
    invalid(Result) |
	    self # service_id(Path),
	    extract_module_name_from_path(Path?, ModuleName, Ready),
	    computation # display(term, (""Doors failure"" :
				ModuleName?(""INVALID ok variable."")),
				  known(Ready?)).

extract_module_name_from_path(Path, ModuleName, Ready) :-
    Path ? Name :
      ModuleName = Name,
      Ready = true.

and(Ok1,Ok2,Ok) :-
    Ok1 =\= true, var(Ok) : Ok2 = _, Ok = Ok1;
    Ok2 =\= true, var(Ok) : Ok1 = _, Ok = Ok2;
    Ok1 = true, Ok2 = true, var(Ok) : Ok = true.

and(Oks,Ok) :-
    Oks = [], var(Ok) : Ok = true;
    Oks = [Ok1], var(Ok) : Ok = Ok1;
    Oks = [_,_|_] |
	    and1(Oks,Oks1),
	    and(Oks1?,Ok).

and1(Oks,Oks1) :-
    Oks ? Ok1, Oks' ? Ok2 :
	Oks1 = [ Ok? | Oks1'?] |
	    and(Ok1, Ok2, Ok), self;
    Oks = [_], var(Oks1) : Oks1 = Oks;
    Oks = [], var(Oks1) : Oks1 = Oks.


sequential_and(Oks, Ok) :-
    Oks = [] | Ok = true;
    Oks = [Ok1] | 
	sequential_and1(Ok1,[],Ok);
    Oks = [Ok1, Ok2 | Oks'] |
	sequential_and1(Ok1, [Ok2 | Oks'], Ok).

sequential_and1(Ok, Oks, Ok1) :-

    Ok = true |
	    sequential_and(Oks, Ok1);
    Ok = false(Reason), var(Ok1) :
	_ = Oks?,
	Ok1 = false(Reason);
    Ok = false :
	_ = Oks?,
	Ok' = false(false_result) |
	    self;
    invalid(Ok), var(Ok1) :
	_ = Oks?,
	Ok1 = false(invalid_message(Ok)).

append(A, B, C) :-
    A ? X, var(C) : C = [X | C'?] | self;
    A = [], var(C) : C = B.


"

	| true.
