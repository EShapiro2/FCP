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
      String Processes
      2011
*/

-export([string/1]).
-mode(trust).

string(Text) :- true : Text =

"library.

-language([evaluate, compound, colon]).
-include(system_constants).

MAXCHAR => 65535.	% See fch.h/kernels.c

/*
** stripbrk(S1, S2, Iv, Sv)
**
** Returns the index to the first instance in S1 of any character in S2, and a
** substring of S1, starting with that first instance.
** Returns 0 and an empty string if no character from S2 is encountered in S1.
** In either case the integer result is returned in variable Iv, and the string
** is returned in Sv.
*/ 

stripbrk(S1, S2, Iv, Sv) + (FName = stripbrk) :-

    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    we(Iv),
    we(Sv) :
      FName = _,
      Iv = Iv'?,
      Sv = Sv'? |
	'_str_scan'(S1', S2', token, Si),
	'_str_ipV'(S1', Si?, 1, Iv', Sv');

    otherwise |
	'_str_parameter_test'(S1, Iv, FName),
	'_str_parameter_test'(S2, Sv, FName).


/*
** stribrk(S1, S2, Iv)
**
** Returns the index to the first instance in S1 of any character in S2.
** Returns 0 if no character from S2 is encountered in S1.
** In either case the integer result is returned in variable Iv.
*/ 

stribrk(S1, S2, Iv) :-
	stripbrk(S1, S2, Iv, _Sv, stribrk).

/*
** strpbrk(S1, S2, Sv)
**
** Returns the substring starting at the first instance in S1 of any character
** in S2.
** Returns an empty string if no characters from S2 are encountered in S1.
** In either case the result is returned in variable Sv.
*/ 

strpbrk(S1, S2, Sv) :-
	stripbrk(S1, S2, _Iv, Sv, strpbrk).

/*
** strcat(S1, S2, Sv)
**
** appends the string S2 to the string S1.  The value of the concatenated
** strings is returned in variable Sv.
*/

strcat(S1, S2, Sv) + (FName = strcat) :-

    convert_to_string(S2, S2'),
    convert_to_string(S1, S1'),
    we(Sv) :
      Sv = Sv'? |
	'_str_cat_to_str';

    otherwise |
	'_str_parameter_test'(S1, S2, Sv, FName).

'_str_cat_to_str'(S1, S2, Sv, FName) :-

    string_length(S1) + string_length(S2) < MAXCHAR,
    string_to_dlist(S2, L2, []),
    string_to_dlist(S1, L1, L2),
    list_to_string(L1, S) :
      FName = _,
      Sv = S;

    otherwise :
      S1 = _,
      S2 = _,
      Sv = _ |
	fail(FName, 'S1+S2'(too_long)).

/*
** strncat(S1, S2, N, Sv)
**
** appends at most the first N characters of the string S2 to the string S1.
** The value of the concatenated strings is returned in variable Sv.
*/

strncat(S1, S2, N, Sv) :-

    convert_to_string(S2, S2'),
    convert_to_string(S1, S1'),
    convert_to_integer(N, N'),
    we(Sv)  :
      Sv = Sv'? |
	'_strncat_copy';

    otherwise |
	'_str_parameter_test'(S1, S2, N, Sv, strncat).

  '_strncat_copy'(S1, S2, N, Sv) :-

    string_length(S2) =< N |
	strcat + (FName = strncat);

    otherwise |	
	'_str_copyN'(S2, N, [], S2'),
	strcat + (FName = strncat).


/*
** strchr(S, C, Sv)
**
** Returns the sub-string of S, starting at the first instance of character C.
** Returns an empty string if C is not encountered in S.
** In either case the result is returned in variable Sv.
*/

strchr(S, C, Sv) :-

    string_length(C) =:= 1,
    nth_char(1, C, C') |
	self;

    otherwise,
    convert_to_string(S, S'),
    convert_to_integer(C, C'),
    -256 < C', C' < 256,
    string_to_dlist(S', L, []),
    we(Sv) :
      Sv = Sv'? |
	'_str_findchr'(C', L, L'),
	list_to_string(L', Sv');

    otherwise |
        '_str_not_character'(C, strchr),
	'_str_parameter_test'(S, Sv, strchr).


/*
** strrchr(S, C, Sv)
** Returns the sub-string of S, starting at  the last instance of character C.
** Returns an empty string if C is not encountered in S.
** In either case the result is returned in variable Sv.
*/

strrchr(S, C, Sv) :-

    string_length(C) =:= 1,
    nth_char(1, C, C') |
	self;

    otherwise,
    convert_to_string(S, S'),
    convert_to_integer(C, C'),
    -256 < C', C' < 256,
    string_to_dlist(S', L, []),
    we(Sv) :
      Sv = Sv'? |
	'_str_findchr'(C', L, L'),
	'_str_lastchr'(EMPTY_STRING, C', L', Sv');

    otherwise |
        '_str_not_character'(C, strrchr),
	'_str_parameter_test'(S, Sv, strrchr).

  '_str_lastchr'(Previous, C, L, Sv) :-

    L ? C :
      Previous = _,
      Previous' = L |
	'_str_findchr'(C, L', L''),
	self;

    otherwise :
      C = _,
      L = _,
      Previous = L' |
	list_to_string(L', Sv).


/*
** strcmp(S1, S2, Iv)
**
** compare the string S1 to the string S2.
** The result is returned in Iv:
**    Iv = 0 if they are the same
**    Iv < 0 if S1 is less than S2
**    Iv > 0 if S1 is greater than S2
*/

strcmp(S1, S2, Iv) :-
	strncmp(S1, S2, MAX_STRING_LENGTH, Iv, strcmp).

/*
** strncmp(S1, S2, N, Iv)
**
** compare up to N characters in the string S1 to the string S2.
** The result is returned in Iv:
**    Iv = 0 if they are the same
**    Iv < 0 if S1 is less than S2
**    Iv > 0 if S1 is greater than S2
*/

strncmp(S1, S2, N, Iv) + (FName = strncmp) :-

    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    convert_to_integer(N, N'),
    we(Iv),
    string_to_dlist(S1', L1, []),
    string_to_dlist(S2', L2, []) :
      FName =  _,
      Iv = Iv'? |
	'_str_compare_lists';

   otherwise |
	'_str_parameter_test'(S1, S2, N, Iv, FName).

  '_str_compare_lists'(N, L1, L2, Iv) :-

    N > 0,
    L1 ? C1,
    L2 ? C2,
    D := C1 - C2,
    D =\= 0 :
      L1' = _,
      L2' = _,
      Iv = D;

    N-- > 0,
    L1 ? C1,
    L2 ? C2,
    C2 - C1 =:= 0 |
	self;

    N > 0,
    L1 ? C1,
    L2 = [],
    D1 := C1 :
      L1' = _,
      Iv = D1;

    N > 0,
    L1 = [],
    L2 ? C2,
    D2 := - C2 :
      L2' = _,
      Iv = D2;

    otherwise : /* N =< 0 or L1 = L2 */
      L1 = _,
      L2 = _,
      N = _,
      Iv = 0.

/*
** strcpy(S1, S2, Sv)
**
** copies the string S2 to variable S1.  The value of S1 is returned in
** variable Sv.
*/

strcpy(S1, S2, Sv) :-

    convert_to_string(S2, S2'),
    string_to_dlist(S2', L1, []),
    we(S1), we(Sv)  :
      S1 = S?,
      Sv = S? |
	list_to_string(L1, S);

    otherwise |
        '_str_not_string'(S2, strcpy),
	'_str_not_assignable'(S1, strcpy),
	'_str_not_assignable'(Sv, strcpy).


/*
** strncpy(S1, S2, N, Sv)
**
** copies at most N characters of the string S2 to variable S1.
** The value of the copied substring is returned in variable Sv.
*/

strncpy(S1, S2, N, Sv) :-

    convert_to_string(S2, S2'),
    string_length(S2') =< N |
	strcpy;

    convert_to_string(S2, S2'),
    convert_to_integer(N, N'),
    string_length(S2') > N',
    we(S1), we(Sv)  :
      Sv = Sv'?,
      S1 = Sv'? |
	'_str_copyN'(S2', N', [], Sv');

    otherwise |
	'_str_parameter_test'(S1, S2, N, Sv, strncpy).


/*
** strspn(S1, S2, Iv)
**
** Returns the length of the longest substring of S1 that begins at the start
** of S1 and consists only of the characters found in string S2. 
** The integer result is returned in variable Iv.
*/

strspn(S1, S2, Iv) :-

    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    we(Iv) :
      Iv= Iv'? |
	'_str_scan'(S1', S2', separator, Si),
	Iv' := string_length(S1') - string_length(Si);

    otherwise |
	'_str_parameter_test'(S1, S2, Iv, strspn).


/*
** strcspn(S1, S2, Iv)
**
** Returns the length of the longest substring of S1 that begins at the start
** of S1 and consists only of the characters not found in string S2.
** The integer result is returned in variable Iv.
*/

strcspn(S1 ,S2, Iv) :-


    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    we(Iv) :
      Iv= Iv'? |
	'_str_scan'(S1', S2', token, Si),
	Iv' := string_length(S1') - string_length(Si);

    otherwise |
	'_str_parameter_test'(S1, S2, Iv, strcspn).


/*
** stristr(S1, S2, Sv)
**
** Returns the index to S1 of the first substring of S1 which matches S2.
** Returns 0 if S2 does not match any substring of S1.
*/

stristr(S1, S2, Iv) :-

    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    we(Iv),
    N := string_length(S1) - string_length(S2),
    string_to_dlist(S1', L1, []),
    string_to_dlist(S2', L2, _) :
      Iv = Iv'? |
	'_str_match_substring'(L1, L2, N, Si),
	'_str_ipV'(S1, Si, 1, Iv', _Sv);

    otherwise |
	'_str_parameter_test'(S1, S2, Iv, stristr).

/*
** strpstr(S1, S2, Sv)
**
** Returns the first substring of S1, whose front end matches string S2,
** followed by any trailing characters.
** Returns an empty string if S2 does not match any substring of S1.
*/

strpstr(S1, S2, Sv) :-

    convert_to_string(S1, S1'),
    convert_to_string(S2, S2'),
    we(Sv),
    N := string_length(S1) - string_length(S2),
    string_to_dlist(S1', L1, []),
    string_to_dlist(S2', L2, _) :
      Sv = Sv'? |
	'_str_match_substring'(L1, L2, N, Sv');

    otherwise |
	'_str_parameter_test'(S1, S2, Sv, strpstr).


/*
** strlen(S, Iv)
**
** Returns the length of string S in variable Iv.
*/

strlen(S, Iv) :-

    convert_to_string(S, S'),
    we(Iv),
    string_length(S', I) :
      I = Iv;

    otherwise |
	'_str_parameter_test'(S, Iv, strlen).


/*
** strtok(S1, S2, Sv)
**
** Succesive calls to this function extract successive 'tokens' from string S1;
** each token is separated from its successor (if any) by one or more
** separators; each character in S2 is a separator (The string S1 may end 
** without a separator).
**
** The initial call has the form:
**
**    strtok(S1, CHv)
**
** where CHv is a variable in which the channel is returned.
**
** Subsequent calls use the channel as the first argument. 
**
** Second and subsequent calls have the forms:
**
**    strtok(CH, S2, Tv)
**    strtok(CH, S2, Tv, Sv)
**    strtok({CH, CH'}, S2, Tv)
**    strtok({CH, CH'}, S2, Tv, Sv)
**
** The current token is returned to variable Tv; an empty string is returned
** if there are no more tokens.
** The current separator is returned to variable Sv; an empty string is
** returned when there are no more tokens or when the last token in S1 is not
** followed by a separator.
*/

strtok(S1, CHv) :-

    convert_to_string(S1, S1'),
    we(CHv),
    string_to_dlist(S1', L1, []) :
      make_channel(CHv, Input) |
	'_strtok_monitor'(Input, L1, CHv);

    otherwise |
	'_str_parameter_test'(S1, CHv, strtok(initialize)).

strtok(CH, S2, Tv) :-
	strtok(CH, S2, _Sv, Tv).

strtok(CHt, S2, Sv, Tv) :-

    channel(CHt) |
	strtok({CHt, _CH}, S2, Sv, Tv);	

    CHt =?= {CH, CHv},
    channel(CH), we(CHv),
    convert_to_string(S2, S2'),
    we(Sv), we(Tv) :
      CHv = CHv'?, Sv = Sv'?, Tv = Tv'? |	
	'_strtok1';

    otherwise |
	'_str_not_channel_tuple'(CHt, strtok),
	'_str_not_assignable'(Sv, strtok),
	'_str_parameter_test'(S2, Tv, strtok).


  '_strtok1'(CH, CHv, S2, Sv, Tv) :-

    true:
      write_channel(token(S2, Sv, Tv), CH, CHv);

    otherwise :
      S2 = _,
      CH = CHv,
      Sv = EMPTY_STRING,
      Tv = EMPTY_STRING.


'_strtok_monitor'(Input, L1, CH) :-

    Input ? token(S2, Sv, Tv),
    string_to_dlist(S2, L2, []) |
	'_str_scan_item'(L1, L2, separator, _, L1'),
	'_str_scan_item'(L1'?, L2, token, LT, L1''),
	list_to_string(LT?, Tv),
	'_str_scan_item'(L1''?, L2, separator, LS, L1'''),
	list_to_string(LS?, Sv),
	self;

    Input ? Other,
    otherwise |
	fail(strtok, unrecognised(Other)),
	self;

    Input = [] :
      L1 = [],
      close_channel(CH);

    L1 = [],
    unknown(Input) :
      close_channel(CH).
      
/************************** Auxilliary Processes *****************************/

'_str_copyN'(S2, N, L, Sv) :-

    N-- > 0,
    nth_char(N, S2, C) :
      L' = [C | L] |
	self;

    otherwise,
    list_to_string(L, S) :
      N = _,
      S2 = _,
      S = Sv;

    otherwise,
    L = [] :
      N = _,
      S2 = _,
      EMPTY_STRING = Sv.
    
'_str_findchr'(C, L, LC) :-

    L ? NotC, C =\= NotC |
	self;

    otherwise :
      C = _,
      L = LC.

'_str_parameter_test'(S1, V, FName) :-
        '_str_not_string'(S1, FName),
	'_str_not_assignable'(V, FName).

'_str_parameter_test'(S1, S2, V, FName) :-
        '_str_not_string'(S2, FName),
	'_str_not_string'(S1, FName),
	'_str_not_assignable'(V, FName).

'_str_parameter_test'(S1, S2, I, V, FName) :-
        '_str_not_string'(S2, FName),
	'_str_not_string'(S1, FName),
        not_integer(I, FName),
	'_str_not_assignable'(V, FName).

'_str_match_substring'(L1, L2, N, Sv) :-

    N < 0 :
      L1 = _,
      L2 = _,
      Sv = EMPTY_STRING;

    N >= 0 :
      L1 = L2 |
	list_to_string(L1, Sv);

    N-- >= 0,
    otherwise,
    L1 ? _ |
	self.

'_str_not_assignable'(Sv, F) :-

    we(Sv) :
      F = _;

    otherwise |
	fail(F, not_assignable(Sv)).

'_str_not_channel_tuple'(CHt, F) :-

    CHt =?= {CH, CHv},
    channel(CH),
    we(CHv) :
      F = _,
      CH = CHv;

    otherwise |
	fail(F, not_channel_tuple(CHt)).


'_str_not_character'(C, F) :-

    convert_to_string(C, _S) :
      F = _;

    otherwise |
	fail(F, not_a_character(C)).

    
not_integer(N, F) :-

    convert_to_integer(N, _S) :
      F = _;

    otherwise |
	fail(F, not_an_integer(N)).


'_str_not_string'(S, F) :-

    convert_to_string(S, _S) :
      F = _;

    otherwise |
	fail(F, not_a_string(S)).


'_str_scan_item'(L1, L2, Match, Lv, NewL1) :-

    L1 =?= [C | _] |
	'_str_scan_C_in_L2'(C, L2, Match, Status),
	'_str_scan_next'(L1, L2, Match, Lv, NewL1, Status);

    L1 =?= [] :
      L2 = _,
      Match = _,
      L1 = NewL1,
      Lv = [].

  '_str_scan_C_in_L2'(C, L2, Match, Status) :-

    L2 ? NC, C =\= NC |
	self;

    L2 ? C,
    Match = token :
      L2' = _,
      Status = false;

    L2 ? C,
    Match = separator |
      L2' = _,
      Status = true;

    L2 =?= [],
    Match = token :
      C = _,
      Status = true;

    L2 =?= [],
    Match = separator :
      C = _,
      Status = false.

  '_str_scan_next'(L1, L2, Match, Lv, NewL1, Status) :-

    Status =?= true,
    L1 ? C :
      Lv ! C |
	'_str_scan_item';

    Status = false :
      L2 = _,
      Match = _,
      L1 = NewL1,
      Lv = [].


'_str_scan'(S1, S2, Match, Sv) :-

    S1 =?= EMPTY_STRING:
      Match = _,
      S2 = _,
      S1 = Sv;

    otherwise,
    string_to_dlist(S1, L1, []),
    string_to_dlist(S2, L2, []) |
	'_str_scan_item'(L1, L2, Match, _Lm, L1'),
	list_to_string(L1', Sv). 


'_str_ipV'(S1, Si, Increment, Iv, Sv) :-

    string_length(S1) * string_length(Si) =:= 0 :
      Increment = _,
      Iv = 0,
      Sv = EMPTY_STRING;

    otherwise,
    I := string_length(S1) - string_length(Si) + Increment :
      I = Iv,
      Sv = Si.

"

	| true.
