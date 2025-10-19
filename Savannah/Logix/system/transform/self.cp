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
       Language Transformer for FCP
       08/88
*/

-export([languages/8, language/8, language_names/4]).
-mode(trust).
-language(compound).


procedure language_attribute(Any, String, Any, language([Any])).

/*
** languages_names/4 collects option/attribute language specifications.
**
** Input:	Options			% esp: 	language(Name)
**						language(NameList)
**		Target			% Truncate list before this name
**		Attributes		% like  Options
** Output:	Languages		% NameList
*/

language_names(Options, Target, Attributes, NameList) :-
	options(Options, Attributes, Attributes', _TransformerId),
	language_list(Attributes', Target, _Attributes, Languages),
	remove_duplicates(Languages, Target, NameList).

procedure languages(Any, String, Any, [Any], [Any], [Any], [Any], Any).

/*
** languages/8 Calls services to transform options, attributes, source terms.
**
** The services are specified in the options and/or attributes.
**
** Input:	Options			% esp: 	transformer(Path)
**						language(Name)
**						language(NameList)
**		Target			% one of [dg, typed] initially
**		Attributes1		% like  Options
**		Terms1			% source
**		Output2			% tail of diagnostic stream
**
** Output:	Attributes4		% transformed  Options..Attributes1
**		Terms3			% transformed source
**		Output1			% diagnostics
*/

languages(Options, Target, Attributes1, Attributes4,
		Terms1, Terms3, Output1, Output2
) :-
	options(Options, Attributes1, Attributes2, TransformerId),
	language_list(Attributes2, Target, Attributes3, Languages1),
	remove_duplicates(Languages1?, Target, Languages2),
	simplify_language_attribute(Languages2?, Languages3),
	call_transformations(TransformerId, Languages2?,
			     {[language(Languages3?) | Attributes3?], Terms1,
			       Attributes4, Terms3
			     },
			     Output1, Output2
	).


options(Os, As1, As2, TransformerId) :-

    Os ? transformer(Path) : TransformerId' = _ |
	options,
	computation_utils # path_id(computation # Path, LId, Ok),
	check_context(Ok, Path, LId, TransformerId);

    Os ? OV, OV = override(AN,AV) :
      As2 ! AN(AV) |
	override(AN, As1, As1'),
	options;

    otherwise,
    Os ?  Attribute :
      As2 ! Attribute |
	options;

    Os = [] :
      As1 = As2,
      TransformerId = standard |
	true;

    otherwise :
      Os' = [Os] |
	options.

  override(AN, As, As1) :-

    As ? AN |
	self;

    As ? Tuple, arg(1, Tuple, AN) |
	self;

    As ? Other,
    otherwise :
      As1 ! Other |
	self;

    As =?= [] :
      AN = _,
      As1 = [];

    otherwise :
      As' = [As] |
	self.

  check_context(true, _, LId, LId^).
  check_context(Other, Path, _, error(Path, Other)^) :-
    otherwise |
	true.


language_list(As1, To, As2, Ls)  :-

    As1 ? language(L) |
	language_list,
	standard_language(L, To, L'),
	include_languages(L', Ls, Ls');

    otherwise,
    As1 ? A :
      As2 ! A |
	language_list;

    As1 = [] :
      As2 = [],
      Ls = [To] |
	true.


standard_language(L, To, Ls) :-

    L = compound : To = _,
      Ls = [compound, colon, typed] ;

    L = d : To = _,		% The successor of c!
      Ls = [dfcp] ;

    L = biospi : To = _,
      Ls = [evaluate, biospi#biospi, compound, colon, typed] ;

    L = psifcp : To = _,
      Ls = [evaluate, biospi#spifcp, compound, colon, typed] ;

    L = spifcp : To = _,
      Ls = [evaluate, biospi#spifcp, compound, colon, typed] ;

    L = fcp :
      Ls = [To] ;		% cut here

    list(L) : To = _ |
	filter_and_replace([psifcp(biospi#spifcp)], L, Ls) ;

    L = [] : To = _,
      Ls = [] ;

    otherwise : To = _,
      Ls = [L] .

  filter_and_replace(Replacements, List, Ls) :-

    List ? String, string(String) :
      Ls ! String'? |
	replace_string(Replacements, String, String'),
	self ;

    List ? Other,
    otherwise :
      Ls ! Other |
	self ;

    List =?= [] :
      Replacements = _,
      Ls = [] ;

    otherwise :
      Replacements = _,
      Ls = [List] .

  replace_string(Replacements, String, Replacement) :-

    Replacements ? String(Replacement^) :
      Replacements' = _ ;

    Replacements ? _Other,
    otherwise |
	self;

    Replacements =?= [] :
      String = Replacement .


include_languages(LNs, Ls1, Ls2) :-

    LNs ? LN :
      Ls1 ! LN |
	self ;

    LNs = [] :
      Ls1 = Ls2 ;

    otherwise :
      Ls1 = [LNs | Ls2] .


remove_duplicates(Ls1, To, Ls2) + (Inherit = false) :-

    Ls1 ? To,
    To = dfcp, Inherit = false : Ls1' = _,
      Ls2 = [inherit] ;

    Ls1 ? To, otherwise : Ls1' = _, Inherit = _,
      Ls2 = [] ;

    Ls1 ? To#_ : Ls1' = _, Inherit = _,
      Ls2 = [] ;

    Ls1 ? _#To : Ls1' = _, Inherit = _,
      Ls2 = [] ;

    Ls1 ? L, L =\= To, L = inherit : Inherit = _,
      Inherit' = true,
      Ls2 = [L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 ? L, L =\= To, L = dfcp, Inherit = false :
      Inherit' = true,
      Ls2 = [inherit, L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 ? L, L =\= To, otherwise :
      Ls2 = [L | Ls2'?] |
	remove_duplicates1(L, Ls1', Ls1''),
	self;

    Ls1 = [] : To = _, Inherit = _,
      Ls2 = [] .


remove_duplicates1(L, Ls1, Ls2) :-

    Ls1 ? L |
	self;

    Ls1 ? O, L =\= O :
      Ls2 = [O | Ls2'?] |
	self;

    Ls1 = [] : L = _,
      Ls2 = [] .


simplify_language_attribute(Ls2, Ls3) :-

    Ls2 ? (_Module#Goal) :
      Ls2'' = [Goal | Ls2'] |
	self;

    Ls2 ? Language, Language =\= (_#_) :
      Ls3 ! Language |
	self;

    Ls2 =?= [] :
      Ls3 = [].


call_transformations(LId, Ls, Stuff, O1, O2) :-

    LId = standard |
	self # service_id(LId'),
	call_transformations;

    LId = error(Path, Other) : Ls = _,
      Stuff = {As, _, As, []},
      O1 = [language(Path) - Other | O2] |
	true;

    Ls = [] : LId = _,
      Stuff = {As, Ts, As, Ts},
      O1 = O2 |
	true;

    otherwise,				% LId = Service Id
    Ls ? L |
	call_service_and_functor(L, L', T, E),
	computation # call([computation_utils # path_context(LId#L'?, LC, Ok)],
			   Events
		      ),
	call_transformations,
	call_transformation(Ok, L, T, E, LC, Events, Stuff, Stuff', O1, O1').

  call_service_and_functor(S#L, S^, L^, L^).
  call_service_and_functor(L, L^, transform^, L^) :-
    otherwise | true.

call_transformation(Ok, L, T, E, LC, Events, Stuff1, Stuff2, O1, O2) :-

    Ok = true,
    Stuff1 = {As1, Ts1, As4, Ts4} : L = _,
      Stuff2 = {As3, Ts3, As4, Ts4},
      write_channel(T(As1, Ts1, As2, Ts2, Ds), LC, LC') |
	close_channel(LC'),
	serve_call(Events, Ds, E, As2, Ts2, As3, Ts3, O1, O2);

    Ok = false(Reason) : LC = _, Events = _, T = _, E = _,
      Stuff1 = Stuff2,
      O1 = [(language(L) : Reason) | O2] |
	true.


Path ::= String ; Channel ; [String] ; {`'#', Path, Path}.
Channel ::= Vector.

procedure language(Path, Any, [Any], [Any], Any, [Any], [Any], Any).

/*
** language/8 calls a Service to transform attributes and source terms.
**
** Input:	Service			% the implementing service
**		As1			% list of attributes
**		Ts1			% source
**		O2			% tail of diagnostic stream
**		Id			% Diagnostic identifier (language name)
**
** Output:	As3			% transformed attributes
**		Ts3			% transformed source
**		O1			% diagnostics
*/

language(Service, As1, Ts1, O1, Id, As3, Ts3, O2) :-
	computation #
		call([Service # transform(As1, Ts1, As2, Ts2, Ds)], Events),
	serve_call(Events, Ds, Id, As2, Ts2, As3, Ts3, O1, O2).


serve_call([aborted | _], _, _, _, _, []^, []^, O, O^).
serve_call(Es, Ds, Id, As2, Ts2, As3, Ts3, O1, O2) :-

    Es ? BE,
    string(BE), BE =\= aborted |
	serve_call;

    Es ? failed(call(_), _) |
	serve_call;

    otherwise,
    Es ? E |
	computation # E,
	serve_call;

    Ds ? Comment, Comment = (_ : _) :
      O1 ! Comment |
	self;

    Ds ? E, E =\= (_ : _) :
      O1 ! (Id ! E) |
	serve_call;

    As2 ? A :
      As3 ! A |
	serve_call;

    Ts2 ? T :
      Ts3 ! T |
	serve_call;

    Ds = [] : Es = _, Id = _,
      As2 = As3,
      Ts2 = Ts3,
      O1 = O2 |
	true.
