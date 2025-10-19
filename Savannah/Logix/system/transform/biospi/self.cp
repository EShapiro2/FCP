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
       Transformer for (Ambient) Stochastic Pi Calculus procedures.
       June 2000.
*/

-language([evaluate, compound, colon]).
-export([transform/5, transform/6, spifcp/5, biospi/5]).
-mode(trust).

-include(spi_constants).
-include(bio_constants).

/*
** Transform/6
**
** Transform biospi/spifcp module to compound Fcp.
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - SpiFcp code, minus attributes.
**   Language    - spifcp/biospi.
**
** Output:
**
**   Attributes2 - Attributes1 augmented by exported Fcp procedures.
**   Compound    - Compound Fcp code.
**   Errors      - Diagnostics in the form:  Name - comment(Argument).
*/


biospi(Attributes1, Source, Attributes2, Compound, Errors) :-
	transform(Attributes1, Source, Attributes2, Compound, Errors, biospi).


spifcp(Attributes1, Source, Attributes2, Compound, Errors) :-
	transform(Attributes1, Source, Attributes2, Compound, Errors, spifcp).

transform(Attributes1, Source, Attributes2, Compound, Errors) + 
		(Language = biospi) :-
    true :
      Compound = Terms? |

	/* Get Exported list. */
	filter_attributes(Attributes1, Attributes1', Exported, BlockPrefix),

	/* Prefix Entries */
	entries(Entries?, Attributes1', Attributes1''),

	/* Complete attributes */
	Attributes2 = [export(Exports?) | Attributes1''?],

	/* Transform Source */
	tospifcp#translate(Source, Source', Errors, Errors'?),
	program.

  entries([], Attributes1, Attributes1^).
  entries(Entries, Attributes1, [entries(Entries) | Attributes1]^) :-
    otherwise | true.

  filter_attributes(In, Out, Exported, BlockPrefix) :-

    In ? export(Es), string(Es), Es =\= all :
      Exported = [Es] |
	self;

    In ? export(all) :
      Exported = all |
	self;

    In ? export(Es), list(Es) :
      Exported = Es |
	self;

    In ? block_prefix(String),
    string(String) :
      BlockPrefix = String |
	self;

    In ? Other,
    otherwise :
      Out ! Other |
	self;

    In =?= [] :
      Out = [] |
	unify_without_failure(Exported, []),
	unify_without_failure(BlockPrefix, "").


program(Exported, Source, Exports, Terms, Errors, Language,
			Entries, BlockPrefix) :-
	filter_spifcp_attributes(Source, Exported, Controls, Source',
					Errors, Errors'?),
	servers#serve_empty_scope(Scope?, Controls?, Language,
				  Exports, Ambients,
				  NextTerms, Optimize, Errors'),
	process_definitions+(Processes = [], NextScope = []),
	optimize#initialize(Optimize?, Exports?, Accessible),
	optimize#procedures(Terms''?, Accessible, [], Terms'),
	spc#stochasticize(Language, BlockPrefix, Terms'?, Terms),
	ambient_entries(Terms, Ambients, Entries).

  ambient_entries(Terms, Ambients, Entries) :-

    Ambients =\= [],
    Terms ? Procedure, Procedure =?= (Atom :- _RHSS) |
	ambient_entry(Atom, Ambients, Ambients', Entries, Entries'),
	self;

    Ambients =?= [] :
      Terms = _,
      Entries = [];

    Terms =?= [] :
      Entries = Ambients;

    Terms ? _Atom,
    otherwise |	
	self.

  ambient_entry(Atom, Ambients, NewAmbients, Entries, NewEntries) :-

    Ambients ? Name,
    arg(1, Atom, Name),
    Arity := arity(Atom) - 1 :
      Entries = [Name/Arity | NewEntries],
      NewAmbients = Ambients';

    Ambients ? Name,
    otherwise :
      NewAmbients ! Name |
	self;

    Ambients =?= [] :
      Atom = _,
      NewAmbients = [],
      NewEntries = Entries.
      

/* Extract Exports and Parameter declarations, base rate and weighter. */
filter_spifcp_attributes(Source, Exported, Controls, NextSource,
			Errors, NextErrors) +
	(Parameters = [],
	 AllVariables = Variables?, Variables,
	 Defaults = {_Weighter, _Rate},
	 SpiExports = AddExports?, AddExports) :-

    Source ? String, string(String) |
	spifcp_attribute(String,
			 Parameters, Parameters', Variables, Variables',
			 Defaults, Defaults', AddExports, AddExports',
			 Errors, Errors'),
	self;

    Source ? Tuple, Tuple =\= (_ :- _) |
	spifcp_attribute(Tuple,
			 Parameters, Parameters', Variables, Variables',
			 Defaults, Defaults', AddExports, AddExports',
			 Errors, Errors'),
	self;

    otherwise :
      AddExports = [],
      NextSource = Source,
      Variables = [] |
	choose_exports(Exported, SpiExports, Exported'),
	complete_spifcp_attributes.

  spifcp_attribute(Attribute,
		   OldParameters, NewParameters, Variables, NextVariables,
		   Defaults, NewDefaults, Exports, NextExports,
		   Errors, Errors') :-

    /* Obsolescent */
    tuple(Attribute), arity(Attribute) >= 2,
    arg(1, Attribute, global) :
      Exports = NextExports,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Attribute, [_ | Gs], []),
	validate_parameters(Gs?, Defaults, OldParameters, NewParameters,
			    Variables, NextVariables, Errors, Errors');

    tuple(Attribute), arity(Attribute) >= 2,
    arg(1, Attribute, public) :
      Exports = NextExports,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Attribute, [_ | Ps], []),
	validate_parameters(Ps?, Defaults, OldParameters, NewParameters,
			    Variables, NextVariables, Errors, Errors');

    tuple(Attribute), arity(Attribute) >= 2,
    arg(1, Attribute, export) :
      NewDefaults = Defaults,
      NewParameters = OldParameters,
      Variables = NextVariables |
	utils#tuple_to_dlist(Attribute, [_ | Es], []),
	validate_exports(Es?, Exports, NextExports, Errors, Errors');
  
    Attribute = baserate(Rate) :
      Exports = NextExports,
      NewParameters = OldParameters,
      Variables = NextVariables |
	validate_default_base_rate(Rate, Defaults, NewDefaults,
					Errors, Errors');

    Attribute =?= String(List),		% upwards compatibility from PsiFcp
    list(List),  
    string(String), string_length(String) >5 |
	utils#list_to_tuple([String | List], Attribute'),
	self;

    Attribute = weighter(Weighter) :
      Exports = NextExports,
      NewParameters = OldParameters,
      Variables = NextVariables |
	validate_default_weighter(Weighter, Defaults, NewDefaults,
					Errors, Errors');

    Attribute =?= stochastic :		% upwards compatible from PsiFcp
      Errors' = Errors,
      Exports = NextExports,
      NewDefaults = Defaults,
      NewParameters = OldParameters,
      Variables = NextVariables;

    /* skip fcp attributes - testing */
    Attribute = -_ :
      Errors' = Errors,
      Exports = NextExports,
      NewDefaults = Defaults,
      NewParameters = OldParameters,
      Variables = NextVariables;

    otherwise :
      Errors ! invalid_spifcp_attribute(Attribute),
      Exports = NextExports,
      NewDefaults = Defaults,
      NewParameters = OldParameters,
      Variables = NextVariables.

  choose_exports(FcpExports, SpiExports, Exports) :-

    FcpExports =?= [], SpiExports =?= [] :
      Exports = all;

    FcpExports =?= all :
      SpiExports = _,
      Exports = all;

    otherwise |
	utilities#concatenate_lists([FcpExports, SpiExports], Exports).


complete_spifcp_attributes(Exported, Defaults, Parameters, Controls,
			   AllVariables, Errors, NextErrors) :-

    Defaults =?= {DefaultWeighter, DefaultRate} :
      Controls = {Exported?, Parameters, ParameterNames?,
		  {FinalDefaultWeighter?, FinalDefaultRate?}
	         } |
	extract_parameter_names,
	unify_without_failure(DefaultWeighter, SPI_DEFAULT_WEIGHT_NAME),
	unify_without_failure(DefaultRate, infinite),
	variable_default_base_rate(DefaultRate, FinalDefaultRate,
				   ParameterNames, Errors, Errors'),
	variable_default_weighter,
	verify_public_variables(ParameterNames, _,
				AllVariables, NextErrors', NextErrors,
				undeclared_public_variable).

  extract_parameter_names(Parameters, ParameterNames) :-

    Parameters ? Parameter,
    arg(1, Parameter, Name) :
      ParameterNames ! Name |
	self;

    Parameters =?= [] :
      ParameterNames = [].

  variable_default_base_rate(Default, FinalDefault,
			     ParameterNames, Errors, NextErrors) :-

    Default =?= `VariableName |
	public_variable_name + (FinalVariable = FinalDefault,
				Diagnostic = non_public_variable_base_rate);

    otherwise :
      ParameterNames = _,
      Default = FinalDefault,
      Errors = NextErrors.

   variable_default_weighter(DefaultWeighter, FinalDefaultWeighter,
			     ParameterNames, Errors, NextErrors) :-

    string(DefaultWeighter) :
      ParameterNames = _,
      DefaultWeighter = FinalDefaultWeighter,
      Errors = NextErrors;

    DefaultWeighter =?= `VariableName |
	public_variable_name(VariableName, FinalDefaultWeighter,
			     ParameterNames, Errors, NextErrors,
			     non_public_default_weighter);

    tuple(DefaultWeighter), DefaultWeighter =\= `_,
    arg(1, DefaultWeighter, `VariableName) :
      Diagnostic = non_public_default_weighter_argument |
	public_variable_name(VariableName, FinalVariable,
			     ParameterNames, Errors, Errors',
			     non_public_default_weighter_name),
	utils#tuple_to_dlist(DefaultWeighter, [_Functor, Index | Arguments],
			     []),
	utils#list_to_tuple([FinalVariable?, Index | FinalArguments?],
			    FinalDefaultWeighter),
	verify_public_variables;

    otherwise :
      Diagnostic = non_public_default_weighter_argument |
	utils#tuple_to_dlist(DefaultWeighter, [String, Index | Arguments], []),
	utils#list_to_tuple([String, Index | FinalArguments?],
			    FinalDefaultWeighter),
	verify_public_variables.

  verify_public_variables(Arguments, FinalArguments,
			  ParameterNames, Errors, NextErrors, Diagnostic) :-

    Arguments ? `VariableName :
      FinalArguments ! DefaultVariable |
	public_variable_name(VariableName, DefaultVariable, ParameterNames,
			     Errors, Errors', Diagnostic),
	self;

    Arguments ? Other,
    Other =\= `_ :
      FinalArguments ! Other |
	self;

    Arguments =?= [] :
      Diagnostic = _,
      ParameterNames =_,
      Errors = NextErrors,
      FinalArguments = [].

  public_variable_name(VariableName, FinalVariable,
		       ParameterNames, Errors, NextErrors, Diagnostic) :-

    ParameterNames ? VariableName :
      Diagnostic = _,
      ParameterNames' =_,
      Errors = NextErrors,
      FinalVariable = `VariableName;

    ParameterNames ? ParameterName,
    VariableName =\= ParameterName |
	self;

    ParameterNames =?= [] :
      Errors = [Diagnostic(VariableName) | NextErrors],
      FinalVariable = 0.    


validate_exports(New, Exports, NextExports, Errors, NextErrors) :-

    New ? `String,
    nth_char(1, String, C),
    CHAR_A =< C, C =< CHAR_Z :
      Exports ! String |
	self;

    New ? String,
    nth_char(1, String, C),
    CHAR_a =< C, C =< CHAR_z :
      Exports ! String |
	self;

    New ? Other,
    otherwise :
      Errors ! invalid_export(Other) |
	self;

    New =?= [] :
      Exports = NextExports,
      Errors = NextErrors.


validate_default_base_rate(Rate, Defaults, NewDefaults, Errors, NextErrors) :-

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    number(Rate), 0 =< Rate :
      DefaultRate = Rate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    string(Rate),
    convert_to_integer(Rate, IntegerRate), 0 =< IntegerRate :
      DefaultRate = IntegerRate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    string(Rate), otherwise,
    convert_to_real(Rate, RealRate), 0 =< RealRate :
      DefaultRate = RealRate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    Rate =?= infinite :
      DefaultRate = Rate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {_DefaultWeighter, DefaultRate},
    we(DefaultRate),
    Rate =?= `Name, string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      DefaultRate = Rate,
      NewDefaults = Defaults,
      Errors = NextErrors;

    otherwise :
      NewDefaults = Defaults,
      Errors = [invalid_default_base_rate(Rate) | NextErrors].

validate_default_weighter(Weighter, Defaults, NewDefaults,
				Errors, NextErrors) :-

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    string(Weighter), nth_char(1, Weighter, C),
    CHAR_a =< C, C =< CHAR_z :
      DefaultWeighter = Weighter,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    Weighter =?= `Name,
    string(Name), nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      DefaultWeighter = Weighter,
      NewDefaults = Defaults,
      Errors = NextErrors;

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    tuple(Weighter), arg(1, Weighter, Name),
    string(Name), nth_char(1, Name, C),
    CHAR_a =< C, C =< CHAR_z :
      DefaultWeighter = NewWeighter?,
      Diagnostic = invalid_default_public_weighter_parameter,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Weighter, [_Name | Args], []),
	validate_public_weighter_params + (Variables= _, NextVariables = _),
	utils#list_to_tuple([Name, VAR_NULL | Params], NewWeighter);

    Defaults = {DefaultWeighter, _DefaultRate},
    we(DefaultWeighter),
    tuple(Weighter), arg(1, Weighter, `Name),
    string(Name), nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      DefaultWeighter = NewWeighter?,
      Diagnostic = invalid_default_public_weighter_parameter,
      NewDefaults = Defaults |
	utils#tuple_to_dlist(Weighter, [_VariableName | Args], []),
	validate_public_weighter_params + (Variables= _, NextVariables = _),
	utils#list_to_tuple([`Name, `"_" | Params], NewWeighter);

    otherwise :
      NewDefaults = Defaults,
      Errors = [invalid_default_weighter(Weighter) | NextErrors].


validate_parameters(Parameters, Defaults, Old, New,
		    Variables, NextVariables, Errors, NextErrors) +
			(Head = Tail?, Tail) :-

    Parameters ? Variable, Variable =?= `String,
    nth_char(1, String, C),
    CHAR_A =< C, C =< CHAR_Z :
      Tail ! {String},
      Variables ! Variable |
	self;

    Parameters ? Parameter, string(Parameter),
    nth_char(1, Parameter, C),
    CHAR_a =< C, C =< CHAR_z,
    Defaults = {SPI_DEFAULT_WEIGHT_NAME, Rate} :
      Tail ! Parameter(Rate) |
	self;

    Parameters ? Parameter, string(Parameter),
    nth_char(1, Parameter, C),
    CHAR_a =< C, C =< CHAR_z,
    Defaults = {Weighter, Rate},
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      Tail ! Parameter(Rate, Weighter) |
	self;

    Parameters ? Parameter(Rate), string(Parameter),
    nth_char(1, Parameter, C),
    CHAR_a =< C, C =< CHAR_z,
    Defaults = {SPI_DEFAULT_WEIGHT_NAME, _Rate} :
      Tail ! Parameter(Rate'?) |
	validate_public_channel_rate(Rate, Defaults, Rate',
				     Variables, Variables', Errors, Errors'),
	self;

    Parameters ? Parameter(Rate), string(Parameter),
    nth_char(1, Parameter, C),
    CHAR_a =< C, C =< CHAR_z,
    Defaults = {Weighter, _Rate},
    Weighter =\= SPI_DEFAULT_WEIGHT_NAME :
      Tail ! Parameter(Rate'?, Weighter) |
	validate_public_channel_rate(Rate, Defaults, Rate',
				     Variables, Variables', Errors, Errors'),
	self;

    Parameters ? Parameter(Rate, Weighter), string(Parameter),
    nth_char(1, Parameter, C),
    CHAR_a =< C, C =< CHAR_z :
      Tail ! Parameter(Rate'?, Weighter'?) |
	validate_public_channel_rate(Rate, Defaults, Rate',
				     Variables, Variables',Errors, Errors'),
	validate_public_channel_weighter(Weighter, Defaults, Weighter',
				 Variables', Variables'', Errors', Errors''),
	self;

    Parameters ? Other,
    otherwise :
      Errors ! invalid_public_parameter(Other) |
	self;

    Parameters =\= [], Parameters =\= [_|_] :
      Parameters' = [Parameters] |
	self;

    Parameters =?= [] :
      Defaults = _,
      Tail = [],
      Diagnostic = duplicate_public_parameter,
      Variables = NextVariables |
	utilities#sort_out_duplicates([Head], Head', Reply),
	diagnose_duplicates(Reply, Diagnostic, Errors, Errors'?),
	utilities#sort_out_duplicates([Old, Head'?], New, Reply'),
	diagnose_duplicates.

  diagnose_duplicates(Reply, Diagnostic, Errors, NextErrors) :-

    Reply ? Duplicate :
      Errors ! Diagnostic(Duplicate) |
	self;

    Reply =?= [] :
      Diagnostic = _,
      Errors = NextErrors;

    otherwise :
      Reply = _,
      Errors = [Diagnostic | NextErrors].


validate_public_channel_rate(Rate, Defaults, NewRate,
			     Variables, NextVariables, Errors, NextErrors) :-

    number(Rate), 0 =< Rate :
      Defaults = _,
      NewRate = Rate,
      Errors = NextErrors,
      Variables = NextVariables;

    Rate =?= infinite :
      Defaults = _,
      NewRate = Rate,
      Errors = NextErrors,
      Variables = NextVariables;

    string(Rate),
    convert_to_integer(Rate, IntegerRate), 0 =< IntegerRate :
      Defaults = _,
      NewRate = IntegerRate,
      Errors = NextErrors,
      Variables = NextVariables;

    string(Rate), otherwise,
    convert_to_real(Rate, RealRate), 0 =< RealRate :
      Defaults = _,
      NewRate = RealRate,
      Errors = NextErrors,
      Variables = NextVariables;

    Rate =?= `Name, string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      Defaults = _,
      NewRate = Rate,
      Errors = NextErrors,
      Variables = [Rate | NextVariables];

    otherwise,
    Defaults  = _Weighter(DefaultRate) :
      NewRate = DefaultRate,
      Variables = NextVariables,
      Errors = [invalid_public_channel_rate(Rate) | NextErrors].


validate_public_channel_weighter(Weighter, Defaults, NewWeighter,
			Variables, NextVariables, Errors, NextErrors) :-

    string(Weighter), nth_char(1, Weighter, C),
    CHAR_a =< C, C =< CHAR_z :
      Defaults = _,
      NewWeighter = Weighter,
      Variables = NextVariables,
      Errors = NextErrors;

    tuple(Weighter), arg(1, Weighter, Name),
    nth_char(1, Name, C),
    CHAR_a =< C, C =< CHAR_z :
      Defaults = _,
      Diagnostic = invalid_public_channel_weighter_parameter |
	utils#tuple_to_dlist(Weighter, [_Name | Args], []),
	validate_public_weighter_params,
	utils#list_to_tuple([Name, VAR_NULL | Params], NewWeighter);

    otherwise,
    Defaults = DefaultWeighter(_DefaultRate) :
      NewWeighter = DefaultWeighter,
      Errors = [invalid_public_channel_weighter(Weighter) | NextErrors],
      Variables = NextVariables.


validate_public_weighter_params(Args, Params,
			Variables, NextVariables,
			Errors, NextErrors, Diagnostic) :-

    Args ? Arg, number(Arg) :
      Params ! Arg |
	self;

    Args ? Arg, string(Arg),
    convert_to_integer(Arg, Arg') :
      Params ! Arg' |
	self;

    Args ? Arg, string(Arg), otherwise, 
    convert_to_real(Arg, Arg') :
      Params ! Arg' |
	self;

    Args ? Variable, Variable = `Name, string(Name),
    nth_char(1, Name, C),
    CHAR_A =< C, C =< CHAR_Z :
      Params ! Variable,
      Variables ! Variable |
	self;

    Args ? Arg, otherwise :
      Errors ! Diagnostic(Arg) |
	self;

    Args = [] :
      Diagnostic = _,
      Params = [],
      Variables = NextVariables,
      Errors = NextErrors.

/************************* Program Transformations ***************************/

process_definitions(Language, Source, Processes,
		    Terms, NextTerms, Scope, NextScope) :-

    Source ? (SpiLHS :- RHSS) :
      Scope ! process(SpiLHS'?, LHSS, Descriptors, ProcessScope) |

	utilities#untuple_predicate_list(';', RHSS, RHSList),
	communication_delays(RHSList?, RHSList', DelayDeclarations),
	add_delay_channels(SpiLHS, DelayDeclarations?, SpiLHS'),

	utilities#make_predicate_list(';', RHSList'?, RHSS'),

	process_definitions(Language, Processes, [], Nested, Nested'?,
				ProcessScope, ProcessScope'?),
	process(Language, LHSS?, RHSS'?, Descriptors?,
		ProcessScope', Process,	Nested'),
	nested_procedures(Process, Nested?, Terms, Terms'?),
	self;

    Source ? P,
    P =\= (_ :- _) :
      Scope ! error(invalid_process_definition(P)) |
	self;

    Source = [] :
      Language = _,
      Processes = _,
      Terms = NextTerms,
      Scope = NextScope.
	
  communication_delays(RHSList, NewRHSList, DelayDeclarations)
			+ (Index = 0) :-

    RHSList ? Clause,
    Index++,
    Clause =?= (delay(Rate) | Continuation),
    convert_to_string(Index', Id),
    string_to_dlist(Id, IdL, []),
    string_to_dlist("delay.", DL, IdL),
    list_to_string(DL, DelayName) :
      DelayDeclarations ! DelayName(Rate),
      NewRHSList ! (DelayName ? [] | Continuation) |
	self;

    RHSList ? Clause,
    Index++,
    Clause =?= (delay(Rate, Weighter) | Continuation),
    convert_to_string(Index', Id),
    string_to_dlist(Id, IdL, []),
    string_to_dlist("delay.", DL, IdL),
    list_to_string(DL, DelayName) :
      DelayDeclarations ! DelayName(Rate, Weighter),
      NewRHSList ! (DelayName ? [] | Continuation) |
	self;

    RHSList ? Clause,
    Index++,
    otherwise:
      NewRHSList ! Clause |
	self;

    RHSList =?= [] :
      Index = _,
      DelayDeclarations = [],
      NewRHSList = [].

  add_delay_channels(SpiLHS, DelayDeclarations, NewSpiLHS) :-

    DelayDeclarations =\= [],
    SpiLHS =?= Atom + PrivateChannels :
      NewSpiLHS = Atom + AddedChannels? |
	utilities#untuple_predicate_list(',', PrivateChannels,
						PList, DelayDeclarations),
	utilities#make_predicate_list(',', PList, AddedChannels);


    DelayDeclarations =\= [],
    SpiLHS =\= _ + _ :
      NewSpiLHS = SpiLHS + AddedChannels? |
	utilities#make_predicate_list(',', DelayDeclarations, AddedChannels);

    DelayDeclarations =?= [] :
      SpiLHS = NewSpiLHS.

/************************* Process Transformations ***************************/

process(Language, LHSS, RHSS, Descriptors, Scope, Process, Nested) :-

    LHSS =?= [] :
      RHSS = _,
      Descriptors = _,
      Language = _,
      Scope = [],
      Process = [],
      Nested = [];

    LHSS =?= {OuterLHS, InnerLHS},
    Descriptors =\= [] :
      Nested ! outer(OuterLHS, Initializer?, []),
      /* This process needs access to the scheduler. */
      Scope ! logix_variables([SCHEDULER]),
      Descriptors' = [] |
	arg(1, InnerLHS, Name),
	initialize_parameters(Name, Descriptors, Initializer),
	self;

    LHSS =?= {_OuterLHS, InnerLHS},
    Descriptors =?= [],
    RHSS =\= (_|_), RHSS =\= (_;_)  :
      Scope ! code(no_guard, [], []),
      Process = no_guard(InnerLHS, RHSS'?, []) |
	transform_body(Language, RHSS, RHSS', Nested, [], Scope', []);

    LHSS = {_OuterLHS, InnerLHS},
    otherwise :
      Descriptors = _,
      Process = _Type(InnerLHS, RHSS'?, _Action) |
	guarded_clauses(Language, RHSS, RHSS', Process, Nested, Scope).

  initialize_parameters(Name, Descriptors, Initializer) +
		(AskList = Asks?, Asks, TellList = Tells?, Tells) :-

    Descriptors ? Descriptor |
	make_and_name_parameter(Name, Descriptor, Asks,Asks'?, Tells,Tells'?),
	self;

    Descriptors =?= [] :
      Asks = [],
      Tells = [],
      Initializer = (Ask? : Tell? | Name) |
	utilities#make_predicate_list(',', AskList?, Ask),
	utilities#make_predicate_list(',', TellList?, Tell).

  make_and_name_parameter(Name, Descriptor, Asks,NextAsks, Tells,NextTells) :-

    Descriptor = ChannelName(BaseRate),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Tells = [write_channel(
		new_channel(ChannelId, `ChannelName, Rate?),
				VAR_SCHEDULER) |
	      NextTells],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	complete_rate,
	parameters_to_asks([BaseRate], [number], Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    string(ComputeWeight),
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Tells = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, Rate?),
				VAR_SCHEDULER) |
	       NextTells],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	complete_rate,
	parameters_to_asks([BaseRate], [number], Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    tuple(ComputeWeight), ComputeWeight =?= `_,
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Tells = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, Rate?),
				VAR_SCHEDULER) |
	      NextTells],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	complete_rate,
	parameters_to_asks([BaseRate, ComputeWeight], [number, string],
				Asks, NextAsks);

    Descriptor = ChannelName(BaseRate, ComputeWeight),
    tuple(ComputeWeight), ComputeWeight =\= `_,
    string_to_dlist(ChannelName, Suffix, []),
    string_to_dlist(Name, PH, PS) :
      Tells = [write_channel(
		new_channel(ChannelId, `ChannelName, ComputeWeight, Rate?),
				VAR_SCHEDULER) |
	      NextTells],
      Ops = [known | Ops],
      PS = Suffix |
	list_to_string(PH, ChannelId),
	complete_rate,
	utils#tuple_to_dlist(ComputeWeight, [Functor, _ | List], []),
	parameters_to_asks([BaseRate, Functor | List], [number, string | Ops],
				Asks, NextAsks);

    string(Descriptor) |
      Name = _,
      Asks = NextAsks,
      Tells = [(`Descriptor = VAR_NULL) | NextTells].

  complete_rate(ChannelName, BaseRate, Rate) :-

    string_to_dlist(ChannelName, ChannelChars, []),
    string_to_dlist("delay.", DelayChars, _) :
      ChannelChars = DelayChars,
      Rate = delay(BaseRate);

    otherwise :
      ChannelName = _,
      Rate = BaseRate.

  parameters_to_asks(List, Ops, Asks, NextAsks) :-

    List ? Parameter, Parameter = `_,
    Ops ? Op :
      Asks ! Op(Parameter) |
	self;
 
    List ? Parameter, constant(Parameter),
    Ops ? _ |
	self;

    List =?= [] :
      Ops = _,
      Asks = NextAsks.

nested_procedures(Process, Nested, Terms, NextTerms) :-

    Process =\= [] :
      Terms ! Process,
      Process' = [] |
	self;

    Process =?= [],
    Nested ? Proc :
      Terms ! Proc |
	self;

    Process =?= [],
    Nested =?= [] :
      Terms = NextTerms.


/************************* Guard Transformations *****************************/

guarded_clauses(Language, RHS1, RHS2, Process, Nested, Scope) +
			(Mode = none, Index = 0,
	NextRHSS, RHSS = NextRHSS?, NextPrepares = [], FinalMode = _) :-

    RHS1 =?= (_ | _),
    Index++ :
      NextRHSS = [Clauses?],
      FinalMode = Mode'? |
	guarded_clause(Language, RHS1, GuardMode(Index'), Clauses,
			Nested, [], Scope, Scope'?),
	utilities#update_process_mode(Mode, GuardMode, Mode'),
	make_right_hand_side + (Index = 1),
	make_rhs2 + (Scope = Scope', NextScope = Scope''?),
	code_reply;

    RHS1 =?= (Guarded ; RHS1'), Guarded =?= (_|_),
    Index++ :
      NextRHSS ! Clauses? |
	guarded_clause(Language, Guarded, GuardMode(Index'), Clauses,
			Nested, Nested'?, Scope, [end_clause | Scope'?]),
	utilities#update_process_mode(Mode, GuardMode, Mode'),
	self;

    otherwise :
      Index = _,
      FinalMode = _,
      Process = none(_LHS, _ProcessRHS, []),
      Scope ! error(invalid_guarded_clause(RHS1)),
      NextRHSS = [],
      Nested = [] |
	make_right_hand_side + (Index = 1),
	make_rhs2 + (PrepareProcedure = _, NextScope = []).

  code_reply(Process, FinalMode, PrepareProcedure, Scope) :-

    PrepareProcedure =?= (_ :- PrepareRHS) :
      Process = FinalMode(_LHS, ProcessRHS, PrepareProcedure),
      Scope = [code(FinalMode, ProcessRHS, PrepareRHS)];

    otherwise :
      PrepareProcedure = _,
      Process = FinalMode(_LHS, _RHSS, []),
      Scope = [code(FinalMode, [], [])].

  make_rhs2(Language, Mode, ClauseList, Prepares, RHS2,
	PrepareProcedure, Scope, NextScope) :-

    Mode =?= communicate :
      RHS2 = (PrepareGuards? | Communicator?),
      PrepareProcedure = (CommunicationLHS? :- FcpClauses?),
      /* This process needs access to the scheduler. */
      Scope ! logix_variables([SCHEDULER]),
      Scope' = [lhss(_OuterLHS, InnerLHS) | NextScope] |
	utilities#make_predicate_list(';', ClauseList, FcpClauses),
	prepares_to_guards(Prepares, PrepareGuards),
	utilities#make_communicator(Language, InnerLHS, Communicator,
					CommunicationLHS);

    /* compared, logix, none */
    Mode =\= communicate, Mode =\= compare, Mode =\= conflict :
      Language = _,
      Prepares = _,
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = NextScope |
	utilities#make_predicate_list(';', ClauseList, FcpClauses);

    Mode =?= compare :
      Language = _,
      Prepares = _,
      RHS2 = FcpClauses?,
      PrepareProcedure = [],
      Scope = [lhss(_Outer, Inner) | NextScope] |
	arg(1, Inner, Name),
	utilities#concatenate_lists(
			[ClauseList,[(otherwise | fail(Name-compare))]],
					ClauseList'),
	utilities#make_predicate_list(';', ClauseList'?, FcpClauses);

    Mode =?= conflict :
      Language = _,
      Prepares = _,
      ClauseList = _,
      RHS2 = true,
      PrepareProcedure = [],
      Scope = [error("conflicting_guards") | NextScope].
      

  prepares_to_guards(Prepares, PrepareGuards) +
	(NextAsk, Asks = NextAsk?, NextTell, Tells = NextTell?) :-

    Prepares ? {Ask, Tell} :
      NextAsk ! Ask,
      NextTell ! Tell |
	self;

    Prepares =?= [] :
      NextAsk = [],
      NextTell = [],
      PrepareGuards = (Asks'? : Tells'?) |
	utilities#make_predicate_list(',', Asks, Asks'),
	utilities#make_predicate_list(',', Tells, Tells').


  make_right_hand_side(RHSS, Index, ClauseList,	Prepares, NextPrepares) :-

    RHSS ? RHS,
    RHS = {Mode, RHSList},
    Index++ |
	make_clauselist(Mode, Index, RHSList,
		ClauseList, ClauseList'?, Prepares, Prepares'?),
	self;

    RHSS ? true,
    Index++ |
	self;

    RHSS =?= [] :
      Index = _,
      ClauseList = [],
      Prepares = NextPrepares.


  make_clauselist(Mode, Index, RHSList,
	ClauseList, NextClauseList, Prepares, NextPrepares) :-

    Mode =?= receive,
    RHSList ? ({{Identify, Write}, Consume} | Body) :
      ClauseList ! (VAR_CHOSEN = Index, Consume | Body),
      Prepares ! {Identify, Write} |
	self;

    Mode =?= send,
    RHSList ? ({{Identify, Write}, Unify}  | Body) :
      Prepares ! {Identify, Write},
      ClauseList ! (VAR_CHOSEN = Index : Unify | Body) |
	self;

    Mode =?= none,
    RHSList ? _ |
	self;

    otherwise,
    RHSList ? Other :
      ClauseList ! Other |
	self;

    RHSList =?= [] :
      Mode = _,
      Index = _,
      ClauseList = NextClauseList,
      Prepares = NextPrepares.


guarded_clause(Language, RHS1, Control, Clauses, Nested, NextNested,
			Scope, NextScope) :-

    /* Recognize compound_guard */
    RHS1 =?= (Guard | Guarded), Guarded =?= (_ | _) :
      RHS1' = (Guard | [Guarded]) |
	self;

    RHS1 =?= (Guard | Guarded), Guarded =?= (_ ; _) :
      RHS1' = (Guard | [Guarded]) |
	self;

    RHS1 =?= (Guard | Body1), Body1 =\= (_ | _), Body1 =\= (_ ; _) :
      LastClause = (BodyGuard? | Body2?) |
	transform_guard(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, Scope'?),
	transform_body.


RECEIVE_REQUEST(Medium, Message) =>
      Control = receive(CommunicationIndex),
      Scope = [guard_receive(Medium, Message, CommunicationIndex, BodyGuard)
              | NextScope],
      Clauses = receive([LastClause]).

SEND_REQUEST(Medium, Message) => 
      Control = send(CommunicationIndex),
      Scope = [guard_send(Medium, Message, CommunicationIndex, BodyGuard)
              | NextScope],
      Clauses = send([LastClause]).

transform_guard(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, NextScope) :-

    Guard =?= otherwise :
      Control = otherwise(_Index),
      Clauses = otherwise([LastClause]),
      BodyGuard = otherwise,
      Scope = NextScope;      
    
    Guard =?= {_,_} |
    	transform_guard2;

    Guard =?= {_,_,_},
    Guard =\= (_ : _) |
    	transform_guard3;

    otherwise :
      Scope = [Result? | NextScope] |
	logix_guards.


  transform_guard2(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, NextScope) :-

    arity(Guard, 2), arg(1, Guard, "?"), arg(2, Guard, Channel) :
      RECEIVE_REQUEST(Channel, []);

    Guard =?= Channel! :
      SEND_REQUEST(Channel, []);

    Guard =?= accept(_) :
      RECEIVE_REQUEST(Guard, []);

    Guard =?= enter(_) :
      SEND_REQUEST(Guard, []);

    Guard =?= expel(_) :
      RECEIVE_REQUEST(Guard, []);

    Guard =?= exit(_) :
      SEND_REQUEST(Guard, []);

    otherwise :
      Scope = [Result? | NextScope] |
	logix_guards.


  transform_guard3(Guard, Control, LastClause, Clauses, BodyGuard,
			Scope, NextScope) :-

    Guard =?= (Channel ? Message) :
      RECEIVE_REQUEST(Channel, Message);

    Guard =?= (Channel ! Message) :
      SEND_REQUEST(Channel, Message);

    Guard =?= (merge + _) :
      RECEIVE_REQUEST(Guard, []);

    Guard =?= (merge - _) :
      SEND_REQUEST(Guard, []);

    Guard =\= (_ ? _), Guard =\= (_ ! _),
    Guard =\= (_ =?= _), Guard =\= (_ =\= _),
    Guard =\= (merge + _), Guard =\= (merge - _),
    Guard =\= (_ & _) :
      Scope = [Result? | NextScope] |
	logix_guards;

    otherwise :
      Clauses = compare([LastClause]),
      Control = compare(_Index) |
	compare_channels + (Channels = [], NextChannels = _).


logix_guards(Guard, Control, LastClause, Clauses, BodyGuard, Result) :-

    tuple(Guard),
    Guard =\= `_, Guard =\= ?_,
    Guard =\= (_ , _), Guard =\= (_ : _),
    arity(Guard, Arity) :
      Clauses = logix([LastClause]),
      Control = logix(_),
      Index = 1,
      Result = logix_variables(LogixVars?) |
	copy_ask_guards;

    Guard =?= (_ , _) :
      BodyGuard = Guard,
      Clauses = logix([LastClause]),
      Control = logix(_),
      Result = logix_variables(LogixVars?) |
	utilities#find_logix_variables(Guard, LogixVars, []);

    Guard =?= (_ : _) :
      BodyGuard = Guard,
      Clauses = logix([LastClause]),
      Control = logix(_),
      Result = logix_variables(LogixVars?) |
	utilities#find_logix_variables(Guard, LogixVars, []);

    otherwise:
      BodyGuard = true,
      Clauses = none([LastClause]),
      Control = none(_),
      Result = error(invalid_guard(Guard)).

  copy_ask_guards(Guard, Index, Arity, BodyGuard, LogixVars) :-

    Index++ < Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = (Predicate, BodyGuard') |
	utilities#find_logix_variables(Predicate, LogixVars, LogixVars'?),
	self;

    Index =:= Arity,
    arg(Index, Guard, Predicate) :
      BodyGuard = Predicate |
	utilities#find_logix_variables(Predicate, LogixVars, []).


compare_channels(Guard, BodyGuard, Channels, NextChannels, Scope, NextScope) :-

    Guard =?= (Guard' & Compares) :
      BodyGuard = (BodyGuard'?, Comparers?) |
	compare_channels(Guard', BodyGuard', Channels, Channels',
				Scope, Scope'?),
	compare_channels(Compares, Comparers, Channels'?, NextChannels,
				Scope', NextScope);

    Guard =\= (_ & _), Guard =\= (_ =?= _), Guard =\= (_ =\= _) :
      BodyGuard = true,
      NextChannels = Channels,
      Scope = [error(invalid_compare_guard(Guard)) | NextScope];

    otherwise :
      Scope = [guard_compare(Guard, Channels, NextChannels, BodyGuard) |
		NextScope].


/************************* Body Transformations ******************************/

transform_body(Language, Body1, Body2, Nested, NextNested, Scope, NextScope) :-
    true :
      NextGoals = [] |
	transform_body1,
	utilities#make_predicate_list(',', Goals?, Body2).

  transform_body1(Language, Body1, Goals, NextGoals, Nested, NextNested,
			Scope, NextScope) :-

    Body1 = (Body2, Body1') |
	transform_body1(Language, Body2, Goals, Goals'?, Nested, Nested'?,
			Scope, Scope'?),
	self;

    Body1 = (Channel ? _Message),
    nth_char(1, Channel, C), CHAR_a =< C, C =< CHAR_z :
      Language = _,
      Scope ! error(receive_in_body(Body1)),
      Goals = NextGoals,
      Nested = NextNested,
      Scope' = NextScope;

    list(Body1) :
      Goals = [Body2? | NextGoals] |
	tospifcp # new_scope(Body1, Body1', Errors, []),
	translation_errors(Errors?, Scope, Scope'),
	new_scope;

    Body1 =?= Name(Content),
    string(Name), Name =\= EMPTY,
    list(Content) :
      Goals = [Body, Body2? | NextGoals] |
	tospifcp # new_scope(Content, Body1', Errors, []),
	translation_errors(Errors?, Scope, Scope'?),
	new_ambient(Name, Body, Scope', Scope''?),
	new_scope;

    Body1 =?= Name # Call1 :
      Language = _,
      Goals = [(Name # Call2) | NextGoals],
      Scope = [Result? | NextScope],
      Nested = NextNested |
	parse_remote_call(Call1, Call2, Result);

    otherwise :
      Language = _,
      Goals = [Body2 | NextGoals],
      Scope ! call(Body1, Body2),
      Nested = NextNested,
      Scope' = NextScope.

  translation_errors(Errors, Scope, NextScope) :-

    Errors ? Error :
      Scope ! error(translation(Error)) |
	self;

    Errors =?= [] :
      Scope = NextScope.

  new_ambient(Name, Body, Scope, NextScope) :-

    string_to_dlist(SERVICE, ServiceIdL, [CHAR_MINUS | IdL?]) :
      Scope = [next_scope_id(Id), ambient(Id?) | NextScope],
      Body = new_ambient(Name, Id, `ServiceId?) |
	string_to_dlist(Id?, IdL, []),
	list_to_string(ServiceIdL, ServiceId).

  new_scope(Language, Body1, Body2, Nested, NextNested, Scope, NextScope) :-

    Body1 =?= [(_ :- _) | _] :
      Body1' = [true | Body1] |
	self;

    Body1 =?= [Body],
    Body =\= (_ :- _) :
      Processes = [],
      Channels = [] |
	expand_new_scope;

    Body1 =?= [Body | Processes],
    Body =\= (_ :- _), Processes =?= [(_ :- _)| _] :
      Channels = [] |
	expand_new_scope;

    Body1 =?= [Channels, Body | Processes],
    Channels =\= (_ :- _), Body =\= (_ :- _) |
	expand_new_scope.


expand_new_scope(Language, Channels, Body, Processes, Body2,
		Nested, NextNested, Scope, NextScope) :-
    true :
      Scope ! new_scope_id(Id),
      Body2 = Id? |
	make_new_lhs,
	process_definitions(Language, [(SpiLHS :- Body)], Processes,
			    Nested, NextNested,	Scope', NextScope).

  make_new_lhs(Id, Channels, SpiLHS) :-

    Channels =?= [] :
      SpiLHS = `Id;

    Channels =\= [] :
      SpiLHS = `Id + Channels.


parse_remote_call(Call1, Call2, Result) :-

    Call1 =?= Name # Call1', string(Name) :
      Call2 = Name # Call2' |
	self;

    Call1 =\= _ # _, Call1 =\= `_,
    tuple(Call1), arg(1, Call1, Name), string(Name) :
      Call2 = Call1,
      Result = logix_variables(LogixVars?) |
	utilities#find_logix_variables(Call1, LogixVars, []);

    Call1 = `Name, string(Name) :
      Call2 = Name,
      Result = logix_variables([]);

    Call1 =\= _ # _,
    tuple(Call1), arg(1, Call1, `_) :
      Result = remote_call(Call1, Call2);

    otherwise :
      Call2 = Call1,
      Result = logix_variables(LogixVars?) |
	utilities#find_logix_variables(Call1, LogixVars, []).
