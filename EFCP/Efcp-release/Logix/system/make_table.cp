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

/*  make_table#rectangle(ListOrTuple, Options, OutputList)

	Format input list or  tuple into tabular list.

* Options:			(default)	constraint

        space(Blank_Spacing)	(1)		> 0
	minimum(Minimum_Width)	(1)		> 0
	maximum(Maximum_Columns)((W+B)/(M+B))	< 61 % based on W = 120
	width(Page_Width)	(78)		> 8
	Convert			("%0.12G")	Convert is a legal c-format
						specification string to 
					 	convert one real number.

	Convert may be produced by a call to:

		c_output#prepare(<specification>, Convert, _) 

* Measure:

	Length = length of input list
        Entries = list of (converted)  entries [String, ...]
	EntryMinWidth = actual minimum {length(String)}
	EntryMaxWidth = actual maximum {length(String)}
		(Fail if EntryMaxWidth exceeds Page_Width)
* Compute:

	MinimumColumns = max(1,
		(Page_Width + Blank_Spacing) / (EntryMaxWidth + Blank_Spacing))
	MaximumColumns = min(Maximum_Columns,
		(Page_Width + Blank_Spacing) / (EntryMinWidth + Blank_Spacing))
	MinimumRows = ((Length - 1) / MaximumColumns) + 1
	MaximumRows = ((Length - 1) / MinimumColumns) + 1

	If  MaximumColumns = 1,
		return  OutputList = Entries

* Algorithm:

	For each value of  Rows  from  MinimumRows  up to  MaximumRows:

	 * Columns = (Length - 1)/Rows + 1
	   Create a  Tuple ,  ColumnMaximums  of size  Columns
	   RowIndex = 0, ColumnIndex = 0; ColumnMaximum = 0; TableWidth = 0;

	 * For each  Entry  of  Entries:
	
		If  RowIndex == 0
			ColumnIndex++
		RowIndex++

		EntrySize = max(Minimum_Width, length(Entry)) 
		If  ColumnIndex <  Columns
			EntrySize += Blank_Spacing
		maximize  ColumnMaximum  with  EntrySize  
		RowIndex'' = mod(RowIndex', Rows)
		if  RowIndex'' == 0
			ColumnMaximums[ColumnIndex] = ColumnMaximum
			TableWidth += ColumnMaximum

	 * If  TableWidth   exceeds  Page_Width
	 	fail this pass and iterate for next larger  Rows  (or fail)

	If succeeded
		proceed to output phase
	otherwise (failed)
		return  OutputList = Entries

	output:

	 * construct  SpacesTuple  a tuple of size  Columns ,
	   where arg(N) of the tuple is a string of N ascii spaces

	 * Create a tuple,  RowTuple , of  Rows  pairs, whose first
	   argument is the head of a list of strings, and whose second
	   argument addresses the tail of the list;
	   RowIndex = 0, ColumnIndex = 0;

	 * For each  Entry  of  Entries:
	
		If  RowIndex == 0
			ColumnIndex++
		RowIndex++

		Add  Entry  to  RowTuple[RowsIndex]
		RowIndex'' = mod(RowIndex', Rows)
		if  RowIndex'' == 0
			close RowsTuple[RowsIndex]
			concatenate strings in  RowTuple[RowIndex]
			add concatenated string to  OutputList
		else
			SpacesIndex =
				ColumnMaximums[ColumnIndex] - length(Entry)
			add  SpacesTuple[SpacesIndex]  to
			        RowsTuple[RowIndex]
*/

-language(compound).
-export(rectangle/3).
-mode(interrupt).

rectangle(ListOrTuple, Options, Output_List) :-

    tuple(ListOrTuple) |
	utils#tuple_to_dlist(ListOrTuple, ListOrTuple', []),
	self;

    list(ListOrTuple) :
      OptionsTuple =
	{Blank_Spacing, Minimum_Width, Maximum_Columns, Page_Width, Convert} |
	extract_options,
	Too_Long := Page_Width + 1,
	measure(ListOrTuple, Convert, 0, Too_Long, Minimum_Width,
		Entries, Length, EntryMinWidth, EntryMaxWidth),
/*
computation#display(term,
	{blank_spacing(Blank_Spacing), minimum_width(Minimum_Width),
	 maximum_columns(Maximum_Columns), page_width(Page_Width),
	 convert(Convert), length(Length), entry_minimum_width(EntryMinWidth),
	 entry_maximum_width(EntryMaxWidth), Output_List},
		    type(ground)
),
*/

	MaximumColumns := min(Maximum_Columns,
	  (Page_Width + Blank_Spacing) / (EntryMinWidth + Blank_Spacing)),
	MinimumColumns := min(MaximumColumns, max(1,
	  (Page_Width + Blank_Spacing) / (EntryMaxWidth + Blank_Spacing))),

	MaximumWidth := max(Minimum_Width, EntryMaxWidth) + Blank_Spacing,
	compose,
/*
computation#display(term,
	(mimimumcolumns(MinimumColumns), maximumcolumns(MaximumColumns)),
		    type(ground)
),
*/
	output;

    ListOrTuple =?= [] :
      Options = _,
      Output_List = [];

    otherwise :
      ListOrTuple' = [ListOrTuple] |
	self.


extract_options(Options, OptionsTuple, Errors) :-

    Options ? Option,
    tuple(Option) |
	validate_option(Option, OptionsTuple, OptionsTuple', Errors, Errors'),
	self;
    
    Options ? Option,
    string(Option) |
	validate_convert(Option, OptionsTuple, OptionsTuple', Errors, Errors'),
	self;

    Options =?= [],
    make_tuple(5, Dummy),
    OptionsTuple =
      {Blank_Spacing, Minimum_Width, Maximum_Columns, Page_Width, Convert} |
	default_option(Blank_Spacing, 1),
	default_option(Minimum_Width, 1),
	MaxCol := (Page_Width+Blank_Spacing)/(Minimum_Width+Blank_Spacing),
	validate_option(maximum(MaxCol?), Dummy, _, Errors, []),
	default_option(Maximum_Columns, MaxCol?), 
	default_option(Page_Width, 78),
	default_option(Convert, "%0.12G");

    tuple(Options) :
      Options' = [Options] |
	self;

    string(Options) :
      Options' = [Options] |
	self;

    otherwise :
      Errors = [invalid_option(Options) | Errors'],
      Options' = [] |
	self.

validate_option(Option, Tuple, NewTuple, Errors, NewErrors) :-

    Tuple =
      {Blank_Spacing, _Minimum_Width, _Maximum_Columns, _Page_Width, _Convert},
    Option =?= space(Space),
    integer(Space),
    Space > 0,
    we(Blank_Spacing) :
      Blank_Spacing = Space,
      Tuple = NewTuple,
      Errors = NewErrors;

    Tuple = 
      {_Blank_Spacing, Minimum_Width, _Maximum_Columns, _Page_Width, _Convert},
    Option =?= minimum(Minimum),
    integer(Minimum),
    Minimum > 0,
    we(Minimum_Width) :
      Minimum_Width = Minimum,
      Tuple = NewTuple,
      Errors = NewErrors;

    Tuple =
      {_Blank_Spacing, _Minimum_Width, Maximum_Columns, _Page_Width, _Convert},
    Option =?= maximum(Maximum),
    integer(Maximum),
    Maximum > 0,
    Maximum < 61,
    we(Maximum_Columns) :
      Maximum_Columns = Maximum,
      Tuple = NewTuple,
      Errors = NewErrors;

    Tuple =
      {_Blank_Spacing, _Minimum_Width, _Maximum_Columns, Page_Width, _Convert},
    Option =?= width(Width),
    integer(Width),
    Width > 8,
    we(Page_Width) :
      Page_Width = Width,
      Tuple = NewTuple,
      Errors = NewErrors;

    otherwise :
      Errors = [extra_or_invalid_option(Option) | NewErrors],
      Tuple = NewTuple.

  default_option(Value, Default) :-

    we(Value) :
      Value = Default;

    otherwise :
      Default = _,
      Value = _.

  validate_convert(String, Tuple, NewTuple, Errors, NewErrors) :-

    Tuple =
      {_Blank_Spacing, _Minimum_Width, _Maximum_Columns, _Page_Width, Convert},
    we(Convert) :
      Convert = String,
      Tuple = NewTuple|
	processor # interface(sprint(Convert, 1.0, _Display), Ok),
	convert_ok(Ok, Errors, NewErrors);

    otherwise :
      Errors = [extra_convert(String) | NewErrors],
      Tuple = NewTuple.

  convert_ok(true, Errors, Errors^).
  convert_ok(Other, [Other | Errors]^, Errors) :-
    otherwise | true.
	

measure(List, Convert, Count, Minimum, Maximum,
	Entries, Length, EntryMinWidth, EntryMaxWidth) :-

    List ? Item,
    Count += 1 :
      Entries ! String? |
	item_to_string(Item, Convert, String),
	measure_string(String?, Minimum, Minimum', Maximum, Maximum'),
	self;

    List =?= [] :
      Convert = _,
      EntryMinWidth = Minimum,
      EntryMaxWidth = Maximum,
      Entries = [],
      Length = Count;

    List =\= [_|_],
    List =\= [] :
      List' = [List] |
	self.

item_to_string(Item, Convert, String) :-

    string(Item) :
      Convert = _,
      Item = String;

    integer(Item),
    convert_to_string(Item, String') :
      Convert = _,
      String = String';

    real(Item) :
      String = String'? |
	processor#interface(sprint(Convert, Item, String'), _Ok);

    otherwise :
      Convert = _ |
	computation#display(term, Item, [width(1000000), list, copy(String)]).

measure_string(String, Minimum, NewMinimum, Maximum, NewMaximum) :-

    string_length(String, Minimum'),
      Minimum' < Minimum |
	self;

    string_length(String, Maximum'),
    Maximum' > Maximum |
	self;

    otherwise :
      String = _,
      Minimum = NewMinimum,
      Maximum = NewMaximum.


compose(Errors, Blank_Spacing, Minimum_Width, Page_Width, Entries, Length,
	MaximumWidth, MinimumColumns, MaximumColumns,
	Result
) :-

    Errors =\= [] :
      Blank_Spacing = _,
      Page_Width = _,
      Entries = _,
      Length = _,
      MaximumColumns = _,
      MaximumWidth = _,
      MinimumColumns = _,
      Minimum_Width = _,
      Result = [] |
	computation#display(stream, Errors, type(ground));

    Errors =?= [],
    MaximumWidth < Page_Width + Blank_Spacing,
    Length > 1,
    Rows := ((Length - 1) / MaximumColumns) + 1,
    MaximumRows := ((Length - 1) / MinimumColumns) + 1 |
/*
computation#display(term, (minimumrows(Rows),maximumrows(MaximumRows)), type(ground)),
*/
	arrange;

    Errors =?= [],
    otherwise :
      Blank_Spacing = _,
      Length = _,
      MaximumColumns = _,
      MaximumWidth = _,
      MinimumColumns = _,
      Minimum_Width = _,
      Page_Width = _,
      Result = Entries.


arrange(Entries, Blank_Spacing, Minimum_Width, Page_Width, Length,
	MaximumWidth, Rows, MaximumRows, Result
) :-
    Columns := (Length - 1)/Rows + 1,
    make_tuple(Columns, ColumnMaximums) :
      RowIndex = 0,
      ColumnIndex = 0,
      ColumnMaximum = 0,
      TableWidth = 0 |
/*
computation#display(term, rows(Rows), type(ground)),
*/
	try_arranging,
	iterate_arrange.


try_arranging(Entries, Blank_Spacing, Minimum_Width, Page_Width,
	Rows, Columns,
	RowIndex, ColumnIndex, ColumnMaximum, TableWidth, ColumnMaximums,
	Reply
) :-

    Entries ? Entry,
    RowIndex++ =?= 0,
    mod(RowIndex', Rows, RowIndex''),
    ColumnIndex++,
    ColumnIndex' < Columns,
    ColumnMaximum' := string_length(Entry) + Blank_Spacing :
      ColumnMaximum = _ |
	try_arranging1;

    Entries ? Entry,
    RowIndex++ =?= 0,
    mod(RowIndex', Rows, RowIndex''),
    ColumnIndex++,
    ColumnIndex' >= Columns,
    ColumnMaximum' := string_length(Entry) :
      ColumnMaximum = _ |
	try_arranging1;

    Entries ? Entry,
    RowIndex++ =\= 0,
    mod(RowIndex', Rows, RowIndex''),
    ColumnIndex < Columns |
	ColumnMaximum' := max(ColumnMaximum, length(Entry) + Blank_Spacing),
	try_arranging1;

    Entries ? Entry,
    RowIndex++ =\= 0,
    mod(RowIndex', Rows, RowIndex''),
    ColumnIndex >= Columns |
	ColumnMaximum' := max(ColumnMaximum, length(Entry)),
	try_arranging1;

    Entries =?= [],
    TableWidth += ColumnMaximum,
    TableWidth' =< Page_Width,
    arg(ColumnIndex, ColumnMaximums, CM),
    ColumnIndex++ :
      CM = ColumnMaximum,
      ColumnMaximum' = 0 |
	self;

    Entries =?= [],
    TableWidth += ColumnMaximum,
    TableWidth' > Page_Width :
      Blank_Spacing = _,
      ColumnIndex = _,
      ColumnMaximums = _,
      Columns = _,
      Minimum_Width = _,
      RowIndex = _,
      Rows = _,
      Reply = false;

    Entries =?= [],
    otherwise :
      Blank_Spacing = _,
      ColumnIndex = _,
      ColumnMaximum = _,
%      ColumnMaximums = _,
      Columns = _,
      Minimum_Width = _,
      Page_Width = _,
      RowIndex = _,
      Rows = _,
      TableWidth = _,
      Reply = true(ColumnMaximums).

try_arranging1(Entries, Blank_Spacing, Minimum_Width, Page_Width,
	Rows, Columns, 
	RowIndex, ColumnIndex, ColumnMaximum, TableWidth, ColumnMaximums,
	Reply
) :-

    RowIndex =?= 0,
    TableWidth += ColumnMaximum,
    TableWidth' =< Page_Width,
    arg(ColumnIndex, ColumnMaximums, CM) :
      CM = ColumnMaximum,
      ColumnMaximum' = 0 |
	try_arranging;

    RowIndex =?= 0,
    TableWidth += ColumnMaximum,
    TableWidth' > Page_Width :
      Blank_Spacing = _,
      ColumnIndex = _,
      ColumnMaximums = _,
      Columns = _,
      Entries = _,
      Minimum_Width = _,
      RowIndex = _,
      Rows = _,
      Reply = false;

    otherwise |
	try_arranging.


iterate_arrange(Entries, Blank_Spacing, Minimum_Width, Page_Width, Length, 
		MaximumWidth, Rows, MaximumRows, Reply, Result
) :-

    Reply =?= true(ColumnMaximums) :
      Blank_Spacing = _,
      Entries = _,
      Length = _,
      MaximumRows = _,
      Minimum_Width = _,
      Page_Width = _,
      Result = output(Entries, Rows, MaximumWidth, ColumnMaximums);

    Reply =?= false,
    Rows++ < MaximumRows |
	arrange;

    otherwise :
      Blank_Spacing = _,
      Length = _,
      MaximumRows = _,
      Minimum_Width = _,
      MaximumWidth = _,
      Page_Width = _,
      Reply = _,
      Rows = _,
      Result = Entries.



output(Result, Output_List) :-

    Result = output(Entries, Rows, MaximumWidth, ColumnMaximums),
    make_tuple(MaximumWidth, SpacesTuple),
    make_tuple(Rows, RowTuple) |
/*
computation#display(term,
	{entries(Entries), rows(Rows), maximumwidth(MaximumWidth),
			columnmaximums(ColumnMaximums)
	},
				type(ground)),
*/
	fill_spaces_tuple(MaximumWidth, _Spaces, SpacesTuple),

	fill_rowtuple,

	fill_row_list(Entries, 0, Rows, RowTuple, RowLists),

	output_list;

    otherwise :
      Output_List = Result.


  fill_spaces_tuple(Ix, Spaces, Tuple) :-

    arg(Ix, Tuple, Arg),
    Ix-- :
      ascii(" ", Space),
      Spaces ! Space |
	list_to_string(Spaces, Arg),
	self;

    otherwise :
      Ix = _,
      Tuple = _,
      Spaces = [].

  fill_spaces_tuple(Ix, Spaces, Tuple) :-

    arg(Ix, Tuple, Arg),
    Ix-- :
      ascii(" ", Space),
      Spaces ! Space,
      Arg = Spaces |
	self;

    otherwise :
      Ix = _,
      Tuple = _,
      Spaces = [].

  fill_rowtuple(Rows, RowTuple) :-

    Rows-- > 0,
    arg(Rows, RowTuple, Tuple) :
      make_channel(Tail, Head),
      Tuple = {Head, Tail} |
	self;

    otherwise :
      Rows = _,
      RowTuple = _.

fill_row_list(Entries, RowIndex, Rows, RowTuple, RowLists) :-

    Entries ? Entry,
    RowIndex++,
    mod(RowIndex', Rows, RowIndex''),
    arg(RowIndex', RowTuple, {_Head, Tail}) :
      write_channel(Entry, Tail) |
	self;

    Entries =?= [] :
      RowIndex = _,
      RowIndex' = 1 |
	close_rows.

close_rows(RowIndex, Rows, RowTuple, RowLists) :-

    RowIndex++ =< Rows,
    arg(RowIndex, RowTuple, {Head, Tail}) :
      close_channel(Tail),
      RowLists ! Head |
	self;

    RowIndex > Rows :
      RowTuple = _,
      RowLists = [].


output_list(RowLists, ColumnMaximums, SpacesTuple, Output_List) :-

    RowLists ? List,
    List =\= [] :
      Output_List ! String? |
	fill_list(List, 1, ColumnMaximums, SpacesTuple, Characters),
	list_to_string(Characters, String),
	self;

    RowLists ? List,
    List =?= [] |
	self;

    RowLists =?= [] :
      ColumnMaximums = _,
      SpacesTuple = _,
      Output_List = [].

  fill_list(List, Index, Maximums, SpacesTuple, Characters) :-

    List ? String,
    Index++ < arity(Maximums),
    string_to_dlist(String, Chars, Spaces),
    arg(Index, Maximums, Max),
    Extra := Max - string_length(String),
    arg(Extra, SpacesTuple, SpaceString),
    string_to_dlist(SpaceString, Spaces, Characters') :
      Characters = Chars |
	self;

    List ? String,
    Index >= arity(Maximums),
    string_to_dlist(String, Chars, Characters') :
      Characters = Chars |
	self;

    List =?= [] :
      Index = _,
      Maximums = _,
      SpacesTuple = _,
      Characters = [].
