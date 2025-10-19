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
This is my little hash table emulator.
To be substituted with an efficient version when necessary.


Re-written using inheritence.

*/

-language([inherit,dfcp]).
-mode(interrupt).

Hash ::= [Item].
Item ::= Key-Value.

Key ::= String.
Value ::= Any.

create(Hash) :-
	Hash=[].


search(Key,Hash,Hash1) :-
	Hash ? Key1-Value1, Key=\=Key1 |
	Hash1 ! Key1? - Value1?, self.


insert(Key,Value,Hash,Hash1,Ok) :-
	+search;

	Hash ? Key1-Value1, Key=Key1 |
		Hash1 = [Key1-Value1 | Hash'?],
		Ok=false([
	'Cannot insert since item with key already exists. ',Key-Value]);  

	Hash=[] | 
		Hash1=[Key-Value],
		Ok=true.


find(Key,Hash,Hash1,Ok) :-
	+search;

	Hash=[] | 
		Hash1=[],
		Ok=false(['Item with key not found. ',Key]).



check(Key,Hash,Hash1,Ok) :-
	+find;
	Hash ? Key-Value, ground(Key) |
		Hash1 ! Key - Value?, 
		Hash1' = Hash',
		Ok = true.


lookup(Key,Value1,Hash,Hash1,Ok) :-
	+find;
	Hash ? Key-Value, ground(Key), listener(Value) |
		Hash1 ! Key - Value, 
		Value1 = Value,
		Hash1' = Hash',
		Ok = true.


replace(Key,Value,NewValue,Hash,Hash1,Ok) :-
	+ search;

	Hash ? Key1-Value1, Key=Key1 |
		Value=Value1?,
		Hash1=[Key-NewValue|Hash'?],
		Ok=true;

	Hash=[] | 
		Hash1=[],
		Value = _,
		Ok=false(['Item with key not found. ',Key-NewValue]).

replace_or_insert(Key,Value,NewValue,Hash,Hash1,Ok) :-
	+ search;

	Hash ? Key1-Value1, Key=Key1 |
		Value=Value1?,
		Hash1=[Key-NewValue|Hash'?],
		Ok=true;

	Hash=[] | 
		Hash1=[Key-NewValue],
		Value = [],
		Ok=true.

delete(Key,Value,Hash,Hash1,Ok) :-
	+find;

	Hash ? Key1-Value1, Key=Key1 |
		Value=Value1,
		Hash1=Hash',
		Ok=true.


retrieve_keys(Hash,Keys,Hash1) :-
	Hash=[] | Keys=[], Hash1=[];

	Hash ? Key-Value, ground(Key) | 
		Keys ! Key,	
		Hash1 ! Key-Value?,
		retrieve_keys.


transform_values(Entries,Entries1,Values,Values1) :-
	Entries = [] | Entries1 = [], Values = [], ask(Values1,[]);
	
	Entries ? Key - Value |
		Values ! Value,  
		transform_values1(Key,Entries',Entries1,Values',Values1).

transform_values1(Key,Entries,Entries1,Values,Values1) :-
	Values1 ? Value |
		Entries1 ! Key-Value,
		transform_values(Entries,Entries1',Values,Values1').
		


ask(X,Y) :- X=Y | true.

tail_and_update(Key, Value, NewValue, Hash, Hash1, Ok) + (Found):-
  +search;

  initially |
	Found = false;

  Hash ? Key1-Value1, Key=Key1 |
	Found = _, 
	Found' = true(Value1),
	self;

  Hash = [], Found = true(Value1) |
	Hash1 = [Key - NewValue],
	Value = Value1,
	Ok = true;

  Hash = [], Found =\= true(_) |
	Hash1 = [],
	Value = undefined,
	NewValue = _,
	Ok = false(['Item with key not found. ',Key]).
	
