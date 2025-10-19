/*
** This module is part of EFCP.
**

     Copyright 2007 Eitan Shterenbaum
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
 *  Handles appropriate output of the block-module, according to the
 *  options.
 *
 *  The types used in this module are defined in module
 *  super.
 */

-export([pretty/3]).
-language(compound).
%-mode(trust).

/******* PRETTY **************************************************************/

procedure pretty(ServiceId, BlockedSource, OptionsTuple).

% Waits till the 1st and 3rd argument of the OptionsTuple are
% instantiated, than acts according to these options, possibly
% pretty-printing and writing the block-module.

pretty(RootId, BlockedSource, OptionsTuple) :-

    OptionsTuple = {no_name, _, no_text, _, _, Residue\Residue'},
    RootId ? Name |
	"Logix_Users" # pretty # module(BlockedSource, Residue'),
	write(RootId', Name, Residue);

    OptionsTuple = {_, _, text(Residue), _, _, Residue\Residue'} :
      RootId = _ |
	"Logix_Users" # pretty # module(BlockedSource, Residue');

    OptionsTuple = {name(Name), _, no_text, _, Residue\Residue'},
    RootId ? _ |
	"Logix_Users" # pretty # module(BlockedSource, Residue'),
	write(RootId', Name, Residue);

    otherwise,
    OptionsTuple = {_, _, Text, _, _, _} : RootId = _, BlockedSource = _ |
	unify_without_failure(Text, text([])).

/******* WRITE ***************************************************************/


procedure write(ServiceId, Name, [String]).

% Creates a file Name.cp in Context, and writes the
% strings of list Strings into it.

write(NodeId, Name, Strings) :-
	auxils # append_strings(Name, '.cp', Name_cp),
	file # execute_in_context(NodeId,
				  put_file(Name_cp, Strings, [], _)
			).

/******* (END OF MODULE) *****************************************************/
