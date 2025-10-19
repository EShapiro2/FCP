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

-language(dfcp).
-monitor(serve).
-include([api_includes,server_includes]).
-mode(user).
-scope(server).


serve(In) + (File, WhatToLog, FileStream) :-

  finally |
	FileStream = [];

  In ? log_file(File'), string(File') |
	File = _,
	self;

  In ? log(WhatToLog'), ground(WhatToLog') |
	WhatToLog = _,
	self;

  In ? start, string(File), ground(WhatToLog) |
	FileStream = _,
	file#put_file(File,FileStream'?,append,Ok),
        ok(Ok?, logging_file),
	self;

  In ? start, unknown(File) |
	computation#display("Cannot start logging - Log file name not defined"),
	self;

  In ? start, unknown(WhatToLog) |
	computation#display("Cannot start logging - Event types not defined"),
	self;
	
  In ? log_event(URL, PresenceId, Date, EventCategory, EventType,
		To, Contents, RecipientsList), 
  string(URL), constant(EventCategory), string(Date), ground(Contents),
  ground(RecipientsList), ground(WhatToLog) |
	codes#category_code_to_string(EventCategory, EventCategoryString),
	codes#event_code_to_string(EventCategory, EventType, EventTypeString),
	destination_code_to_recipients(To, RecipientsList, R),
	processor#interface(gmt2date(Date, Date')),
	LoggedEvent = [URL, PresenceId, R?, Date'?,
		EventCategoryString?, EventTypeString?, Contents],
	member(EventCategory, WhatToLog, Ok),
        In'' = [do_log_event(Ok?, LoggedEvent?) | In'?],
	self;

  In ? do_log_event(false, _) |
        self;

  In ? do_log_event(true, LoggedEvent), ground(LoggedEvent) |
        "Logix_Users" # pretty # module([LoggedEvent],Strings),
        FileStream!Strings?,
        self;

  In ? terminate(Ok) |
	FileStream' = [], FileStream = _, In' = _,
        In'' = [set_termination(Ok)],
        self;

  In ? set_termination(Ok) |
	File = _, WhatToLog = _, FileStream = _, In' = _,
        Ok = true;

  In = [], writable(FileStream) |
	File = _, WhatToLog = _,
	FileStream = [];

  In = [], FileStream = [] |
	File = _, WhatToLog = _.


member(Elem, List, Ok) :-

    List = all | Ok = true, Elem = _;
    List ? Elem  | Ok = true, List' = _;
    List = [] | Ok = false, Elem = _;
    List ? NotElem, NotElem =\= Elem | self.

destination_code_to_recipients(To, RecipientsList, R) :-
  To = DOORS_LIST |
	R = RecipientsList;

  To = DOORS_CONVERSATION |
	R = DOORS_CONVERSATION_STRING,
	RecipientsList = _;

  To = DOORS_SERVER |
	R = DOORS_SERVER_STRING,
	RecipientsList = _;

  To = DOORS_PLACE |
	R = DOORS_PLACE_STRING,
	RecipientsList = _.
