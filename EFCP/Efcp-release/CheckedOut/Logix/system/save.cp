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
       09-07-85
*/

-mode(trust).
-export([save/3, quiet/2]).
-language(compound).


procedure save(String, String, done).
procedure quiet(String, done).


save(FileName, Announcement, Ready) :-
    true : Ready = Done? |
	processor # interface(date_time(_, Date, Time)),
	save_announcement(FileName, Announcement, Date, Time, Done).


save_announcement(FileName, Announcement, Date, Time, Done) :-
    known(Date),
    known(Announcement) |
	processor # machine(idle_queue(Idle, 10)),
	annunciate(Idle, FileName, Announcement, Date, Time, Done).
	

annunciate(Idle, FileName, Announcement, SaveDate, SaveTime, Done) :-
    known(Idle) |
	prepare_save(FileName, Done, Saved),
	restart_date_time(Saved, Date, Time),
	computation # display(term,
		['
',		 'Emulated Flat Concurrent Prolog ', SaveDate - SaveTime, '
'		],
      			[list, known(Saved), close(done, D)]
	),
	computation # display(term,
		[
		 Announcement, ' Enquire about "license", "warranty" !
'		],
      			[list, known(D), close(done, Announced)]
	),
	computation # display(term, Date - Time,
			      [known(Announced), known(Time),
			       close(Saved, Done)]
		 ).

restart_date_time(Saved, Date, Time) :-
    known(Saved) |
	processor # interface(date_time(_, Date, Time)).


quiet(FileName, Ready) :-
    string(FileName) : Ready = Done? |
	prepare_save(FileName, Done, Done).


prepare_save(FileName, Done, Saved) :-
	processor # machine(idle_queue(Idle, 10)),
	save_system(Idle, FileName, Answer),
	processor # machine(boot_wait(Booted)),
	check_answer(Answer, Booted, FileName, Saved),
	start_saved_system(Done, Booted).

check_answer(Answer, Booted, FileName, Saved) :-

    Answer = done : Booted = _, FileName = _,
      Saved = done ;

    Answer =\= done : Booted = _,
      Saved = done |
	fail(save(FileName), Answer);

    known(Booted) : Answer = _, FileName = _,
      Saved = done .

save_system(Idle1, FileName, Answer) :-
    known(Idle1) |
	processor # machine(request(save_state(FileName), Answer)).


start_saved_system(Done, Restarted) :-

    known(Done),
    known(Restarted) |
	processor # device(restart),
	super # '_close'(save, _);

    known(Done),
    unknown(Restarted) |
	super # '_close'(save, _).
