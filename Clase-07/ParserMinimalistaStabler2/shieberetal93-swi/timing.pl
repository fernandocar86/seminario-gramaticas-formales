%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -*- Mode: Prolog -*- %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% timing.pl --- 
%% Author          : Frank Morawietz
%% Created On      : Tue Oct 26 14:55:44 1999
%% Last Modified By: Frank Morawietz
%% Last Modified On: Tue Oct 26 14:56:05 1999
%% Update Count    : 1
%% Status          : Unknown, Use with caution!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%% Timing predicates %%%%%%%%%%%%%%%%%%%%%%%%%

% time(+Goal)
% write the CPU time used to the screen
time(Goal) :-
	cpu_time(Goal,Time),
	nl,write('Time used (sec): '),write(Time),nl,nl.
% time(+Repetitions,+Goal)
% write the CPU time used to the screen
time(N,Goal) :-
	cpu_time(N,Goal,Time),
	nl,write('Time used (sec): '),write(Time),nl,nl.

% cpu_time/2: from O'Keefe
% cpu_time(+Goal,-Time)
cpu_time(Goal,Duration) :-
	statistics(runtime,[Start|_]),
	( call(Goal) -> 
	    (nl,write('Goal succeeded'),nl,true)
	;
	    (nl,write('Goal failed'),nl,true) 
	),
	statistics(runtime,[Finish|_]),
	Duration is (Finish - Start)*0.001.
% cpu_time(+Repetitions,+Goal,-Time)
cpu_time(N,Goal,Duration) :-
	statistics(runtime,[T0|_]),
	( call((repeat(N),(Goal ->fail))) ; true),
	statistics(runtime,[T1|_]),
	( call((repeat(N),(true ->fail))) ; true),
	statistics(runtime,[T2|_]),
	Duration is ((T1 - T0) - (T2 - T1))*0.001,
	Goal.

% cpu_time_fail(+Repetitions,+Goal,-Time)
cpu_time_fail(N,Goal,Duration) :-
	statistics(runtime,[T0|_]),
	( call((repeat(N),(Goal ->fail))) ; true),
	statistics(runtime,[T1|_]),
	( call((repeat(N),(true ->fail))) ; true),
	statistics(runtime,[T2|_]),
	Duration is ((T1 - T0) - (T2 - T1))*0.001.

% repeat(+Num)
repeat(N):-
	integer(N),N > 0,
	repeat_1(N).

% repeat_1(+Num)
repeat_1(1):-!.
repeat_1(_).
repeat_1(N):-
	M is N - 1,
	repeat_1(M).

