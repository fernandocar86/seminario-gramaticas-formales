%   File   : closure.pl
%   Author : E. Stabler
%   Updated: 16 October 1999
%   Purpose: this is an interface to the chart mechanism of 
%            Shieber, Schabes & Periera (1993)
%            This paper and source code are available at
%            http://xxx.lanl.gov/comp-lg

% The following load commands assume that the Shieber et al (1993) source
%  code has been placed in a subdirectory called: shieberetal93

:- ensure_loaded('shieberetal93-swi/chart.pl'). 
:- ensure_loaded('shieberetal93-swi/agenda.pl'). 
:- ensure_loaded('shieberetal93-swi/items.pl'). 
:- ensure_loaded('shieberetal93-swi/monitor.pl'). 
:- ensure_loaded('shieberetal93-swi/driver'). 
:- ensure_loaded('shieberetal93-swi/utilities'). 

closure(InitialSet) :-           % computes closure in stored/2
	init_chart,
	empty_agenda(Empty),
	add_items_to_agenda(InitialSet, Empty, Agenda),
	exhaust(Agenda).

closure(InitialSet,Closure) :-   % computes closure in stored/2 and returns it
	init_chart,
	empty_agenda(Empty),
	add_items_to_agenda(InitialSet, Empty, Agenda),
	exhaust(Agenda),
	setof(Member,Index^stored(Index,Member),Closure).

% item_to_key/2 should be specialized for the relations being computed
item_to_key(_:[[A0,A1,A2,A3,A4,A5,_Ancestor]:[F|_Fs]|_Chains0],Hash) :- !, hash_term(x(A0,A2,A4,F,A1,A3,A5,F), Hash).
%item_to_key(_:[[A0,_A1,A2,_A3,A4,_A5,_Ancestor]:[F|_Fs]|_Chains0],Hash) :- !, hash_term(x(A0,A2,A4,F), Hash).
item_to_key(F, Hash) :- hash_term(F, Hash).
