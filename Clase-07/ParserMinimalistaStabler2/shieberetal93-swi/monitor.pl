/*----------------------------------------------------------
                         MONITORING
----------------------------------------------------------*/


%%% verbose
%%% =======
%%%
%%%     Predicate governs the degree of verbosity in the
%%%     notifications. 

:- dynamic vverbose/0.
:- dynamic verbose/0.

% choose a level of verbosity (and leave dynamic declarations above)
%vverbose. % comment to reduce verbosity of chart construction
%verbose. % comment to reduce verbosity of chart construction even more

%%% notify_...(...)
%%% ===============
%%%
%%%     Prints debugging information if the flag verbose/0
%%%     is true. 

notify_consequence(_RuleName, _Trigger, _Others, _SideConds, _Consequent) :- !.
notify_consequence(RuleName, Trigger, Others, SideConds, Consequent) :-
	( verbose ->
	    format("~p:~n    trigger: ~p~n", 
		   [RuleName, Trigger]),
	    format("   others: ~p~n", [Others]),
	    format("   side conds: ~p~n", [SideConds]),
	    format("   cons:    ~p~n", [Consequent])
	;   true
	).

%:- use_module(library(system),[system/1]).

%notify_agenda_addition(_Item) :- !.
notify_agenda_addition(Item) :-
	(   vverbose ->  (format('~NAdding to agenda: <-> ~p~n', [Item]))
	;   verbose ->  (print('.'), ttyflush)
	;   true
	).

%notify_chart_addition(_Index) :- !.
notify_chart_addition(Index) :-	
	index_to_item(Index, Item),
	item_to_key(Item, Key),
	( vverbose  ->	(format('~NAdding to chart: <~p> ~p~n',	[Key,Item])) 
	;   verbose ->  (print(':'), ttyflush)
	;   true
	).
