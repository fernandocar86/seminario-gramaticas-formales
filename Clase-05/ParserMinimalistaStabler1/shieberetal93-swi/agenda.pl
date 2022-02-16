/*..........................................................
                           AGENDA
..........................................................*/


%%% is_empty_agenda(+Agenda)
%%% ========================
%%%
%%%     Holds if Agenda represents an empty agenda.

is_empty_agenda(queue(Front, Back)) :-
    Front >= Back.


%%% empty_agenda(-Agenda)
%%% =====================
%%%
%%%     Agenda is a new empty agenda.

empty_agenda(queue(0, 0)).


%%% pop_agenda(+Agenda, -Index, -NewAgenda)
%%% ======================================
%%%
%%%     Index is the top Index in the Agenda, NewAgenda is
%%%     the Agenda with that item removed.

pop_agenda(queue(Front, Back), 
           Front, queue(NewFront, Back)) :-
    Front < Back,
    NewFront is Front + 1.


%%% add_item_to_agenda(+Item, +Agenda0, -Agenda)
%%% ============================================
%%%
%%%     Add the index corresponding to Item to Agenda0,
%%%     yielding Agenda.  This stores the appropriate items
%%%     in the Prolog database.  Note that the stored/2
%%%     clause must be asserted at the end of the database
%%%     (even though the index numbering provides ordering
%%%     information already) to allow early cut off of
%%%     searches in item_in_chart/2 (q.v.). 

add_item_to_agenda(Item, queue(Front, Back), 
                   queue(Front, NewBack)) :-
    notify_agenda_addition(Item),
    (\+ subsumed_item(Item)
    -> (assertz(stored(Back, Item)),
        item_to_key(Item, Key),
        assert(key_index(Key, Back)),
        NewBack is Back + 1)
    ; NewBack = Back).


%%% add_items_to_agenda(+Items, +Agenda0, -Agenda)
%%% ==============================================
%%%
%%%     Add indices corresponding to all of the Items to
%%%     Agenda0 yielding Agenda.

add_items_to_agenda([], Agenda, Agenda).
add_items_to_agenda([Item|Items], Agenda0, Agenda) :-
    add_item_to_agenda(Item, Agenda0, Agenda1),
    add_items_to_agenda(Items, Agenda1, Agenda).


%%% index_to_item(Index, Item)
%%% ==========================
%%%
%%%     Item is the actual stored item for Index.

index_to_item(Index, Item) :-
    stored(Index, Item).

