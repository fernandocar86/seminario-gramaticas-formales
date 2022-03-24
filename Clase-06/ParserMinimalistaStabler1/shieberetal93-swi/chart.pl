/*..........................................................
                           CHART
..........................................................*/

%%% init_chart
%%% ================
%%%
%%%     Remove any bits of (agenda or) chart clauses and
%%%     associated keys from the Prolog database. 

init_chart :-
    retractall(stored(_,_)),
    retractall(key_index(_,_)).


%%% item_in_chart(?Item, +RefIndex)
%%% ===============================
%%%
%%%     Retrieve a stored item matching Item.  RefIndex is a
%%%     reference index that distinguishes items in the
%%%     chart (at or before the reference index) from those
%%%     in the agenda (after the reference index).  It is
%%%     the index of the item in the chart with the largest
%%%     index.

item_in_chart(Item, RefIndex) :-
    item_stored(Item, ItemIndex),
    (ItemIndex =< RefIndex 
        %% Item is at or before reference, so it is in the
        %% chart 
        -> true
        %% Item is after reference, so it AND ALL LATER
        %% ITEMS are in the agenda, so stop looking for
        %% other chart items.
        ;  !, fail).


%%% item_in_chart(?Item)
%%% ====================
%%%
%%%     Item is an item in the chart generated after agenda
%%%     is exhausted (so there is no reference index
%%%     pointing to the end of the chart, and all stored
%%%     items are chart items). 

item_in_chart(Item) :-
    item_stored(Item, _).


%%% add_item_to_chart(Index)
%%% ========================
%%%
%%%     Add the item stored at Index in the stored items to
%%%     the chart.  (Nothing need be done, since moving on
%%%     to the next agenda item changes the reference index,
%%%     thereby implicitly making the item a chart item, so
%%%     we just print debugging information.)  

add_item_to_chart(Index) :-
    notify_chart_addition(Index).
