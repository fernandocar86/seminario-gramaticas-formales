/*----------------------------------------------------------
                        STORED ITEMS
----------------------------------------------------------*/

%%% stored(Index, Item)
%%% ===================
%%%
%%%     Predicate used to store agenda and chart Items in
%%%     the Prolog database along with a unique identifying
%%%     Index, assigned in numerical order.

:- dynamic stored/2.


%%% key_index(Key, Index)
%%% =====================
%%%
%%%     Predicate used to store an auxiliary indexing table
%%%     for indexing stored items.  The predicate
%%%     item_to_key/2 is used to compute the key for an
%%%     item.

:- dynamic key_index/2.


%%% item_stored(+Item, -Index)
%%% ==========================
%%%
%%%     Finds a stored Item amd its Index in the sequence of
%%%     stored items. 

item_stored(Item, Index) :-
    item_to_key(Item, Key),
    key_index(Key, Index),
    stored(Index, Item).


%%% similar_item(+Item, -StoredItem)
%%% ================================
%%%
%%%     Find a stored item StoredItem in the stored items
%%%     that might subsume Item. 

similar_item(Item, SimilarItem) :-
    item_to_key(Item, Key),
    key_index(Key, IndexofSimilar),
    stored(IndexofSimilar, SimilarItem).


%%% subsumed_item(+Item)
%%% ====================
%%%     Item is subsumed by some stored item.

subsumed_item(Item) :-
    similar_item(Item, OtherItem),
    subsumes(OtherItem, Item).

/*..........................................................
                      CHART and AGENDA
..........................................................*/

:- ensure_loaded(chart).
:- ensure_loaded(agenda).
