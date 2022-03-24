/*----------------------------------------------------------
                      INFERENCE RULES
------------------------------------------------------------

Parsing algorithms are specified as an inference system.
This includes a definition of a class of items, and some
inference rules over those items.  Subsets corresponding to
initial items and final items are also defined.

The following predicates are used in defining an inference
system:

%%% initial_item(Item)  Item is an initial item.
%%% ==================

%%% final_item(Value)   Value is the pertinent information
%%% =================   to return about some final item.

%%% inference(RuleName, Antecedent, 
%%%                     Consequent, SideConditions)
%%% ===============================================
%%%
%%%     Specifies an inference rule named RuleName with
%%%     Antecedent items, a Consequent item, and some
%%%     SideConditions. 

The following predicate is used to define appropriate
indexing of the items:

%%% item_to_key(+Item, -Key)
%%% ========================
%%%
%%%     Key is a hash key to associate with the given Item.
%%%     The item will be stored in the Prolog database under
%%%     that key. 

Definitions of these predicates can be found in the
following files: 
*/

%:- ensure_loaded('inf-top-down.pl').
%:- ensure_loaded('inf-bottom-up.pl').
:- ensure_loaded('inf-earley.pl').
%- ensure_loaded('inf-ccg.pl').


%%% initial_items(-Items)
%%% =====================
%%%
%%%     Items are the initial items of the inference system.

initial_items(Items) :-
    all_solutions(Item, initial_item(Item), Items).
