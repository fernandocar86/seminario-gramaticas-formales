/*----------------------------------------------------------

             Parsing Algorithm Inference System

                   Pure Bottom-Up Parsing

----------------------------------------------------------*/


:- op(1200,xfx,--->).


/*----------------------------------------------------------
                       ITEM ENCODING
----------------------------------------------------------*/

%%% item(BeforeDot, I)
%%% ==================
%%%              r       
%%%     BeforeDot  is a list of nonterminals and terminals
%%%     that have been found from the start of the string
%%%     through position I.  Note that the stack of parsed
%%%     constituents is kept in reversed order, with the
%%%     most recently parsed at the left edge of the list.

initial_item(item([], 0)).

final_item(item([Value], Length), Value) :-
    sentencelength(Length),
    startsymbol(Value).


/*----------------------------------------------------------
                       ITEM INDEXING
----------------------------------------------------------*/

%%% item_to_key(+Item, -Index)
%%% ==========================
%%%
%%%     Items are indexed by position and category of first
%%%     constituent. 

item_to_key(item([First|_], I), Index) :-
    First =.. [Firstcat|_],
    hash_term(a(I,Firstcat), Index).

item_to_key(item([], I), Index) :-
    hash_term(a(I,none), Index).
    

/*----------------------------------------------------------
                      INFERENCE RULES
----------------------------------------------------------*/

%%%.........................................................
%%% SHIFT:

inference(  shift,
            [  item(Beta, I)  ],
%           -------------------------------
            item([B|Beta], I1),
%   where
            [I1 is I + 1, 
             word(I1, Bterm), 
             lex(Bterm, B)]                     ).

%%%.........................................................
%%% REDUCE:

inference(  reduce,
            [  item(BetaAlpha, I)  ],
%           --------------------------------
            item([A|Alpha], I),
%   where
            [(A ---> Beta), 
             reverse(Beta, BetaR), 
             append(BetaR, Alpha, BetaAlpha)]   ).
