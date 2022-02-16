/*----------------------------------------------------------

             Parsing Algorithm Inference System

                   Pure Top-Down Parsing

----------------------------------------------------------*/


:- op(1200,xfx,--->).


/*----------------------------------------------------------
                       ITEM ENCODING
----------------------------------------------------------*/

%%% item(AfterDot, I, Value)
%%% ========================
%%%                     
%%%     AfterDot is a list of nonterminals and terminals
%%%     that need to be found from position I in the string
%%%     to the end of the string.  Value is some term that
%%%     is passed around among all the items to be returned
%%%     as the final value associated with the parse.  It is
%%%     seeded to be the Start category of the parse, but
%%%     may become further instantiated as the parse
%%%     progresses.  

initial_item(item([Start], 0, Start)) :-
    startsymbol(Start).

final_item(item([], Length, Value), Value) :-
    sentencelength(Length).


/*----------------------------------------------------------
                       ITEM INDEXING
----------------------------------------------------------*/

%%% item_to_key(+Item, -Index)
%%% ==========================
%%%
%%%     Items are indexed by position and category of first
%%%     constituent. 

item_to_key(item([First|_], I, _Value), Index) :-
    First =.. [Firstcat|_],
    hash_term(a(I,Firstcat), Index).

item_to_key(item([], I, _Value), Index) :-
    hash_term(a(I,none), Index).
    

/*----------------------------------------------------------
                      INFERENCE RULES
----------------------------------------------------------*/

%%%.........................................................
%%% SCANNER:

inference(  scanner,
            [  item([B|Beta], I, Value)  ],
%           -------------------------------
            item(Beta, I1, Value),
%   where
            [I1 is I + 1, 
             word(I1, Bterm), 
             lex(Bterm, B)] ).

%%%.........................................................
%%% PREDICTOR:

inference(  predictor,
            [  item([A|Alpha], I, Value)  ],
%           --------------------------------
            item(BetaAlpha, I, Value),
%   where
            [(A ---> Beta), 
             append(Beta, Alpha, BetaAlpha)]).

