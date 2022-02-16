/*----------------------------------------------------------

             Parsing Algorithm Inference System

                     Earley's Algorithm

----------------------------------------------------------*/


:- op(1200,xfx,--->).

:- ensure_loaded(strings).


/*----------------------------------------------------------
                       ITEM ENCODING
----------------------------------------------------------*/

%%% item(LHS, BeforeDot, AfterDot, I, J)
%%% ====================================
%%%                     r
%%%     LHS -> BeforeDot  AfterDot                 r
%%%             is a DCG production where BeforeDot  is the
%%%             reversal of BeforeDot 
%%%     I, J    are the two indices into the string

initial_item(item('<start>', [], [Start], 0,0)) :-
    startsymbol(Start).

final_item(item('<start>', [Start], [], 0, Length), 
           Start) :- 
    startsymbol(Start),
    sentencelength(Length).


/*----------------------------------------------------------
                       ITEM INDEXING
----------------------------------------------------------*/

%%% item_to_key(+Item, -Key)
%%% ========================

%% Active edges are indexed by the category of the
%% constituent after the dot and the starting position of
%% that constituent. 
item_to_key(item(_A, _Alpha, [B|_Beta], _I, J), Hash) :-
    B =.. [Bcat|_],
    hash_term(a(J,Bcat), Hash).

%% Passive edges (with nothing after the dot) are indexed by
%% the category of the parent constituent, and the starting
%% position of that constituent. 
item_to_key(item(A, _Alpha, [], I, _J), Hash) :-
    A =.. [Acat|_],
    hash_term(p(I,Acat), Hash).


/*----------------------------------------------------------
                      INFERENCE RULES
----------------------------------------------------------*/

%%%.........................................................
%%% SCANNER:

inference(  scanner,
            [  item(A, Alpha, [B|Beta], I, J)  ],
%           -------------------------------------
            item(A, [B|Alpha], Beta, I, J1),
%   where
            [J1 is J + 1, 
             word(J1, Bterm), 
             lex(Bterm, B)]                           ).

%%%.........................................................
%%% PREDICTOR:

inference(  predictor,
            [  item(_A, _Alpha, [B|_Beta], _I,J)  ],
%           ----------------------------------------
            item(B, [], Gamma, J,J),
%   where
            [(B ---> Gamma)]                          ).
    
%%%.........................................................
%%% COMPLETOR:

%%% Type 1 and 2 Completor
inference(  completor,
            [  item(A, Alpha, [B|Beta], I,J),
               item(B, _Gamma, [], J,K)   ],
%           --------------------------------
            item(A, [B|Alpha], Beta, I,K),
%   where
            []                                        ).
