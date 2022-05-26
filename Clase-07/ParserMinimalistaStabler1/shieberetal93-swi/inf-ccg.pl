/*----------------------------------------------------------

             Parsing Algorithm Inference System

      Bottom-Up Combinatory Categorial Grammar Parsing

----------------------------------------------------------*/

/*----------------------------------------------------------
                          ENCODING
----------------------------------------------------------*/

%%% item(Cat, I, J, Deriv)
%%% =================================================
%%%     Cat      is a CCG category
%%%     I, J     are the two indices into the string
%%%     Deriv    is the derivation of the item


initial_item(item(Cat,I,J1, [Cat,Word])) :-
        word(J1, Word),
        lex(Word,Cat),
        I is J1 - 1.


final_item(item(Start,0, Length,D), D) :-
    startsymbol(Start),
    sentencelength(Length).

/*----------------------------------------------------------
                       ITEM INDEXING
----------------------------------------------------------*/


%%% item_hash(Item, Index)
%%% ======================

%% Temporarily disabled.

item_hash(_Item, index).


/*----------------------------------------------------------
                           INFERENCE RULES
----------------------------------------------------------*/

%%%.........................................................
%%% FORWARD APPLICATION:

inference(  forward-application,
            [  item(X+Y, I, J, D1), item(Y, J, K, D2)  ],
%           -------------------------------------
            item(X,I,K,[D1, D2]),
%   where
            []        ).

%%%.........................................................
%%% BACKWARD APPLICATION:

inference(  backward-application,
            [  item(Y, I, J, D1), item(X-Y, J, K, D2)  ],
%           -------------------------------------
            item(X,I,K, [D1, D2]),
%   where
            []        ).

%%%.........................................................
%%% FORWARD COMPOSITION 1:

inference(  forward-composition1,
            [  item(X+Y, I, J, D1), item(Y+Z, J, K, D2)  ],
%           -------------------------------------
            item(X+Z,I,K, [D1, D2]),
%   where
            []        ).


%%%.........................................................
%%% FORWARD COMPOSITION 2:

inference(  forward-composition1,
            [  item(X+Y, I, J, D1), item(Y-Z, J, K, D2)  ],
%           -------------------------------------
            item(X-Z,I,K, [D1, D2]),
%   where
            []        ).


%%%.........................................................
%%% BACKWARD COMPOSITION 1:

inference(  backward-composition1,
            [  item(Y+Z, I, J, D1), item(X-Y, J, K, D2)  ],
%           -------------------------------------
            item(X+Z,I,K, [D1, D2]),
%   where
            []        ).


%%%.........................................................
%%% BACKWARD COMPOSITION 2:

inference(  backward-composition2,
            [  item(Y-Z, I, J, D1), item(X-Y, J, K, D2)  ],
%           -------------------------------------
            item(X-Z,I,K, [D1, D2]),
%   where
            []        ).

