/*----------------------------------------------------------------------
		4. Earley-Style TAG Parsing Algorithm
----------------------------------------------------------------------*/


/*----------------------------------------------------------------------
                           GRAMMAR ENCODING
------------------------------------------------------------------------

Tree node encodings:

We assume that nodes in the trees are encoded as terms of the form
Tree/Address where Tree specifies the tree the node is in and Address
specifies the tree address of the node in that tree.  A special case
is the node nil, which is the start node of the whole grammar.  It is
considered to be the root node if tree nil.

Grammar rule encodings:

Nonterminals are encoded as nodes plus a specification of top versus
bottom, given as TB-Node where TB specifies top (t) or bottom (b), and
Node is the node.

Terminals are encoded as constants.

Grammar rules are encoded with unit clauses of the predicate rule/2.

LHS ---> RHS            LHS (a nonterminal) and RHS (a list of
                        nonterminals and terminals) are
                        respectively the left- and right-hand
                        side of a rule.   

In addition, we require specifications of which nodes are root and
foot of auxiliary trees, and root of initial trees, and which nodes
are the start nodes of the grammar. These are given by:

initroot(Node)          Node is the root node of an initial tree
auxroot(Node)           Node is the root noe of an auxiliary tree
auxfoot(Node)  		Node is the foot node of an auxiliary tree

Modifier and predicative trees are distinguished with the predicates

modifier(Tree)		Tree is a modifier tree
predicative(Tree)	Tree is a predicative tree

*/

:- op(1200,xfx,--->).

/*----------------------------------------------------------------------
                       INFERENCE RULE UTILITIES
----------------------------------------------------------------------*/

%%% tree(Node, Tree)
%%% ================
%%% 
%%%     Node is a node in the tree Tree.  (As a special case, the node
%%%     nil is considered to be a part of the tree nil.)

tree(nil, nil).    
tree(T/_A, T).     


%%% addr(Node, Address)
%%% ===================
%%%
%%%     Node is at address Address in its tree.  (As a special case,
%%%     the node nil is considered to be the root node of the tree
%%%     nil.) 

addr(nil, 0).
addr(_T/A, A).


%%% combine(+I, +J, -K)
%%% ===================
%%%
%%%     Implements the U operator from Schabes 1991, where the
%%%     constant x is the don't care index value.

combine(I, x, I) :- !.
combine(x, I, I) :- !.
combine(I, I, I).
                         


/*----------------------------------------------------------------------
			    ITEM ENCODING
----------------------------------------------------------------------*/

%%% item(LHS, BeforeDot, AfterDot, I, J, K, L, Deriv)
%%% =================================================
%%%                     r
%%%     LHS -> BeforeDot  AfterDot                         r
%%%                     is a DCG production where BeforeDot  is the
%%%                     reversal of BeforeDot 
%%%     I, J, K, L      are the four indices into the string
%%%     Deriv		is the derivation of the item

initial_item(item(Alpha, [], t, I,x,x,s(I))) :-
    initroot(InitRoot).

final_item(item(t-nil, [t-Start], [], 0,x,x,Length, Deriv), Deriv) :-
    startnode(Start),
    sentencelength(Length).


/*----------------------------------------------------------------------
			    ITEM INDEXING
----------------------------------------------------------------------*/


%%% item_to_key(+Item, -Index)
%%% ==========================

%% Temporarily disabled.

item_to_key(_Item, index).


/*----------------------------------------------------------------------
                           INFERENCE RULES
----------------------------------------------------------------------*/


%%% inference(RuleName, Antecedent, Consequent, SideConditions)
%%% ===========================================================
%%%
%%%     Specifies an inference rule named RuleName with Antecedent
%%%     items, a Consequent item, and some SideConditions.
%%%
%%%     The particular inference rules are those of Schabes and
%%%     Shieber, 1991, "An Alternative Conception of Tree-Adjoining
%%%     Derivation".

%%%---------------------------------------------------------------------
%%% SCANNER:

inference(  scanner,
            [  item(b-Eta, Gamma, [A|Delta], I, J, K, L, S)  ],
%           ---------------------------------------------------
            item(b-Eta, [A|Gamma], Delta, I, J, K, L1, S),
%   where
            [L1 is L + 1, word(L1, A)]).

%%%---------------------------------------------------------------------
%%% PREDICTOR:

inference(  predictor,
            [  item(_Pos-_Eta, _Gamma, [Pos1-Eta1|_Delta],
                                                _I,_J,_K,L,_S)  ],
%           -------------------------------------------------------
            item(Pos1-Eta1, [], Theta, L,x,x,L, []),
%   where
            [(Pos1-Eta1 ---> Theta)]).
    
%%%---------------------------------------------------------------------
%%% COMPLETORS:

%%% Type 1 and 2 Completor
inference(  type1/2,
            [  item(b-Eta1, Gamma, [t-Eta|Delta], M,J1,K1,I, S2),
               item(t-Eta, _Theta, [], I,J,K,L, S1)   ],
%           --------------------------------------------------------
            item(b-Eta1, [t-Eta|Gamma], Delta, M, J2, K2, L, S3),
%   where
            [\+ auxroot(Eta),
             combine(J, J1, J2), 
             combine(K, K1, K2),
             append(S1,S2,S3)]).

%%% Type 3 Completor
inference(  type3,
            [  item(t-Eta, [], [b-Eta], I,x,x,I, []) ,
               item(b-Eta, _Theta, [], I,J,K,L, S)  ],
%           ---------------------------------------------
            item(t-Eta, [b-Eta], [], I,J,K,L, S),
            []).

%%% Type 4a Completor
inference(  type4a,
            [  item(t-Eta, [], [t-EtaR], I,x,x,I, []), 
               item(t-EtaR, _Theta, [], I,J,K,L, S1),
               item(b-Eta, _Delta, [], J,P,Q,K, S2)    ],
%           ---------------------------------------------
            item(t-Eta, [t-EtaR], [], I,P,Q,L, S),
%   where
            [auxroot(EtaR),
             tree(EtaR, TreeR),  addr(EtaR, _AddR),
             tree(Eta, _Tree),   addr(Eta,  Add),
             predicative(TreeR),
             append([[TreeR/Add | S1]], S2, S)]  ).

%%% Type 4b Completor
inference(  type4b,
            [  item(b-Eta, [], [t-EtaR], I,x,x,I, []), 
               item(t-EtaR, _Theta, [], I,J,K,L, S1),
               item(b-Eta, _Delta, [], J,P,Q,K, S2)    ],
%           ---------------------------------------------
            item(b-Eta, [t-EtaR], [], I,P,Q,L, S),
%   where
            [auxroot(EtaR),
             tree(EtaR, TreeR),  addr(EtaR, _AddR),
             tree(Eta, _Tree),   addr(Eta,  Add),
             modifier(TreeR),
             append([[TreeR/Add | S1]], S2, S)]  ).

%%% Type 5 Completor
inference(  type5,
            [  item(b-EtaF, [], [b-Eta], I,x,x,I, []),
               item(b-Eta, _Theta, [], I,_J,_K,L, _S)   ],
%           -----------------------------------------------
            item(b-EtaF, [b-Eta], [], I,I,L,L, []),
%   where
            [auxfoot(EtaF)]).

%%% Type 6 Completor
inference(  type6,
            [  item(t-Eta, [], [t-EtaR], I,x,x,I, []),
               item(t-EtaR, _Theta, [], I,x,x,L, S)   ],
%           -----------------------------------------------
            item(t-Eta, [t-EtaR], [], I,x,x,L, S1),
%   where
            [initroot(EtaR),
             S1 = [[TreeR/Add | S]]   ]) :-
%************************************************************************
% Should these be moved into the side conditions?
    tree(EtaR, TreeR),  addr(EtaR, _AddR),
    tree(Eta, _Tree),   addr(Eta,  Add).

