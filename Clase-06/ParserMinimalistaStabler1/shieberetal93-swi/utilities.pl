/*----------------------------------------------------------
                         UTILITIES
----------------------------------------------------------*/

%%% subsumes(+General, +Specific)
%%% =============================
%%%
%%%     Holds if General subsumes Specific.

%%% Note that Quintus Prolog 3.x has a subsumes_chk/2 that
%%% could replace subsumes/2.  The explicit implementation
%%% is left here to illustrate this standard Prolog idiom
%%% for subsumption testing. 

mysubsumes(General, Specific) :-
    \+ \+ ( make_ground(Specific),
            General = Specific ).


%%% make_ground(Term)
%%% =================
%%%
%%%     Instantiates all variables in Term to fresh constants.

make_ground(Term) :-
    numbervars(Term, 0, _).


%%% all_solutions(Term, Goal, Solutions)
%%% ====================================
%%%
%%%     Solutions is a list of instances of Term such that
%%%     Goal holds.  All free variables in Goal are taken to
%%%     be existentially quantified.  Solutions may be the
%%%     empty list if there are no solutions.  

%%% This implementation relies on the details of findall in
%%% Quintus Prolog.  It could be reimplemented using the
%%% more standard built-in predicate setof/3 as follows:
%%%
%%% all_solutions(Var, Goal, Solutions) :-
%%%     setof(Var, Goal^Goal, Solutions).

all_solutions(Var, Goal, Solutions) :-
    findall(Var, Goal, Solutions).


%%% split(Elem, List, Rest)
%%% =======================
%%%
%%%     List = U @ [Elem] @ V and Rest = U @ V.

split(Term, [Term|Rest], Rest).
split(Term, [First|Rest0], [First|Rest]) :-
    split(Term, Rest0, Rest).
