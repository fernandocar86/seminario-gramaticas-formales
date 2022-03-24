/* pptree.pl
 * 
 * pretty print a tree in Prolog-readable form
 * 
 * E. Stabler, December 1993
 */

:- module(pptree,[pptree/1,pptrees/2]).

pptree(Term) :- numbervars(Term, 0, _), pptree(Term, 0), write('.'), nl, fail ; true.

pptree(Cat/Children, Column) :- !,
	tab(Column), 
%	print(Cat),
	writeq(Cat),
	write(' /['),
	pptrees(Children, Column).
pptree(X, Column) :- 
	tab(Column), 
%	print(X).
	writeq(X).

pptrees([], _) :- write(']').
pptrees([Tree|Trees], Column) :-
	NextColumn is Column+4,
	nl, pptree(Tree, NextColumn),
	pprest_trees(Trees, NextColumn).

pprest_trees([], _) :- write(']').
pprest_trees([Tree|Trees], Column) :-
	write(','),
	nl, pptree(Tree, Column),
	pprest_trees(Trees, Column).
