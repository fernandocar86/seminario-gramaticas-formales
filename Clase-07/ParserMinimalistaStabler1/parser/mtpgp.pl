% file   : mtpgp.pl  (swi version)
% origin author : E Stabler
% origin date: Jun 2004
% purpose: 
% updates: 
%    note! slow! really! not optimized for speed

:- op(500, xfy, ::). % lexical items
:- op(500, xfy, @). % for selection features

:- compile('../closure'). % Shieber, Schabes & Pereira's 1993 chart-based closure
:- use_module('../tree_display/stepx',[step/5]).
%:- use_module(library(lists),[append/3,member/2,memberchk/2,reverse/2,nth/3]).
%:- use_module(library(system),[system/1]).

% for swi
nth(X,L,A) :- var(X),!,nthmember(L,A,X).
nth(1,[A|_],A) :- !.
nth(N,[_|L],A) :- N>1, N0 is N-1, nth(N0,L,A).

nthmember([A|_],A,1).
nthmember([_|L],A,N) :- nthmember(L,A,N0), N is N0+1.

%% top level interface predicates
parse(Input) :- startCategory(Cat), parse(Input,Cat).

parse(Input,StartCat) :-
	parse(Input,StartCat,Derivation),
	portray_clause(derivation:Derivation).

parse(Input,StartCat,Derivation) :-
	write('analyzing...'),ttyflush,
	statistics(runtime,_),
        buildChart(Input),!,
	statistics(runtime,[_,Runtime]),
	nl,portray_clause(runtime:Runtime),
	( bagof(x,NN^II^stored(NN,II),IIL),length(IIL,IIN),portray_clause(chartlength:IIN) ),
	nl,writeAxioms,nl,
	collectDerivation(Input,StartCat,Derivation).
%	sameOrder(Input,Derivation). % to select derivations with leaves in string order

showParse(Input) :-
	startCategory(StartCat),
	showParse(Input,StartCat).

showParse(Input,StartCat) :-
	startCategory(StartCat),
	parse(Input,StartCat,T),
	portray_clause(derivation:T),
%	lparse(Derivation,T,B,X,LexItems,D),
	B=T,X=T,LexItems=[],D=T, % temporarily, just the derivation tree
	step(T,B,X,LexItems,D).

%% the recognizer

buildChart(Input) :-
	setof(Axiom,lexAxiom(Input,Axiom),Axioms),!,
        closure(Axioms). % close Axioms under relations defined by inference/4
buildChart(_).

reducibleTuple(Types0) :-
	selectNth(_I,Types0,TypeI,Types1), selectNth(_J,Types1,TypeJ,_Types),
	(   properTuple([TypeI],[]), properAndSaturatedTypes([TypeJ],[])
	;   properTuple([TypeJ],[]), properAndSaturatedTypes([TypeI],[])
	),
	match(TypeI,TypeJ,_NewType,_HiddenTerm,_Uncon).

% a tuple is (ListOfCoords:ListOfTypes@Src)
% merge combines a pair of tuples (if they do not overlap, and catsizes=<1)
inference(merge,
          [ (LeftCoords:LeftTypes@_),
	    (RightCoords:RightTypes@_) ],
            (Coords:Types@mrg(N)),
	  [noOverlaps(LeftCoords,RightCoords),
	   \+reducibleTuple(LeftTypes), \+reducibleTuple(RightTypes),  % eager
	   properTuple(LeftTypes,[]), properAndSaturatedTypes(RightTypes,[]), %proper and saturated
	   nappend(LeftCoords,RightCoords,Coords,N),
	   nappend(LeftTypes,RightTypes,Types,N),
	   reducibleTuple(Types)  % eager
	  ]).

% move combines coordinates of a tuple (if they are adjacent)
inference(move,
          [ (Coords0:Types0@_) ],
            ([(X,Z)|Coords]:[NewType|Types]@v(Y,I,J,TypeI,TypeJ)),
	  [properTypes(Types0), % don't apply if a contraction is waiting to be done, in rl setting, implies reduced
	   selectNthPair(Coords0,X,Y,Coords1,I), selectNthPair(Coords1,Y,Z,Coords,J),
	   selectNth(I,Types0,TypeI,Types1), selectNth(J,Types1,TypeJ,Types),
	   match(TypeI,TypeJ,_NewType,_HiddenTerm,_Uncon), % only combine when reduction possible
	   append(TypeI,TypeJ,NewType)
	  ]).

% contract any type
inference(con,
          [ (Coords0:Types0@_) ],
            ([(X,Y)|Coords]:[NewType|Types]@con(I,Type,N)),
	  [selectNthPair(Coords0,X,Y,Coords,I),
	   selectNth(I,Types0,Type,Types),
	   matchAdjacent(Type,NewType,N)
	  ]).

matchAdjacent([l(A),B|Rest],Rest,1) :- lte(B,A).
matchAdjacent([A,r(B)|Rest],Rest,1) :- lte(A,B).
matchAdjacent([A|Rest0],[A|Rest],N) :- matchAdjacent(Rest0,Rest,N0), N is N0+1.

match(L0,[r(C)|R],New,r(C),U) :- !, selectLast(L0,B,L), lte(B,C), append(L,R,New), append(L0,[r(C)|R],U), !.
match(L0,[C|R],New,l(C),U) :- atomic(C), selectLast(L0,l(B),L), lte(C,B), append(L,R,New), append(L0,[C|R],U), !.

lte(A,A).
lte(A,B):- lt(A,C),lte(C,B).

% a sequence of types is proper 
properTypes([]).
properTypes([T|Ts]) :- properType(T,0,_A), properTypes(Ts).

% a tuple is proper iff each type is proper and no 2 tuples have same simple type
properTuple([],_).
properTuple([T|Ts],SimpleTypes0) :-
	properType(T,0,A), onlyLast(A,SimpleTypes0,SimpleTypes), properTuple(Ts,SimpleTypes).

% a type is proper iff it has exactly 1 simple type
properType([],1,_).
properType([r(_)|T],Flag,A) :- !, properType(T,Flag,A).
properType([l(_)|T],Flag,A) :- !, properType(T,Flag,A).
properType([A|T],0,A) :- !, properType(T,1,A).

% when these feature lists are short, not worth overhead of using order
onlyLast(r(_),Ts,Ts) :- !.
onlyLast(l(_),Ts,Ts) :- !.
onlyLast(A,[A|_],_) :- !, fail.
onlyLast(A,[B|Ts0],[B|Ts]) :- !, onlyLast(A,Ts0,Ts).
onlyLast(A,[],[A]).

% a tuple is proper iff each tuple is properAndSaturated and no 2 tuples have same simple type
properAndSaturatedTypes([],_).
properAndSaturatedTypes([[A]|Ts],SimpleTypes0) :- atomic(A), onlyLast(A,SimpleTypes0,SimpleTypes), properAndSaturatedTypes(Ts,SimpleTypes).

%% collect derivation

collectDerivation(L,Cat,Tree):-
	length(L,N),
	stored(_,([(0,N)]:[[Cat]]@Src)),%!, EPS -- cannot cut here
	write('accepted as category '),write(Cat),write(': '), writeSpacedList(L), nl,
	retractall(gCntr(_)),
%	buildDerivation(([(0,N)|Coords]:[[Cat]|Cats]@Src),L,Tree). % assumes chart is in stored/2
	buildDerivation(([(0,N)]:[[Cat]]@Src),L,Tree). % assumes chart is in stored/2
%	numlex(NumLex), % get numbered list of lex items
%	numlist(D,NumLex,NumList). % represent yield as numbers
collectDerivation(L,Cat,_):-	%EPS
	length(L,N),
	\+ stored(_,([(0,N)]:[[Cat]]@_Src)),
	write('not accepted as category '),write(Cat),write(': '), writeSpacedList(L), nl, fail.

buildDerivation(([(X,Z)|Coords]:[NewType|Types]@v(Y,I,J,TypeI,TypeJ)),Input,
		([NewType|Types]:Wordses)/[L]) :-
	stored(_,([(X,Z)|Coords]:[NewType|Types]@v(Y,I,J,TypeI,TypeJ))),
	selectNthPair(Coords1,Y,Z,Coords,J),
	selectNthPair(Coords0,X,Y,Coords1,I),
	selectNth(J,Types1,TypeJ,Types),
	selectNth(I,Types0,TypeI,Types1), 
%	match(TypeI,TypeJ,NewType,HiddenTerm,MidType),
	buildDerivation((Coords0:Types0@_),Input,L),
	wordses([(X,Z)|Coords],Input,Wordses).

buildDerivation(([(X,Y)|Coords]:[NewType|Types]@con(I,Type,N)),Input,
		([NewType|Types]:Wordses)/[L]) :-
	stored(_,([(X,Y)|Coords]:[NewType|Types]@con(I,Type,N))),
	selectNthPair(Coords0,X,Y,Coords,I),
	selectNth(I,Types0,Type,Types), 
	matchAdjacent(Type,NewType,N),
	buildDerivation((Coords0:Types0@_),Input,L),
	wordses([(X,Y)|Coords],Input,Wordses).

buildDerivation((Coords:Types@mrg(N)),Input,(Types:Wordses)/[L,R]) :-
	stored(_,(Coords:Types@mrg(N))),
	nappend(LeftCoords,RightCoords,Coords,N),
	nappend(LeftTypes,RightTypes,Types,N),
	buildDerivation((LeftCoords:LeftTypes@_),Input,L),
	buildDerivation((RightCoords:RightTypes@_),Input,R),
	wordses(Coords,Input,Wordses).

buildDerivation((Coords:Types@lex(Words)),_,(Types:Words)/[]) :-
	stored(_,(Coords:Types@lex(Words))).

%% That's it. The rest is utilities:

lexAxiom(Input,(Coords:Types@lex(Words))) :- (Types::Strings), wordsCoords(Strings,Input,Coords,Words,[]).

wordsCoords([],_,[],[],_Used).
wordsCoords([[]|Ws],Input,[(N,N)|Coords],[[]|Words],Used) :- !, wordsCoords(Ws,Input,Coords,Words,Used).
wordsCoords([W|Ws],Input,[(N0,N)|Coords],[W|Words],Used) :-
	nth(N,Input,W),
	\+ member(N,Used), % coordinates cannot overlap
	N0 is N-1,
	wordsCoords(Ws,Input,Coords,Words,[N|Used]).

selectLast([H0,H|T],Last,[H0|Rem]) :- !, selectLast([H|T],Last,Rem).
selectLast([H],H,[]) :- !.

selectNthPair([(X,Y)|Rest],X,Y,Rest,0).
selectNthPair([(X,Y)|Rest0],Z,W,[(X,Y)|Rest],s(N)) :-  selectNthPair(Rest0,Z,W,Rest,N).

selectNth(0,[X|Xs],X,Xs).
selectNth(s(N),[Y|Ys],X,[Y|Xs]) :- selectNth(N,Ys,X,Xs).

noOverlaps(X0,Y0) :-
	sort(X0,X), length(X,XN), length(X0,XN),
	sort(Y0,Y), length(Y,YN), length(Y0,YN),
	ordLinearizePairs(X,Y), !.

ordLinearizePairs([],_) :- !.
ordLinearizePairs(_,[]) :- !.
ordLinearizePairs([(A,B)|X],Y) :- A==B, !, ordLinearizePairs(X,Y).
ordLinearizePairs(X,[(A,B)|Y]) :- A==B, !, ordLinearizePairs(X,Y).
ordLinearizePairs(X,Y) :- ordLinearizePairs1(X,Y).

ordLinearizePairs1([],_).
ordLinearizePairs1([(A,B)|X],Y0) :- ordInsertPair((A,B),Y0,Y), ordLinearizePairs1(X,Y).

ordInsertPair((A,B),[(C,D)|Y],[(A,B),(C,D)|Y]) :- B=<C, !. % first
ordInsertPair((A,B),[(C,D)],[(C,D),(A,B)]) :- D=<A, !. % last
ordInsertPair((A,B),[(C,D),(E,F)|Y],[(C,D),(A,B),(E,F)|Y]) :- D=<A, B=<E, !. % second
ordInsertPair((A,B),[(C,D),(E,F)|Y0],[(C,D)|Y]) :- A>=F, !, ordInsertPair((A,B),[(E,F)|Y0],Y). % later

wordses([],_,[]).
wordses([(X,Y)|Coords],Input,[Words|Wordses]) :-
	words(X,Y,Input,Words),!,
	wordses(Coords,Input,Wordses),!.

words(X,X,_,[]) :- !.
words(0,N,[Word|Ws],[Word|Words]) :- !, nonvar(N), N>0,
	N0 is N-1, words(0,N0,Ws,Words).
words(N1,N2,[_|Ws],Words) :- nonvar(N1), N1>0, nonvar(N2), N2>0,
	N10 is N1-1, N20 is N2-1, words(N10,N20,Ws,Words).

sameOrder(L,D) :- yield(D,L,[]).

yield(_/[H|T],L0,L) :- yield(H,L0,L1),yields(T,L1,L).
yield((_Types:Lists)/[],L0,L) :- prefixes(Lists,L0,L).

yields([],L,L).
yields([T|Ts],L0,L) :- yield(T,L0,L1),yields(Ts,L1,L).

prefixes([],L,L).
prefixes([H|T],L0,L) :- selectFirst(H,L0,L1), prefixes(T,L1,L).

selectFirst([],L,L).
selectFirst(W,[W|L],L).

nappend([],L,L,0).
nappend([E|L],M,[E|N],s(Pos)) :- nappend(L,M,N,Pos).

concat(Atom1,Atom2,Atom12) :-
	(atomic(Atom1) -> true ; portray_clause(error: Atom1),abort),
	(atomic(Atom2) -> true ; portray_clause(error: Atom2),abort),
	name(Atom1,L1),name(Atom2,L2),append(L1,L2,L12),name(Atom12,L12).

numlex(L) :- retractall(gCntr(_)), setof(N=(A::B),(A::B,gensym(N)),L).

:- dynamic gCntr/1.
gensym(B) :- var(B),  ( retract(gCntr(C)), B is C+1 ; B=0 ), asserta(gCntr(B)), !.
gensym(_).

writeAxioms :- stored(_,Ax), writeAxiom(Ax), fail.
writeAxioms.

writeAxiom(Ax) :- numbervars(Ax,0,_), print(Ax), nl, fail.
writeAxiom(_).

writeSpacedList(X) :- atomic(X), !, writeCat(X).
writeSpacedList(l(X)) :- !, writeCat(l(X)).
writeSpacedList(r(X)) :- !, writeCat(r(X)).
writeSpacedList([H]) :- !, writeCat(H).
writeSpacedList([H|T]) :- !, writeCat(H), put(32), writeSpacedList(T).

writeType(X) :- atomic(X), !, writeCat(X).
writeType(l(X)) :- !, writeCat(l(X)).
writeType(r(X)) :- !, writeCat(r(X)).
writeType([H]) :- !, writeCat(H).
writeType([H|T]) :- !, writeCat(H), writeType(T).

writeCat(r(X)) :- !, write(X),write('''').
writeCat(l(X)) :- !, write(X),write('`').
writeCat([]) :- !,write([]).
writeCat(H) :- !, write(H).

writeStrings([]) :- !.
writeStrings([H]) :- !, writeSpacedList(H).
writeStrings([H|T]) :- !, writeSpacedList(H), write(','), writeStrings(T).
writeStrings(X) :- writeCat(X).

writeTypes([]) :- !.
writeTypes([H]) :- !, writeType(H).
writeTypes([H|T]) :- !, writeType(H), write(','), writeTypes(T).
writeTypes(X) :- writeCat(X).

portray((A:B@C)) :- !,writeTypes(A),write(':'),writeStrings(B),write('@'),write(C).
portray((A:B)) :- !,writeTypes(A),write(':'),writeStrings(B).
portray(A) :- writeTypes(A).
