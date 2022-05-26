% file   : mgpx.pl  (swi version)
% origin author : E Stabler
% origin date: Mar 2000
% purpose: Harkema(2000) recognizer, with Stabler&Keenan(2000) adjustment
% updates: ES Mar 2000: additional information to items for tree collection
%          ES Mar 2001: head movement, affix hopping
%          ES May 2001: collect_derivation -> leaves of derivation (Hale&Stabler 2001) 
%          Willemijn Vermaat June 2001
%          ES Feb 2002: adjunction

% operator defs - don't touch these
:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features
:- op(500, xf, <=). % for right incorporation
:- op(500, fx, =>). % for left incorporation
:- op(500, xf, ==>). % for right affix hop
:- op(500, fx, <==). % for left affix hop
:- op(500, xfy, <<). % for adjunction
:- op(500, xfy, >>). % for adjunction

% for debugging:
:- set_prolog_flag(debugger_print_options,[quoted(true), portray(false), max_depth(100)] ).

% for interaction:
:- set_prolog_flag(toplevel_print_options,[quoted(false), portray(true), max_depth(100)] ).

:- ['../closure']. % Shieber, Schabes & Pereira's 1993 chart-based closure
:- use_module('../tree_display/stepx',[step/5]).

parse(Input) :- startCategory(Cat), parse(Input,Cat).

parse(Input,StartCat) :-
	parse(Input,StartCat,Derivation),
	portray_clause(derivation:Derivation).

parse(Input,StartCat,Derivation) :-
%	write('analyzing...'),ttyflush,
	write('analizando...'),ttyflush,
	statistics(runtime,_),
        buildChart(Input),!,
	statistics(runtime,[_,Runtime]),
	nl,portray_clause(runtime:Runtime),
	( bagof(x,NN^II^stored(NN,II),IIL),length(IIL,IIN),portray_clause(chartlength:IIN) ),
	nl,writeAxioms,nl,
	collectDerivation(Input,StartCat,Derivation).

showParse(Input) :-
	startCategory(StartCat),
	showParse(Input,StartCat).

showParse(Input,StartCat) :-
	parse(Input,StartCat,Derivation),
%	( audio -> unix(system(ding)) ; true ), % audio alert on parse completion
	portray_clause(derivation:Derivation),
	lparse(Derivation,T,B,X,LexItems,D),
	step(T,B,X,LexItems,D).

getDep(Input,DepStructs) :-
	startCategory(StartCat),
	parse(Input,StartCat,Derivation),
%	portray_clause(derivation:Derivation),
	lparse(Derivation,D,B,X,LexItems,DepStructs),
	step(D,B,X,LexItems,DepStructs).

buildChart(Input) :-
        setof(s:[Axiom],Input^axiom(0,Input,Axiom),Axioms),!,
        closure(Axioms). % close Axioms under relations defined by inference/4
buildChart(_).

% build axioms in appropriate format; first overt, then empty elements
axiom(X,[W|Input],([X,Y,lex([W|Pronounced])]:Features)) :-
	current_predicate('::',(_ :: _)),
        ([W|Pronounced] :: Features),
	append(Pronounced,_,Input),
	length([W|Pronounced],Length),
	Y is X+Length.
axiom(X,[_|Input],Axiom) :- Y is X+1, axiom(Y,Input,Axiom).
axiom(_,[],[X,X,empty]:Axiom) :- current_predicate('::',(_::_)), ([] :: Axiom).

inference(merge1/3, 
          [ s:[[X,Y,_]:[=C|Gamma]],
	    _:[[Y,Z,_]:[C]|Chains] ],
            c:[[X,Z,r1(C,Y)]:Gamma|Chains],
	  [smc([[X,Z]:Gamma|Chains])]).

inference(merge2/3,
          [ c:[[X,Y,_]:[=C|Gamma]|Chains1],
	    _:[[V,X,_]:[C]|Chains2] ],
            c:[[V,Y,r2(C,X,N1)]:Gamma|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[V,Y,_]:Gamma|Chains]) ]) .

inference(merge3/3,
          [ _:[[X,Y,_]:[=C|Gamma]|Chains1],
	    _:[[V,W,_]:[C|[Req|Delta]]|Chains2] ],
            c:[[X,Y,r3(C,W,N1)]:Gamma,[V,W,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[X,Y,_]:Gamma,[V,W,_]:[Req|Delta]|Chains]) ]).

inference(move1/2,
          [ c:[[X,Y,_]:[+F|Gamma]|Chains1] ],
            c:[[V,Y,v1(F,X,N2)]:Gamma|Chains],
	  [nappend(Chains2,[[V,X,_]:[-F]|Chains3],Chains1,N2),
	   append(Chains2,Chains3,Chains),
	   smc([[V,Y,_]:Gamma|Chains]) ]).

inference(move2/2,
          [ c:[([X,Y,_]:[+F|Gamma])|Chains1] ],
            c:[([X,Y,v2(F,N1)]:Gamma), ([V,W,_]:[Req|Delta]) |Chains],
  	  [nappend(Chains2,[[V,W,_]:[-F|[Req|Delta]] |Chains3], Chains1, N1),
	   append(Chains2,Chains3,Chains),
	   smc([[X,Y,_]:Gamma,[V,W,_]:[Req|Delta]|Chains]) ]).

% tentative SMC: no two -f features are EXPOSED at any point in the derivation
%  (this is written so as to be used by both inference and by lparse)
smc(Chains) :- smc0(Chains,[]).

smc0([],_).
smc0([_:[-F|_]|Chains],Fs) :- !, \+member(F,Fs), smc0(Chains,[F|Fs]).
smc0([_::[-F|_]|Chains],Fs) :- !, \+member(F,Fs), smc0(Chains,[F|Fs]).
smc0([_::_|Chains],Fs) :- !, smc0(Chains,Fs).
smc0([_:_|Chains],Fs) :- smc0(Chains,Fs).

% collect derivation (i.e. lexical leaves of derivation tree)

collectDerivation(L,Cat,NumList):-
	length(L,N),
	stored(_,Type:[[0,N,Src]:[Cat]]),%!, EPS -- cannot cut here
%	write('accepted as category '),write(Cat),write(': '), writes(L), nl,
	write('aceptada como categoría '),write(Cat),write(': '), writes(L), nl,
	retractall(gCntr(_)),
	buildDerivation(Type:[[0,N,Src]:[Cat]],[],D), % assumes chart is in stored/2
	numlex(NumLex), % get numbered list of lex items
	numlist(D,NumLex,NumList). % represent yield as numbers
collectDerivation(L,Cat,_):-  %EPS
	length(L,N),
	\+ stored(_,_Type:[[0,N,_Src]:[Cat]]),
%	write('not accepted as category '),write(Cat),write(': '), writes(L), nl, fail.
	write('no aceptada como categoría '),write(Cat),write(': '), writes(L), nl, fail.

categoryOf([=_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([+_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([-_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([Feature|_],Feature).

buildDerivation(s:[[X,X,empty]:L], D, [([]::L)|D]) :-
	stored(_,s:[[X,X,empty]:L]).

buildDerivation(Type:[[X,Y,lex(Words)]:L], D, [(Words::L)|D]) :-
	stored(_,Type:[[X,Y,lex(Words)]:L]).

buildDerivation(c:[[X,Z,r1(C,Y)]:Gamma|Chains], D0, D) :-
	stored(_,c:[[X,Z,r1(C,Y)]:Gamma|Chains]),
	buildDerivation(s:[[X,Y,_]:[=C|Gamma]],D1,D),
	buildDerivation(_:[[Y,Z,_]:[C]|Chains],D0,D1).

buildDerivation(c:[[V,Y,r2(C,X,N1)]:Gamma|Chains], D0, D) :-
	stored(_,c:[[V,Y,r2(C,X,N1)]:Gamma|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(c:[[X,Y,_]:[=C|Gamma]|Chains1],D1,D),
	buildDerivation(_:[[V,X,_]:[C]|Chains2],D0,D1).

buildDerivation(Type:[[X,Y,r3(C,W,N1)]:Gamma,[V,W,S]:[Req|Delta]|Chains],D0, D) :-
	stored(_,Type:[[X,Y,r3(C,W,N1)]:Gamma,[V,W,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_SubType:[[X,Y,_]:[=C|Gamma]|Chains1],D1,D),
	buildDerivation(_:[[V,W,_]:[C,Req|Delta]|Chains2],D0,D1).

buildDerivation(c:[[V,Y,v1(F,X,N2)]:Gamma|Chains], D0, D) :-
	stored(_,c:[[V,Y,v1(F,X,N2)]:Gamma|Chains]),
	nappend(Chains2,[[V,X,_Src]:[-F]|Chains3],Chains1,N2),
	append(Chains2,Chains3,Chains),
	buildDerivation(c:[[X,Y,_Src0]:[+F|Gamma]|Chains1], D0, D).

buildDerivation(c:[([X,Y,v2(F,N1)]:Gamma), ([V,W,_src]:[Req|Delta]) |Chains], D0, D) :-
	stored(_,c:[([X,Y,v2(F,N1)]:Gamma), ([V,W,_src]:[Req|Delta]) |Chains]),
	nappend(Chains2, [[V,W,_src]:[-F,Req|Delta] |Chains3] , Chains1, N1),
	append(Chains2, Chains3, Chains),
	buildDerivation(c:[([X,Y,_]:[+F|Gamma])|Chains1], D0, D).

% That's it. The rest is utilities:

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

writeAxioms :- stored(_,Type:Chains), write(Type:'  '), writeChains(Chains), fail.
writeAxioms.

writeAxioms([]).
writeAxioms([Type:Chains|T]) :- write(Type:'  '), writeChains(Chains), writeAxioms(T).

writeChains([]):- nl.
writeChains([H|T]) :- writeChain(H), writeChains(T).

writeChain([I,J,K]:Fs) :-
  numbervars([I,J,K]:Fs,0,_), write('('),write((I,J,_K)),write('):'),writes(Fs),write('   '),fail; true.

writes0([]) :- write([]).
writes0([H|T]) :- writes([H|T]).

writes([]).
writes([H]) :- !, print(H).
writes([H|T]) :- !, print(H), put(32), writes(T).

writels([]).
writels([H]) :- !, print(H).
writels([H|T]) :- !, print(H), write(','), writels(T).

writeLs([]).
writeLs([H]) :- !, print(H).
writeLs([H|T]) :- !, print(H), nl, writeLs(T).

portray((A,B,C):L) :- !,
	write('('),writes0(A),write(','),writes0(B),write(','),writes0(C),write('):'),writes0(L).
portray([A|L]) :- !,writels([A|L]).
portray(A:B) :- !,writes0(A),write(':'),writes0(B).
portray(A::B) :- !,writes0(A),write('::'),writes0(B).
portray(+(X)) :- !,write('+'),write(X).
portray(-(X)) :- !,write('-'),write(X).
portray(X) :- !, write(X).
