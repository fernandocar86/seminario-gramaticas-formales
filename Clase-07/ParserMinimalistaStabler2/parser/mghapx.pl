% file   : mghapx.pl  (swi version)
% origin author : E Stabler
% origin date: Mar 2000
% purpose: Harkema(2000) recognizer, with Stabler&Keenan(2000) adjustment
% updates: ES Mar 2000: additional information to items for tree collection
%          ES Mar 2001: head movement, affix hopping
%          ES May 2001: collect_derivation -> leaves of derivation (Hale&Stabler 2001) 
%          Willemijn Vermaat June 2001 head movement rules in lhapx.pl
%          ES Feb 2002: adjunction

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features
:- op(500, xf, <=). % for right incorporation
:- op(500, fx, =>). % for left incorporation
:- op(500, xf, ==>). % for right affix hop
:- op(500, fx, <==). % for left affix hop

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
%	nl,writeAxioms,nl,
	collectDerivation(Input,StartCat,Derivation).

showParse(Input) :-
	startCategory(StartCat),
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
axiom(X,[W|Input],([S,S,X,Y,C,C,lex([W|Pronounced])]:Features)) :-
	current_predicate('::',(_ :: _)),
        ([W|Pronounced] :: Features),
	append(Pronounced,_,Input),
	length([W|Pronounced],Length),
	Y is X+Length.
axiom(X,[_|Input],Axiom) :- Y is X+1, axiom(Y,Input,Axiom).
axiom(_,[],[S,S,X,X,C,C,empty]:Axiom) :- current_predicate('::',(_::_)), ([] :: Axiom).

% left adjoin a non-moving modifier
inference(a1,
          [ _:[[A0,A1,A1,A2,A2,A3,_]:[Cat0|Gamma]|Chains1], % modifier Cat0 on left
	    _:[[A3,A,B0,B,C0,C,_]:[Cat1|EtaNu]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,a1(Cat0,Gamma,Cat1,A1,A2,A3,N1)]:[Cat1|EtaNu]|Chains],
	  [atomic(Cat0),atomic(Cat1),
	   current_predicate(>>,_),
	   ([Cat0|Gamma]>>[Cat1|Eta]),
	   append(Eta,_Nu,EtaNu),
	   nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C]:[Cat1|EtaNu]|Chains])]).

% right adjoin a non-moving modifier
inference(a2,
          [ _:[[C1,C2,C2,C3,C3,C,_]:[Cat0|Gamma]|Chains1], % modifier Cat0 on right
	    _:[[A0,A,B0,B,C0,C1,_]:[Cat1|EtaNu]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,a2(Cat0,Gamma,Cat1,C1,C2,C3,N1)]:[Cat1|EtaNu]|Chains],
	  [atomic(Cat0),atomic(Cat1),
	   current_predicate(<<,_),
	   ([Cat1|Eta]<<[Cat0|Gamma]),
	   append(Eta,_Nu,EtaNu),
	   nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C]:[Cat1|EtaNu]|Chains])]).

inference(r1,
          [ s:[[A,A,B0,B,C,C,_]:[=Feature|Gamma]],
	    _:[[D0,D1,D1,D2,D2,D,_]:[Feature]|Chains] ],
            c:[[A,A,B0,B,D0,D,r1(Feature,D1,D2)]:Gamma|Chains],
	  [smc([[A,A,B0,B,D0,D]:Gamma|Chains])]).

inference(r1right,
          [ s:[[A,A,B0,B1,C,C,_]:[Feature<=|Gamma]],
	    _:[[D0,D1,B1,B,D1,D,_]:[Feature]|Chains] ],
            c:[[A,A,B0,B,D0,D,r1r(Feature,B1,D1)]:Gamma|Chains],
	  [smc([[A,A,B0,B,D0,D]:Gamma|Chains])]).

inference(r1left,
          [ s:[[A,A,B1,B,C,C,_]:[=>Feature|Gamma]],
	    _:[[D0,D1,B0,B1,D1,D,_]:[Feature]|Chains] ],
            c:[[A,A,B0,B,D0,D,r1l(Feature,B1,D1)]:Gamma|Chains],
	  [smc([[A,A,B0,B,D0,D]:Gamma|Chains])]).

inference(r1hopright, 
          [ s:[[A,A,D2,D3,C,C,_]:[Feature==>|Gamma]],
	    _:[[D0,D1,D1,D2,D3,D,_]:[Feature]|Chains] ],
            c:[[A,A,E,E,D0,D,r1hr(Feature,D1,D2,D3)]:Gamma|Chains],
	  [smc([[A,A,E,E,D0,D]:Gamma|Chains])]).

inference(r1hopleft, 
          [ s:[[A,A,D1,D2,C,C,_]:[<==Feature|Gamma]],
	    _:[[D0,D1,D2,D3,D3,D,_]:[Feature]|Chains] ],
            c:[[A,A,E,E,D0,D,r1hl(Feature,D1,D2,D3)]:Gamma|Chains],
	  [smc([[A,A,E,E,D0,D]:Gamma|Chains])]).

inference(r2,
          [ c:[[A3,A,B0,B,C0,C,_]:[=Feature|Gamma]|Chains1],
	    _:[[A0,A1,A1,A2,A2,A3,_]:[Feature]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,r2(Feature,A1,A2,A3,N1)]:Gamma|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma|Chains]) ]) .

inference(r3,
          [ _:[[A0,A,B0,B,C0,C,_]:[=Feature|Gamma]|Chains1],
	    _:[[D0,D1,D1,D2,D2,D,_]:[Feature|[Req|Delta]]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,r3(Feature,D0,D1,D2,D,N1)]:Gamma,[D0,D,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

% WV 06/01 added: inference/4 for r3right, r3left, r3hopright and r3hopleft
inference(r3right,
          [ _:[[A0,A,B0,B1,C0,C,_]:[Feature<=|Gamma]|Chains1],
	    _:[[D0,D1,B1,B,D1,D,_]:[Feature|[Req|Delta]]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,r3r(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

inference(r3left,
          [ _:[[A0,A,B1,B,C0,C,_]:[=>Feature|Gamma]|Chains1],
	    _:[[D0,D1,B0,B1,D1,D,_]:[Feature|[Req|Delta]]|Chains2] ],
            c:[[A0,A,B0,B,C0,C,r3l(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

inference(r3hopright,
          [ _:[[A0,A,D2,D3,C0,C,_]:[Feature==>|Gamma]|Chains1],
	    _:[[D0,D1,D1,D2,D3,D,_]:[Feature|[Req|Delta]]|Chains2] ],
            c:[[A0,A,E0,E,C0,C,r3hr(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,E0,E,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

inference(r3hopleft,
          [ _:[[A0,A,D1,D2,C0,C,_]:[<==Feature|Gamma]|Chains1],
	    _:[[D0,D1,D2,D3,D3,D,_]:[Feature|[Req|Delta]]|Chains2] ],
            c:[[A0,A,E0,E,C0,C,r3hl(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,_]:[Req|Delta]|Chains],
	  [nappend(Chains1,Chains2,Chains,N1),
	   smc([[A0,A,E0,E,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

inference(v1,
          [ c:[[A1,A,B0,B,C0,C,_]:[+Feature|Gamma]|Chains1] ],
            c:[[A0,A,B0,B,C0,C,v1(Feature,A1,N2)]:Gamma|Chains],
	  [nappend(Chains2,[[A0,A1,_]:[-Feature]|Chains3],Chains1,N2),
	   append(Chains2,Chains3,Chains),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma|Chains]) ]).

inference(v2,
          [ c:[([A0,A,B0,B,C0,C,_]:[+Feature|Gamma])|Chains1] ],
            c:[([A0,A,B0,B,C0,C,v2(Feature,N1)]:Gamma), ([D0,D,_]:[Req|Delta]) |Chains],
  	  [nappend(Chains2,[[D0,D,_]:[-Feature|[Req|Delta]] |Chains3], Chains1, N1),
	   append(Chains2,Chains3,Chains),
	   smc([[A0,A,B0,B,C0,C,_]:Gamma,[D0,D,_]:[Req|Delta]|Chains]) ]).

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
	stored(_,Type:[[0,A,A,B,B,N,Src]:[Cat]]),
%	write('accepted as category '),write(Cat),write(': '), writes(L), nl,
	write('aceptada como categoría '),write(Cat),write(': '), writes(L), nl,
	retractall(gCntr(_)),
	buildDerivation(Type:[[0,A,A,B,B,N,Src]:[Cat]],[],D), % chart is in stored/2
	numlex(NumLex), % get numbered list of lex items
	numlist(D,NumLex,NumList). % represent yield as numbers
collectDerivation(L,Cat,_):-
	length(L,N),
	\+stored(_,_Type:[[0,A,A,B,B,N,_Src]:[Cat]]), % EPS
%	write('not accepted as category '),write(Cat),write(': '), writes(L), nl, fail.
	write('no aceptada como categoría '),write(Cat),write(': '), writes(L), nl, fail.

categoryOf([=>_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([_<=|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([<==_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([_==>|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([=_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([+_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([-_|L],Feature) :- !, categoryOf(L,Feature).
categoryOf([Feature|_],Feature).

buildDerivation(s:[[S,S,X,X,C,C,empty]:L], G, [([]::L)|G]) :-
	stored(_,s:[[S,S,X,X,C,C,empty]:L]).

buildDerivation(Type:[[S,S,X,Y,C,C,lex(Words)]:L], G, [(Words::L)|G]) :-
	stored(_,Type:[[S,S,X,Y,C,C,lex(Words)]:L]).

buildDerivation(c:[[A,A,B0,B,D0,D,r1(Feature,D1,D2)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A,A,B0,B,D0,D,r1(Feature,D1,D2)]:Gamma|Chains]),
	buildDerivation(s:[[A,A,B0,B,C,C,_]:[=Feature|Gamma]],G1,G),
	buildDerivation(_:[[D0,D1,D1,D2,D2,D,_]:[Feature]|Chains],G0,G1).

buildDerivation(c:[[A,A,B0,B,D0,D,r1r(Feature,B1,D1)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A,A,B0,B,D0,D,r1r(Feature,B1,D1)]:Gamma|Chains]),
	buildDerivation(s:[[A,A,B0,B1,C,C,_]:[Feature<=|Gamma]],G1,G),
	buildDerivation(_:[[D0,D1,B1,B,D1,D,_]:[Feature]|Chains],G0,G1).

buildDerivation(c:[[A,A,B0,B,D0,D,r1l(Feature,B1,D1)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A,A,B0,B,D0,D,r1l(Feature,B1,D1)]:Gamma|Chains]),
	buildDerivation(s:[[A,A,B1,B,C,C,_]:[=>Feature|Gamma]],G1,G),
	buildDerivation(_:[[D0,D1,B0,B1,D1,D,_]:[Feature]|Chains],G0,G1).

buildDerivation(c:[[A,A,E,E,D0,D,r1hr(Feature,D1,D2,D3)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A,A,E,E,D0,D,r1hr(Feature,D1,D2,D3)]:Gamma|Chains]),
	buildDerivation(s:[[A,A,D2,D3,C,C,_]:[Feature==>|Gamma]],G1,G),
	buildDerivation(_:[[D0,D1,D1,D2,D3,D,_]:[Feature]|Chains],G0,G1).

buildDerivation(c:[[A,A,E,E,D0,D,r1hl(Feature,D1,D2,D3)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A,A,E,E,D0,D,r1hl(Feature,D1,D2,D3)]:Gamma|Chains]),
	buildDerivation(s:[[A,A,D1,D2,C,C,_]:[<==Feature|Gamma]],G1,G),
	buildDerivation(_:[[D0,D1,D2,D3,D3,D,_]:[Feature]|Chains],G0,G1).

buildDerivation(c:[[A0,A,B0,B,C0,C,r2(Feature,A1,A2,A3,N1)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A0,A,B0,B,C0,C,r2(Feature,A1,A2,A3,N1)]:Gamma|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(c:[[A3,A,B0,B,C0,C,_]:[=Feature|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[A0,A1,A1,A2,A2,A3,_]:[Feature]|Chains2],G0,G1).

buildDerivation(Type:[[A0,A,B0,B,C0,C,r3(Feature,D0,D1,D2,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains], G0, G) :-
	stored(_,Type:[[A0,A,B0,B,C0,C,r3(Feature,D0,D1,D2,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A,B0,B,C0,C,_]:[=Feature|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[D0,D1,D1,D2,D2,D,_]:[Feature|[Req|Delta]]|Chains2],G0,G1).

%WV, added 06/01: r3r, r3l, r3hr and r3hl
buildDerivation(Type:[[A0,A,B0,B,C0,C,r3r(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains], G0, G) :-
	stored(_,Type:[[A0,A,B0,B,C0,C,r3r(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A,B0,B1,C0,C,_]:[Feature<=|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[D0,D1,B1,B,D1,D,_]:[Feature|[Req|Delta]]|Chains2],G0,G1).

buildDerivation(Type:[[A0,A,B0,B,C0,C,r3l(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains], G0, G) :-
	stored(_,Type:[[A0,A,B0,B,C0,C,r3l(Feature,D0,B1,D1,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A,B1,B,C0,C,_]:[=>Feature|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[D0,D1,B0,B1,D1,D,_]:[Feature|[Req|Delta]]|Chains2],G0,G1).

buildDerivation(Type:[[A0,A,E0,E,C0,C,r3hr(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains], G0, G) :-
	stored(_,Type:[[A0,A,E0,E,C0,C,r3hr(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A,D2,D3,C0,C,_]:[Feature==>|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[D0,D1,D1,D2,D3,D,_]:[Feature|[Req|Delta]]|Chains2],G0,G1).

buildDerivation(Type:[[A0,A,E0,E,C0,C,r3hl(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains], G0, G) :-
	stored(_,Type:[[A0,A,E0,E,C0,C,r3hl(Feature,D0,D1,D2,D3,D,N1)]:Gamma,[D0,D,S]:[Req|Delta]|Chains]),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A,D1,D2,C0,C,_]:[<==Feature|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[D0,D1,D2,D3,D3,D,_]:[Feature|[Req|Delta]]|Chains2],G0,G1).

buildDerivation(c:[[A0,A,B0,B,C0,C,v1(Feature,A1,N2)]:Gamma|Chains], G0, G) :-
	stored(_,c:[[A0,A,B0,B,C0,C,v1(Feature,A1,N2)]:Gamma|Chains]),
	nappend(Chains2,[[A0,A1,_Src]:[-Feature]|Chains3],Chains1,N2),
	append(Chains2,Chains3,Chains),
	buildDerivation(c:[[A1,A,B0,B,C0,C,_Src0]:[+Feature|Gamma]|Chains1],G0,G).

buildDerivation(c:[([A0,A,B0,B,C0,C,v2(Feature,N1)]:Gamma), ([D0,D,Src]:[Req|Delta]) |Chains], G0, G) :-
	stored(_,c:[([A0,A,B0,B,C0,C,v2(Feature,N1)]:Gamma), ([D0,D,Src]:[Req|Delta]) |Chains]),
	nappend(Chains2, [[D0,D,Src]:[-Feature|[Req|Delta]] |Chains3] , Chains1, N1),
	append(Chains2, Chains3, Chains),
	buildDerivation(c:[([A0,A,B0,B,C0,C,_]:[+Feature|Gamma])|Chains1], G0, G).

buildDerivation(c:[[A0,A,B0,B,C0,C,a1(Cat0,Gamma,Cat1,A1,A2,A3,N1)]:[Cat1|EtaNu]|Chains], G0, ['>>'|G]) :-
	stored(_,c:[[A0,A,B0,B,C0,C,a1(Cat0,Gamma,Cat1,A1,A2,A3,N1)]:[Cat1|EtaNu]|Chains]),
	   current_predicate(>>,_),
	   ([Cat0|Gamma]>>[Cat1|Eta]),
	   append(Eta,_Nu,EtaNu),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[A0,A1,A1,A2,A2,A3,_]:[Cat0|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[A3,A,B0,B,C0,C,_]:[Cat1|EtaNu]|Chains2],G0,G1).

buildDerivation(c:[[A0,A,B0,B,C0,C,a2(Cat0,Gamma,Cat1,C1,C2,C3,N1)]:[Cat1|EtaNu]|Chains], G0, ['<<'|G]) :-
	stored(_,c:[[A0,A,B0,B,C0,C,a2(Cat0,Gamma,Cat1,C1,C2,C3,N1)]:[Cat1|EtaNu]|Chains]),
	   current_predicate(<<,_),
	   ([Cat1|Eta]<<[Cat0|Gamma]),
	   append(Eta,_Nu,EtaNu),
	nappend(Chains1,Chains2,Chains,N1),
	buildDerivation(_:[[C1,C2,C2,C3,C3,C,_]:[Cat0|Gamma]|Chains1],G1,G),
	buildDerivation(_:[[A0,A,B0,B,C0,C1,_]:[Cat1|EtaNu]|Chains2],G0,G1).

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
  numbervars([I,J,K]:Fs,0,_),
  write('('),write((I,J,K)),write('):'),writes(Fs),write('   '),fail; true.
writeChain([A,B,C,D,E,F,K]:Fs) :-
  numbervars([A,B,C,D,E,F,K]:Fs,0,_),
  write('('),write((A,B,C,D,E,F,K)),write('):'),writes(Fs),write('   '),fail; true.

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
portray(X) :- write(X).
