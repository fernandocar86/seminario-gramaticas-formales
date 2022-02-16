% file   : lpx.pl  (swi version)
% origin author : E Stabler
% origin date: Feb 2002, based on lhp.pl
% various fixes from earlier versions: Willemijn Vermaat
% purpose: l(exical sequence) p(arser) (e)x(tended to various formats) 
% todo:

pronounce(NumLexItems,Words) :- pronounce(NumLexItems,Words,_).
pronounce(NumLexItems,Words,(NWords,Links)) :-
	lparse(NumLexItems,_T,_B,_X,_LexItems,(NWords,Links)),
	numsOut(NWords,Words).

lparse(NumLexItems,T,B,X,LexItems,D) :-
	retractall(gCntr(_)), % fresh numbering of lexical items
	numlex(NumLexicon),
	listnum(NumLexItems,NumLexicon,LexItems),
	reverse(LexItems,RYield),
	retractall(gCntr(_)), % fresh numbering of nontrivial chains
	lprs([],[],[],[],RYield,T,B,X,Deps),
	(  Deps=[[(Head,FeatureNo,NodeLex,Links,StringComponents):String]]
	;  Deps=[[(Head,FeatureNo,NodeLex,Links,StringComponents)::String]]
	),
	featuresOut(Head,Phon),
	D=(Phon,Links).

lparse(NumLexItems,T,B,X) :-
	retractall(gCntr(_)), % fresh numbering of lexical items
	numlex(NumLexicon),
	listnum(NumLexItems,NumLexicon,LexItems),
	reverse(LexItems,RYield),
	retractall(gCntr(_)), % fresh numbering of nontrivial chains
	lprs([],[],[],[],RYield,T,B,X,_Deps).

lparse(NumLexItems) :-
	lparse(NumLexItems,D,B,X,LexItems,DepStructs),
	step(D,B,X,LexItems,DepStructs).

lexItems2LF([F|L0],Item,L) :-		
	synFunc(F,Arity,FName),!,
	lexItems2Args(Arity,L0,L,Args),
	Item=..[FName|Args].

lexItems2Args(0,L,L,[]).
lexItems2Args(N,L0,L,[Item|Items]) :-
	lexItems2LF(L0,Item,L1),
	N1 is N-1,
	lexItems2Args(N1,L1,L,Items).

synFunc(L::[+_|R],Arity,Name) :- !, synFunc(L::R,Arity,Name).
synFunc(_::[-_|_],_,_) :- !, portray_clause('Fatal Error':synFunc), abort.
synFunc(L::[A|_],0,Name) :- atomic(A), !, lfName(L,A,Name).
synFunc(L::[_|R],N,Name) :- synFunc(L::R,N0,Name), N is N0+1.

lfName([Name],_,Name) :- !.
lfName([],Cat,Cat).

% lprs(Ds,Bs,Xs,Deps,Input,B,X,Dep)
%   Ts=stack of derivation trees    T=output
%   Bs=stack of bare trees          B=output
%   Xs=stack of x-bar trees         X=output
%   Ds=stack of dep structs         D=output
%   DepStruct built from (Spec33,Head33,Comp33,FeatureNumber,NodeNumber,ThisNodeElements,ChainElements)

% termination clause
lprs([T],[B],[X],D,[],T,B,X,D) :- tprojected(T), !.
% INIT: shift
lprs([],[],[],[],[(S::Fs)|L],T,B,X,D) :- !,
	length([(S::Fs)|L],N),
	lprs([[S::Fs]/[]],[(S:Fs)/[]],[S/[]],[[([N=(S::Fs)],1,N,[],[N=S])::S]],L,T,B,X,D).
% shift if top element is projected
lprs([T0|Ts],Bs,Xs,[D0|Ds],[(S::Fs)|L],T,B,X,D) :- tprojected(T0), !,
	length([(S::Fs)|L],N),
	lprs([[S::Fs]/[],T0|Ts],[(S:Fs)/[]|Bs],[S/[]|Xs],[[([N=(S::Fs)],1,N,[],[N=S])::S],D0|Ds],L,T,B,X,D).
% reduce: only if T0 is not maximally projected
lprs([T0,T1|Ts],[B0,B1|Bs],[X0,X1|Xs],[D0,D1|Ds],L,T,B,X,D) :-
	tmerge(T0,T1,T2,B0,B1,B2,X0,X1,X2,D0,D1,D2), !,
	lprs([T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds],L,T,B,X,D).
lprs([T0|Ts],[B0|Bs],[X0|Xs],[D0|Ds],L,T,B,X,D) :-
	tmove(T0,T1,B0,B1,X0,X1,D0,D1), !,
	lprs([T1|Ts],[B1|Bs],[X1|Xs],[D1|Ds],L,T,B,X,D).

tprojected([_S:[F|_Fs]|_Chains]/_Ts) :- atomic(F).
tprojected([_S::[F|_Fs]|_Chains]/_Ts) :- atomic(F).

% tmerge1a with (lexical selector and) lexical selectee
tmerge([S0::[=C|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0)::S0],[(Head11,_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	append(S0,S1,S),
	NewExp=[S:Gamma],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[S1::[C]]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[X1]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[X0],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[X0],XSelected]
	),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Head00,Head11,Head01),
	NewDep=[(Head01,FN01,N0,LN,Y):S0].

% tmerge1b with (lexical selector and) derived selectee
tmerge([S0::[=C|Gamma]]/T0,[S1:[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0)::S0],[(Head11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S0,S1,S),
	NewExp=[S:Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[S1:[C]|Chains]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch done
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[X0],X1]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[X0],X1]
	),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Head00,Head11,Head01),
	NewDep=[(Head01,FN01,N0,LN,Y):S0|DChains].

% tmerge2a with (derived selector and) lexical selectee
tmerge([S0:[=C|Gamma]|Chains]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0):HN0|DChains],[(Head11,_FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	append(S1,S0,S),
	NewExp=[S:Gamma|Chains],
	smc(NewExp),
	Ts=[[S0:[=C|Gamma]|Chains]/T0,[S1::[C]]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[X1]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    X=SelectorP/[XSelected,X0]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[XSelected,X0]
	),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head11,Head00,Head10),
	NewDep=[(Head10,FN01,N0,LN,Y):HN0|DChains].

% tmerge2b with (derived selector and) derived selectee
tmerge([S0:[=C|Gamma]|Chains1]/T0,[S1:[C]|Chains2]/T1,NewExp/Ts,
	B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0):HN0|DChains1],[(Head11,_FN1,N1,LN1,Y1):_HN1|DChains2],NewDep
       ) :- !,
	append(S1,S0,S),
	append(Chains1,Chains2,Chains),
	NewExp=[S:Gamma|Chains],
	smc(NewExp),
	Ts=[[S0:[=C|Gamma]|Chains1]/T0,[S1:[C]|Chains2]/T1], %27 Mar 02
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch done
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    X=SelectorP/[X1,X0]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[X1,X0]
	),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(DChains1,DChains2,DChains),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head11,Head00,Head10),
	NewDep=[(Head10,FN01,N0,LN,Y):HN0|DChains].

% tmerge3a with lexical selector, lexical selectee
tmerge([S0::[=C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0)::S0],[(Head11,FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	NewExp=[S0:Gamma,S1:[Req|Delta]],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[X1]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[X0],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[X0],XSelected]
	),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[(Head00,FN01,N0,LN,Y0):S0,(Head11,FN11,N1,LN1,Y1):S1].

% tmerge3b with lexical selector, derived selectee
tmerge([S0::[=C|Gamma]]/T0,[S1:[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0)::S0],[(Head11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
	NewExp=[S0:Gamma,S1:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[S1:[C,Req|Delta]|Chains]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch done
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[X0],X1]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[X0],X1]
	),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[(Head00,FN01,N0,LN,Y0):S0,(Head11,FN11,N1,LN1,Y1):HN1|DChains].

% tmerge3c with derived selector, lexical selectee
tmerge([S0:[=C|Gamma]|Chains]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0):HN0|DChains],[(Head11,FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	append([S0:Gamma|Chains],[S1:[Req|Delta]],NewExp),
	smc(NewExp),
	Ts=[[S0:[=C|Gamma]|Chains]/T0,[S1::[C,Req|Delta]]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[X1]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    X=SelectorP/[XSelected,X0]
	;   concat(Selector0,'''',Selector1),
    	    X=Selector1/[XSelected,X0]
	),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append([(Head00,FN01,N0,LN,Y0):HN0|DChains],[(Head11,FN11,N1,LN1,Y1):S1],NewDep).

% tmerge3d with derived selector, derived selectee
tmerge([S0:[=C|Gamma]|Chains1]/T0,[S1:[C,Req|Delta]|Chains2]/T1,NewExp/Ts,
	B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Head00,FN0,N0,LN0,Y0):HN0|DChains1],[(Head11,FN1,N1,LN1,Y1):HN1|DChains2],NewDep
      ) :- !,
	append([S0:Gamma|Chains1],[S1:[Req|Delta]|Chains2],NewExp),
	smc(NewExp),
	Ts=[[S0:[=C|Gamma]|Chains1]/T0,[S1:[C,Req|Delta]|Chains2]/T1],
	check(_,B0,B00), check(_,B1,B01),
	% now the conventional tree...
	% right branch is done
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    X=SelectorP/[X1,X0]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[X1,X0]
	),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append([(Head00,FN01,N0,LN,Y0):HN0|DChains1],[(Head11,FN11,N1,LN1,Y1):HN1|DChains2],NewDep).

% final move of -F
tmove([S0:[+F|Gamma]|Chains1]/T0,NewExp/Ts,B0,'>'/[MaxF,B],X0,X,
      [(Head00,FN0,N0,LN0,Y0):HN0|DChains1],NewDep
      ) :-
	nappend(Chains2,[S1:[-F]|Chains3], Chains1, Place), !,
	append(Chains2,Chains3,Chains),
	append(S1,S0,S),
	NewExp=[S:Gamma|Chains],
	smc(NewExp),
	Ts=[[S0:[+F|Gamma]|Chains1]/T0],
	deleteMax(-F,B0,B1,MaxF,X0,X1,YP),
	check(_,B1,B),
	% the conventional tree: attach YP to X1
	categoryOf(Gamma,Trigger0),
	( Gamma=[Trigger0|_] ->
	    concat(Trigger0,'P',TriggerP),
	    X=TriggerP/[YP,X1]
	;   concat(Trigger0,'''',Trigger1),
	    X=Trigger1/[YP,X1]
	),
	nappend(DChains2,[(Head11,_FN1,N1,LN1,Y1):_HN1|DChains3], DChains1, Place), !,
	append(DChains2,DChains3,DChains),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head11,Head00,Head10),
	NewDep=[(Head10,FN01,N0,LN,Y):HN0|DChains].
%	append(Head11,Comp11,HC11),
%	append(Spec11,HC11,SHC11),
%	append(SHC11,Spec00,SHCSpec00),
%	NewDep=[(SHCSpec00,Head00,Comp00,FN01,N0,LN,Y):HN0|DChains].

% non-final move of -F
tmove([S0:[+F|Gamma]|Chains1]/T0,NewExp/Ts,B0,'>'/[MaxF,B],X0,X,
      [(Head00,FN0,N0,LN0,Y0):HN0|DChains1],NewDep
      ) :-
	nappend(Chains2,[S1:[-F,Req|Delta]|Chains3], Chains1, Place),
	append(Chains2,[S1:[Req|Delta]|Chains3], Chains4),
	NewExp=[S0:Gamma|Chains4],
	smc(NewExp),
	Ts=[[S0:[+F|Gamma]|Chains1]/T0],
	deleteMax(-F,B0,B1,MaxF,X0,X1,YP),
	check(_,B1,B),
	% the conventional tree: attach YP to X1
	categoryOf(Gamma,Trigger0),
	( Gamma=[Trigger0|_] ->
	    concat(Trigger0,'P',TriggerP),
	    X=TriggerP/[YP,X1]
	;   concat(Trigger0,'''',Trigger1),
	    X=Trigger1/[YP,X1]
	),
	nappend(DChains2,[(Head11,FN1,N1,LN1,Y1):HN1|DChains3], DChains1, Place),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append(DChains2,[(Head11,FN11,N1,LN1,Y1):HN1|DChains3], DChains4),
	NewDep=[(Head00,FN01,N0,LN,Y0):HN0|DChains4].

replaceXComp(YP/[Spec,Y1/[Y,Comp0]],YP/[Spec,Y1/[Y,Comp]],Comp0,Comp) :- maxxcat(YP), midxcat(Y1), !.
replaceXComp(YP/[Y1/[Y,Comp0]],YP/[Y1/[Y,Comp]],Comp0,Comp) :- maxxcat(YP), midxcat(Y1), !.
replaceXComp(Y1/[Y,Comp0],Y1/[Y,Comp],Comp0,Comp) :- midxcat(Y1), !.

%prob: replaceXSpec(YP/[Spec0,Y1/[Y,Comp]],YP/[Spec,Y1/[Y,Comp]],Spec0,Spec) :- maxxcat(YP), midxcat(Y1), !.
replaceXSpec(YP/[Spec0,Y1/[Y,Comp]],YP/[Spec,Y1/[Y,Comp]],Spec0,Spec) :- midxcat(Y1), !.

% deleteMax(F,T0,T,MaxP,X0,X,YP)=
%  delete the UNIQUE F MaxP in T0 to get T; delete YP in X0 to get X
%  (since F is unique, once we find it, we can cut)

% complement T1 is moved
deleteMax(F,'<'/[T0,T1],'<'/[T0,''/[]],T01,X0,X,Comp) :-
%	portray_clause(dM1),
	check(F,T1,T01),
	!,
	Trace=Root/[TI/[]],
	Comp0=Root/CompTs,
	Comp=RootI/CompTs,
	replaceXComp(X0,X,Comp0,Trace),
	buildTrace(Root,RootI,TI).

% extract from complement T1
deleteMax(F,'<'/[T0,T1],'<'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM2),
	replaceXComp(X0,X,Comp0,Comp),
	deleteMax(F,T1,T01,MaxP,Comp0,Comp,ZP), !.

% specifier T0 is moved
deleteMax(F,'>'/[T0,T1],'>'/[''/[],T1],T00,X0, X,Spec) :-
%	portray_clause(dM3),
	check(F,T0,T00), !,
	Trace=Root/[TI/[]],
	Spec0=Root/SpecTs,
	Spec=RootI/SpecTs,
	replaceXSpec(X0,X,Spec0,Trace),
	buildTrace(Root,RootI,TI).

% extract from spec T0 (a "left branch violation")
deleteMax(F,'>'/[T0,T1],'>'/[T00,T1],MaxP,X0,X,YP) :-
%	portray_clause(dM4),
	replaceXSpec(X0,X,Spec0,Spec),
	deleteMax(F,T0,T00,MaxP,Spec0,Spec,YP), !.

% extract from head T1 (when T1 is XP or X')
deleteMax(F,'>'/[T0,T1],'>'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM5),
	( X0=YP/[Spec,Y0], Y0=Y0Cat/_, midxcat(Y0Cat) -> X=YP/[Spec,Y1] ),
	deleteMax(F,T1,T01,MaxP,Y0,Y1,ZP), !.

% NOT NEEDED: extract from head T0 in '<'/[T0,T1]

% last char = '      danger! can be tricked
midxcat(t(_)) :- !, fail.
midxcat(X1) :- atomic(X1), name(X1,List),
	last(List,39).		%sicstus
%	last(39,List). % old version of pl

% last char = P      danger! can be tricked
maxxcat(t(_)) :- !, fail.
maxxcat(MovedXP) :- MovedXP=..[_MaxCat,_Trace], !.
maxxcat(X1) :- name(X1,List),
%	last(80,List). %old version of pl
	last(List,80). %sicstus, new pl

showLexicon :- numlex(L), writeLexs(L).

% numlist(+LexItems,+Pairs(No=Item),-Numlist)
numlist([],_,[]).
numlist([H|T],L,[N|NT]) :- memberchk(N=H,L), numlist(T,L,NT).

% list(+Numlist,+Pairs(No=Item),-LexItems)
listnum([],_,[]).
listnum([N|NT],L,[H|T]) :- memberchk(N=H,L), numlist(T,L,NT).

buildTrace(Root,RootI,TI) :-
	( atomic(Root) -> gensym(I), TI=..[t,I], nonvar(Root), RootI=..[Root,I]
	; nonvar(Root) -> RootI=Root, RootI=..[_,I], TI=..[t,I]
	; true
	).

checkall((S:_)/T,(S:[])/T).
checkall('<'/[T0,T1],'<'/[T00,T1]) :- checkall(T0,T00).
checkall('>'/[T0,T1],'>'/[T0,T01]) :- checkall(T1,T01).

% utilities for head movement in bare tree:

% check(F,T0,T)=delete the first feature F of the head of T0 to get T
check(F,(S:[F|Fs])/[],(S:Fs)/[]).
check(F,'<'/[T0,T1],'<'/[T00,T1]) :- check(F,T0,T00).
check(F,'>'/[T0,T1],'>'/[T0,T01]) :- check(F,T1,T01).

:- dynamic gCntr2/1.
gensym2(B) :- var(B),  ( retract(gCntr2(C)) ; C=0 ), B is C+1, asserta(gCntr2(B)), !.
gensym2(_).

dep_tree(L0) :- 
	dep_string(L0,String0,L1),
	sort(String0,String),
	sort(L1,L),
	tell('dotdep.dot'),
	write('Digraph G {'),nl,
	write('	node [peripheries=0,fontsize=10,height=0.1];'),nl,
	write('	edge [style=solid,arrowhead=1];'),nl,
	write('		ordering=out;'),nl,
	write('		ranksep=0.3;'),nl,
	write('		nodesep=0.3;'),nl,
	string_dot(String),
	length(String,N),
	write('	{rank=same'),
	dot_freeze_string(0,N),
	feature_dot(L),
	write('}'),nl,
	told.

% order ROYGBV: R B G Y O V  and BLACK=trouble
dep_color(1,'"1,1,1"') :- !.             %red
dep_color(2,'"0.640,1,1"') :- !.         %blue
dep_color(3,'"0.3,1,0.8"') :- !.         %green
dep_color(4,'"0.175,1,1"') :- !.         %yellow
dep_color(5,'"0.108,1,1"') :- !.         %orange
dep_color(6,'"0.833,0.454,0.933"') :- !. %violet
dep_color(_,black).

feature_dot([]).
feature_dot([((Node0:Feature)->Node)|L]) :- !,
	dep_color(Feature,Color),
	write('	n'),write(Node0),write('->n'),write(Node),write(' [color='),write(Color),write('];'),nl,
	feature_dot(L).
feature_dot([What|L]) :- !, portray_clause(user,'ERROR':feature_dot([What|L])),abort.

dot_freeze_string(N,N) :- !, write('};'),nl, dot_prec_string(1,N).
dot_freeze_string(N0,N) :- N1 is N0+1, write('; n'),write(N1), dot_freeze_string(N1,N).

dot_prec_string(N,N) :- !.
dot_prec_string(N0,N) :-
	N1 is N0+1, write('	n'),write(N0),write('->n'),write(N1),
	write('[arrowhead=none,style=invis,weight=100];'),nl,
	dot_prec_string(N1,N).

string_dot([]).
string_dot([(N:W)|L]) :-
	write('	'),write(n),write(N),write(' [label="'),write(W),write('"];'),nl,
	string_dot(L).

dep_string([],[],[]).
dep_string([(N:W)|L0],[N:W|String],L) :- !,dep_string(L0,String,L).
dep_string([F|L0],String,[F|L]) :- !,dep_string(L0,String,L).

featuresOut([N=(S::_Fs)|Ns],[N=S|OrderedPhon]) :- featuresOut(Ns,OrderedPhon).
featuresOut([],[]).

%numsOut(NWords,Words)
numsOut([],[]).
numsOut([_=W|NWs],L) :- append(W,Ws,L), numsOut(NWs,Ws).

writeLexs([]).
writeLexs([No=Lex|L]) :- write(No),write('. '),write(Lex),nl,writeLexs(L).
