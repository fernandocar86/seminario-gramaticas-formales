% file   : lhapx.pl  (swi version)
% origin author : E Stabler
% origin date: Feb 2002, based on lhp.pl
% various fixes from earlier versions: Willemijn Vermaat
% purpose: l(exical sequence) p(arser with) h(ead movement) and a(djunction) (e)x(tended to various formats) 
% todo:

pronounce(NumLexItems,Words) :- pronounce(NumLexItems,Words,_).
pronounce(NumLexItems,Words,(NWords,Links)) :-
	lparse(NumLexItems,_T,_B,_X,_LexItems,(NWords,Links)),
	numsOut(NWords,Words).

lparse(NumLexItems,T,B,X,LexItems,D) :-
	retractall(gCntr(_)), % fresh numbering of lexical items
	numlex(NumLexicon),
	listnum(NumLexItems,NumLexicon,LexItems),
	% portray_clause(x(LexItems)),
	reverse(LexItems,RYield),
	retractall(gCntr(_)), % fresh numbering of nontrivial chains
	lprs([],[],[],[],RYield,T,B,X,Deps),
	Deps=[[(Spec,Head,Comp,_FeatureNo,_NodeLex,Links,_StringComponents):_String]],
	append(Head,Comp,HeadComp),
	append(Spec,HeadComp,SpecHeadComp),
	featuresOut(SpecHeadComp,Phon),
	D=(Phon,Links).

x(LexItems) :-
	reverse(LexItems,RYield),
	retractall(gCntr(_)), % fresh numbering of nontrivial chains
	lprs([],[],[],[],RYield,T,B,X,Deps),
	Deps=[[(Spec,Head,Comp,_FeatureNo,_NodeLex,Links,_StringComponents):_String]],
	append(Head,Comp,HeadComp),
	append(Spec,HeadComp,SpecHeadComp),
	featuresOut(SpecHeadComp,Phon),
	D=(Phon,Links),
	step(T,B,X,LexItems,D).

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
	everystep(lexicon,[[S::Fs]/[]],[(S:Fs)/[]],[S/[]],[[([],[N=(S::Fs)],[],1,N,[],[N=S])::S]]),
	lprs([[S::Fs]/[]],[(S:Fs)/[]],[S/[]],[[([],[N=(S::Fs)],[],1,N,[],[N=S])::S]],L,T,B,X,D).
% conjoin
%lprs([(S0::Fs)/T0,(S1::[coord])/[],(S2::Fs)/T2|Ts],[B0,_B1,B2|Bs],[X0,_X1,X2|Xs],['&'(Fs)|L],T,B,X) :-
%	tcoord((S0::Fs)/T0,S1,(S2::Fs)/T2,T3,B0,B2,B3,X0,X2,X3),
%	lprs([T3|Ts],[B3|Bs],[X3|Xs],L,T,B,X).
% adjoin
lprs([T0,T1|Ts],[B0,B1|Bs],[X0,X1|Xs],[D0,D1|Ds],['>>'|L],T,B,X,D) :-
	tleftadjoinable(T0,T1), !,
	tleftadjoin(T0,T1,T2,B0,B1,B2,X0,X1,X2,D0,D1,D2),
	everystep(leftAdjoin,[T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds]),
	lprs([T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds],L,T,B,X,D).
lprs([T0,T1|Ts],[B0,B1|Bs],[X0,X1|Xs],[D0,D1|Ds],['<<'|L],T,B,X,D) :-
	trightadjoinable(T0,T1), !,
	trightadjoin(T0,T1,T2,B0,B1,B2,X0,X1,X2,D0,D1,D2),
	everystep(rightAdjoin,[T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds]),
	lprs([T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds],L,T,B,X,D).
% shift if top element is projected (and no further adjunctions)
lprs([T0|Ts],Bs,Xs,[D0|Ds],[(S::Fs)|L],T,B,X,D) :- tprojected(T0), !,
	length([(S::Fs)|L],N),
	everystep(lexicon,[[S::Fs]/[],T0|Ts],[(S:Fs)/[]|Bs],[S/[]|Xs],[[([],[N=(S::Fs)],[],1,N,[],[N=S])::S],D0|Ds]),
	lprs([[S::Fs]/[],T0|Ts],[(S:Fs)/[]|Bs],[S/[]|Xs],[[([],[N=(S::Fs)],[],1,N,[],[N=S])::S],D0|Ds],L,T,B,X,D).
% reduce: only if T0 is not maximally projected
lprs([T0,T1|Ts],[B0,B1|Bs],[X0,X1|Xs],[D0,D1|Ds],L,T,B,X,D) :-
	tmerge(T0,T1,T2,B0,B1,B2,X0,X1,X2,D0,D1,D2), !,
	everystep(merge,[T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds]),
	lprs([T2|Ts],[B2|Bs],[X2|Xs],[D2|Ds],L,T,B,X,D).
lprs([T0|Ts],[B0|Bs],[X0|Xs],[D0|Ds],L,T,B,X,D) :-
	tmove(T0,T1,B0,B1,X0,X1,D0,D1), !,
	everystep(move,[T1|Ts],[B1|Bs],[X1|Xs],[D1|Ds]),
	lprs([T1|Ts],[B1|Bs],[X1|Xs],[D1|Ds],L,T,B,X,D).

tleftadjoinable([Root0|_]/_,[Root1|_]/_) :-
	arg(2,Root0,[Cat0|Gamma]), arg(2,Root1,[Cat1|EtaNu]),
	current_predicate(>>,_),
	([Cat0|Gamma]>>[Cat1|Eta]),
	append(Eta,_Nu,EtaNu).
trightadjoinable([Root0|_]/_,[Root1|_]/_) :-
	arg(2,Root0,[Cat0|Gamma]), arg(2,Root1,[Cat1|EtaNu]),
	current_predicate(<<,_),
	([Cat1|Eta]<<[Cat0|Gamma]),
	append(Eta,_Nu,EtaNu).

tprojected([_S:[F|_Fs]|_Chains]/_Ts) :- atomic(F).
tprojected([_S::[F|_Fs]|_Chains]/_Ts) :- atomic(F).

% tleftadjoin1a (lex adjunct,lex head)
tleftadjoin([S0::[Cat0|Gamma]]/T0,[S1::[Cat1|EtaNu]]/T1,NewExp/Ts,
       B0,B1,'>>'/[B00,B1],X0,X1,XP,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],_FN1,N1,LN1,Y1)::S1],NewDep
	    ) :- !,
	NewExp=[(S0,S1,[]):[Cat1|EtaNu]],
	Ts=[[S0::[Cat0|Gamma]]/T0,[S1::[Cat1|EtaNu]]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	concat(Cat0,'P',LeftAdjunctP),
	concat(Cat0,'''',LeftAdjunct1),
	concat(Cat1,'P',HostP),
	concat(Cat1,'''',Host1),
	XP=HostP/[LeftAdjunctP/[LeftAdjunct1/[Cat0/[X0]]],HostP/[Host1/[Cat1/[X1]]]],
	append([((N0:FN0)->lm(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[Head00,Head11],[],FN01,N0,LN,Y):S0].

% tleftadjoin1b (lex adjunct,derived head)
tleftadjoin([S0::[Cat0|Gamma]]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains]/T1,NewExp/Ts,
       B0,B1,'>>'/[B00,B1],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep):-
	append(S0,Spec1,S0Spec1),
	NewExp=[(S0Spec1,S1,Comp1):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[S0::[Cat0|Gamma]]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	categoryOf([Cat0|Gamma],Adjunct0),
	concat(Adjunct0,'P',AdjunctP),
	concat(Adjunct0,'''',Adjunct1),
	X1=X1Cat/_,
	X=X1Cat/[AdjunctP/[Adjunct1/[Adjunct0/[X0]]],X1],
	append([((N0:FN0)->lm(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Head00,Spec11,HS01),
	NewDep=[(HS01,Head11,Comp11,FN01,N0,LN,Y):S0|DChains].

% tleftadjoin1c (derived adjunct,lex head)
tleftadjoin([(Spec0,S0,Comp0):[Cat0|Gamma]|Chains]/T0,[S1::[Cat1|EtaNu]]/T1,NewExp/Ts,
       B0,B1,'>>'/[B00,B1],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):_HN0|DChains],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep
	   ) :- !,
	append(S0,Comp0,SComp0),
	append(Spec0,SComp0,SpecSComp0),
	NewExp=[(SpecSComp0,S1,[]):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[Cat0|Gamma]]/T0,[S1::[Cat1|EtaNu]|Chains]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	categoryOf([Cat1|EtaNu],Cat1),
	concat(Cat1,'P',HeadP),
	concat(Cat1,'''',Head1),
	X=HeadP/[X0,HeadP/[Head1/[Cat1/[X1]]]],
	append([((N0:FN0)->lm(N1))|LN0],LN1,LN),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head00,Comp00,HC00), append(Spec00,HC00,SHC00),
	NewDep=[(SHC00,Head11,[],FN01,N0,LN,Y):S1|DChains].

% tleftadjoin1d (derived adjunct,derived head)
tleftadjoin([(Spec0,S0,Comp0):[Cat0|Gamma]|Chains0]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains1]/T1,NewExp/Ts,
       B0,B1,'>>'/[B00,B1],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains2],NewDep
	   ) :- !,
	append(S0,Comp0,SComp0),
	append(Spec0,SComp0,SpecSComp0),
	append(SpecSComp0,Spec1,S0Spec1),
	append(Chains0,Chains1,Chains),
	NewExp=[(S0Spec1,S1,Comp1):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0)::[Cat0|Gamma]|Chains0]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains1]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	X1=X1Head/_,
	X=X1Head/[X0,X1],
	append([((N0:FN0)->lm(N1))|LN0],LN1,LN),
	append(DChains1,DChains2,DChains),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Comp00,Spec11,CS01),append(Head00,CS01,HCS01),append(Spec00,HCS01,SHCS01),
	NewDep=[(SHCS01,Head11,Comp11,FN01,N0,LN,Y):HN0|DChains].

% trightadjoin1a (lex adjunct,lex head)
trightadjoin([S0::[Cat0|Gamma]]/T0,[S1::[Cat1|EtaNu]]/T1,NewExp/Ts,
       B0,B1,'<<'/[B00,B1],X0,X1,XP,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],_FN1,N1,LN1,Y1)::S1],NewDep
	    ) :- !,
%	portray_clause(blip1a),trace,
	NewExp=[([],S1,S0):[Cat1|EtaNu]],
	Ts=[[S0::[Cat0|Gamma]]/T0,[S1::[Cat1|EtaNu]]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	% right branch first
	concat(Cat0,'P',RightAdjunctP),
	concat(Cat0,'''',RightAdjunct1),
	concat(Cat1,'P',HostP),
	concat(Cat1,'''',Host1),
	XP=HostP/[HostP/[Host1/[Cat1/[X1]]],RightAdjunctP/[RightAdjunct1/[Cat0/[X0]]]],
	append([((N0:FN0)->rm(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[Head11,Head00],[],FN01,N0,LN,Y):S0].

% trightadjoin1b (lex adjunct,derived head)
trightadjoin([S0::[Cat0|Gamma]]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains]/T1,NewExp/Ts,
       B0,B1,'<<'/[B00,B1],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
	) :- !,
%	portray_clause(blip1b),
	append(Comp1,S0,Comp1S0),
	NewExp=[(Spec1,S1,Comp1S0):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[S0::[Cat0|Gamma]]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	% right branch done
	% left branch and tree assembly
	categoryOf([Cat0|Gamma],Adjunct0),
	concat(Adjunct0,'P',AdjunctP),
	concat(Adjunct0,'''',Adjunct1),
	X1=X1Cat/_,
	X=X1Cat/[X1,AdjunctP/[Adjunct1/[Adjunct0/[X0]]]],
	append([((N0:FN0)->rm(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Comp11,Head00,CH10),
	NewDep=[(Spec11,Head11,CH10,FN01,N0,LN,Y):S0|DChains].

% trightadjoin1c (der adjunct,lex head)
trightadjoin([(Spec0,S0,Comp0):[Cat0|Gamma]|Chains]/T0,[S1::[Cat1|EtaNu]]/T1,NewExp/Ts,
       B0,B1,'<<'/[B00,B1],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):_HN0|DChains],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep
	     ) :- !,
	append(S0,Comp0,SComp0),
	append(Spec0,SComp0,SpecSComp0),
	NewExp=[([],S1,SpecSComp0):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[Cat0|Gamma]|Chains]/T0,[S1::[Cat1|EtaNu]]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	% right branch done
	% left branch and tree assembly
	categoryOf([Cat1|EtaNu],Cat1),
	concat(Cat1,'P',HeadP),
	concat(Cat1,'''',Head1),
	X=HeadP/[HeadP/[Head1/[Cat1/[X1]]],X0],
	append([((N0:FN0)->rm(N1))|LN0],LN1,LN),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head00,Comp00,HC00), append(Spec00,HC00,SHC00),
	NewDep=[([],Head11,SHC00,FN01,N0,LN,Y):S1|DChains].

% trightadjoin1d (der adjunct,der head)
trightadjoin([(Spec0,S0,Comp0):[Cat0|Gamma]|Chains0]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains1]/T1,NewExp/Ts,
       B0,B1,'<<'/[B00,B1],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains2],NewDep
	    ) :- !,
	append(S0,Comp0,SComp0),
	append(Spec0,SComp0,SpecSComp0),
	append(Comp1,SpecSComp0,Comp1SpecSComp0),
	append(Chains0,Chains1,Chains),
	NewExp=[(Spec1,S1,Comp1SpecSComp0):[Cat1|EtaNu]|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[Cat0|Gamma]|Chains0]/T0,[(Spec1,S1,Comp1):[Cat1|EtaNu]|Chains1]/T1],
	checkall(B0,B00),  %EPS 
	% now the conventional tree...
	X1=X1Head/_,
	X=X1Head/[X1,X0],
	append([((N0:FN0)->rm(N1))|LN0],LN1,LN),
	append(DChains1,DChains2,DChains),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head00,Comp00,HC00), append(Spec00,HC00,SHC00),append(Comp11,SHC00,CSHC10),
	NewDep=[(Spec11,Head11,CSHC10,FN01,N0,LN,Y):HN0|DChains].

% tmerge1a with (lexical selector and) lexical selectee
tmerge([S0::[=C|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	NewExp=[([],S0,S1):Gamma],
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
	NewDep=[([],Head00,Head11,FN01,N0,LN,Y):S0].

% tmerge1b with (lexical selector and) derived selectee
tmerge([S0::[=C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S1,Comp1,SComp1),
	append(Spec1,SComp1,SpecSComp1),
	NewExp=[([],S0,SpecSComp1):Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1],
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
	append(Head11,Comp11,HC11),append(Spec11,HC11,SHC11),
	NewDep=[([],Head00,SHC11,FN01,N0,LN,Y):S0|DChains].

% tmerge1-r1left-a with (lexical selector and) lexical selectee
tmerge([S0::[=>C|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	append(S1,S0,S),
	NewExp=[([],S,[]):Gamma],
	smc(NewExp),
	Ts=[[S0::[=>C|Gamma]]/T0,[S1::[C]]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndLeftAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first -- now empty because head moving left
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[t/[]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[C/[X1],Selector0/[X0]],XSelected]] %EPSbroX0
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[C/[X1],Selector0/[X0]],XSelected]
	),
	append([((N0:FN0)->li(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[Head11|Head00],[],FN01,N0,LN,Y):S0].

% tmerge1-left1-b with (lexical selector and) derived selectee
tmerge([S0::[=>C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S1,S0,SS10),
	append(Spec1,Comp1,SpecComp1),
	NewExp=[([],SS10,SpecComp1):Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[=>C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndLeftAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpDeleteHead(X1,Selected,XHead),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[XHead,Selector0/[X0]],Selected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[XHead,Selector0/[X0]],Selected]
	),
	append([((N0:FN0)->li(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Spec11,Comp11,SC11),
	append(Head11,Head00,Heads01),
	NewDep=[([],Heads01,SC11,FN01,N0,LN,Y):S0|DChains].

% tmerge1-r1right-a with (lexical selector and) lexical selectee
tmerge([S0::[C<=|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	append(S0,S1,S),
	NewExp=[([],S,[]):Gamma],
	smc(NewExp),
	Ts=[[S0::[C<=|Gamma]]/T0,[S1::[C]]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndRightAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[t/[]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[Selector0/[X0],C/[X1]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[Selector0/[X0],C/[X1]],XSelected]
	),
	append([((N0:FN0)->ri(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[Head00|Head11],[],FN01,N0,LN,Y):S0].

% WV: alteration of r1-right, to keep complements and specifiers in same position and not stacked.
% tmerge1-right1-b with (lexical selector and) derived selectee
tmerge([S0::[C<=|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S0,S1,SS01),
	append(Spec1,Comp1,SpecComp1),
	NewExp=[([],SS01,SpecComp1):Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[C<=|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndRightAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpDeleteHead(X1,Selected,XHead),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[Selector0/[X0],XHead],Selected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[Selector0/[X0],XHead],Selected]
	),
	append([((N0:FN0)->ri(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Spec11,Comp11,SC11),
	NewDep=[([],[Head00|Head11],SC11,FN01,N0,LN,Y):S0|DChains].

% tmerge1-r1hopleft-a with (lexical selector and) lexical selectee
tmerge([S0::[<==C|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	append(S0,S1,S),
	NewExp=[([],[],S):Gamma],
	smc(NewExp),
	Ts=[[S0::[<==C|Gamma]]/T0,[S1::[C]]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndLeftAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- now empty because head moving left
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[Selector0/[X0],C/[X1]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	append([((N0:FN0)->hl(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[],[Head00|Head11],FN01,N0,LN,Y):S0].

% tmerge1-hopleft1-b with (lexical selector and) derived selectee
tmerge([S0::[<==C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S1,Comp1,SComp1),
	append(S0,SComp1,SSComp01),
	append(Spec1,SSComp01,SpecSSComp101),
	NewExp=[([],[],SpecSSComp101):Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[<==C|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndLeftAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpLeftAdjoinHead(X1,XSelected,Selector0/[X0]),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	append([((N0:FN0)->hl(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append([Head00|Head11],Comp11,HHC11),
	append(Spec11,HHC11,SHHC11),
	NewDep=[([],[],SHHC11,FN01,N0,LN,Y):S0|DChains].

% tmerge1-r1hopright-a with (lexical selector and) lexical selectee
tmerge([S0::[C==>|Gamma]]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],_FN1,N1,LN1,Y1)::S1],NewDep) :- !,
	append(S1,S0,S),
	NewExp=[([],[],S):Gamma],
	smc(NewExp),
	Ts=[[S0::[C==>|Gamma]]/T0,[S1::[C]]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndRightAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[C/[X1],Selector0/[X0]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
%	    X=SelectorP/[Selector1/[XSelected,Selector0/[t/[]]]] %eps Feb 2002 this line and down 2
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	append([((N0:FN0)->hr(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	NewDep=[([],[],[Head11,Head00],FN01,N0,LN,Y):S0].

% tmerge1-hopright1-b with (lexical selector and) derived selectee
tmerge([S0::[C==>|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains],NewDep
      ) :- !,
	append(S0,Comp1,SComp01),
	append(S1,SComp01,SSComp101),
	append(Spec1,SSComp101,SpecSSComp1101),
	NewExp=[([],[],SpecSSComp1101):Gamma|Chains],
	smc(NewExp),
	Ts=[[S0::[C==>|Gamma]]/T0,[(Spec1,S1,Comp1):[C]|Chains]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndRightAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpRightAdjoinHead(X1,XSelected,Selector0/[X0]),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	append([((N0:FN0)->hr(N1))|LN0],LN1,LN),
	append(Y0,Y1,Y),
	FN01 is FN0+1,
	append(Head11,[Head00|Comp11],HHC11),
	append(Spec11,HHC11,SHHC11),
	NewDep=[([],[],SHHC11,FN01,N0,LN,Y):S0|DChains].

% tmerge2a with (derived selector and) lexical selectee
tmerge([(Spec0,S0,Comp0):[=C|Gamma]|Chains]/T0,[S1::[C]]/T1,NewExp/Ts,
       B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains],[([],Head11,[],_FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	append(S1,Spec0,SSpecS10),
	NewExp=[(SSpecS10,S0,Comp0):Gamma|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[=C|Gamma]|Chains]/T0,[S1::[C]]/T1],
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
	append(Head11,Spec00,HSpec00),
	NewDep=[(HSpec00,Head00,Comp00,FN01,N0,LN,Y):HN0|DChains].

% tmerge2b with (derived selector and) derived selectee
tmerge([(Spec0,S0,Comp0):[=C|Gamma]|Chains1]/T0,[(Spec1,S1,Comp1):[C]|Chains2]/T1,NewExp/Ts,
	B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains2],NewDep
       ) :- !,
	append(Comp1,Spec0,CompSpec10),
	append(S1,CompSpec10,SCompSpec10),
	append(Spec1,SCompSpec10,SpecSCompSpec10),
	append(Chains1,Chains2,Chains),
	NewExp=[(SpecSCompSpec10,S0,Comp0):Gamma|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[=C|Gamma]|Chains1]/T0,[(Spec1,S1,Comp1):[C]|Chains2]/T1], %27 Mar 02
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
	append(Comp11,Spec00,CSpec00),
	append(Head11,CSpec00,HCSpec00),
	append(Spec11,HCSpec00,SHCSpec00),
	NewDep=[(SHCSpec00,Head00,Comp00,FN01,N0,LN,Y):HN0|DChains].

% tmerge3a with lexical selector, lexical selectee
tmerge([S0::[=C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
%EPS	NewExp=[([],S0,[]):Gamma,([],S1,[]):[Req|Delta]],
	NewExp=[([],S0,[]):Gamma,S1:[Req|Delta]],
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
	NewDep=[([],Head00,[],FN01,N0,LN,Y0):S0,([],Head11,[],FN11,N1,LN1,Y1):S1].

% tmerge3b with lexical selector, derived selectee
tmerge([S0::[=C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
	append2((Spec1,S1,Comp1),S1appended),
%EPS	NewExp=[([],S0,[]):Gamma,(Spec1,S1,Comp1):[Req|Delta]|Chains],
	NewExp=[([],S0,[]):Gamma,S1appended:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[=C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1],
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
	NewDep=[([],Head00,[],FN01,N0,LN,Y0):S0,(Spec11,Head11,Comp11,FN11,N1,LN1,Y1):HN1|DChains].

% tmerge3c with derived selector, lexical selectee
tmerge([S0:[=C|Gamma]|Chains]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains],[([],Head11,[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
%EPS	append([S0:Gamma|Chains],[([],S1,[]):[Req|Delta]],NewExp),
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
	append([(Spec00,Head00,Comp00,FN01,N0,LN,Y0):HN0|DChains],[([],Head11,[],FN11,N1,LN1,Y1):S1],NewDep).

% tmerge3d with derived selector, derived selectee
tmerge([S0:[=C|Gamma]|Chains1]/T0,[S1:[C,Req|Delta]|Chains2]/T1,NewExp/Ts,
	B0,B1,'>'/[B01,B00],X0,X1,X,
       [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains2],NewDep
      ) :- !,
%EPS	append([S0:Gamma|Chains1],[S1:[Req|Delta]|Chains2],NewExp),
	append2(S1,S1appended),
	append([S0:Gamma|Chains1],[S1appended:[Req|Delta]|Chains2],NewExp),
	smc(NewExp),
	Ts=[[S0:[=C|Gamma]|Chains1]/T0,[S1:[C,Req|Delta]|Chains2]/T1],
%	portray_clause(newtree=NewExp/Ts),
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
	append([(Spec00,Head00,Comp00,FN01,N0,LN,Y0):HN0|DChains1],[(Spec11,Head11,Comp11,FN11,N1,LN1,Y1):HN1|DChains2],NewDep).

% WV: 06/21/01  added: r3-rules for head-merge and hop-merge rules

% tmerge3-r3right-a with lexical selector, lexical selectee
tmerge([S0::[C<=|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
        append(S0,S1,S),
%EPS	NewExp=[([],S,[]):Gamma,([],[],[]):[Req|Delta]],
	NewExp=[([],S,[]):Gamma,[]:[Req|Delta]],
	smc(NewExp),
	Ts=[[S0::[C<=|Gamma]]/T0,[S1::[C,Req|Delta]]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndRightAdjoinHead(_,B0,B00,Head), 
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
	    X=SelectorP/[Selector1/[Selector0/[Selector0/[X0],C/[X1]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[Selector0/[X0],C/[X1]],XSelected]
	),
	LN=[((N0:FN0)->ri(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[Head00|Head11],[],FN01,N0,LN,Y0):S0,([],[],[],FN11,N1,LN1,Y1):S1].

% tmerge3-rightr3-b with lexical selector and derived selectee
tmerge([S0::[C<=|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
        append(S0,S1,S),
%EPS	NewExp=[([],S,[]):Gamma,(Spec1,[],Comp1):[Req|Delta]|Chains],
	append(Spec1,Comp1,SpecComp1),
	NewExp=[([],S,[]):Gamma,SpecComp1:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[C<=|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndRightAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpDeleteHead(X1,Selected,XHead),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[Selector0/[X0],XHead],Selected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[Selector0/[X0],XHead],Selected]
	),
	LN=[((N0:FN0)->ri(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[Head00|Head11],[],FN01,N0,LN,Y0):S0,(Spec11,[],Comp11,FN11,N1,LN1,Y1):HN1|DChains].

% tmerge3-r3left-a with lexical selector, lexical selectee
tmerge([S0::[=>C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
        append(S1,S0,S),
%EPS	NewExp=[([],S,[]):Gamma,([],[],[]):[Req|Delta]],
	NewExp=[([],S,[]):Gamma,[]:[Req|Delta]],
	smc(NewExp),
	Ts=[[S0::[=>C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndLeftAdjoinHead(_,B0,B00,Head),
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
	    X=SelectorP/[Selector1/[Selector0/[C/[X1],Selector0/[X0]],XSelected]] %EPSbroX0
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[C/[X1],Selector0/[X0]],XSelected]
	),
	LN=[((N0:FN0)->li(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[Head11|Head00],[],FN01,N0,LN,Y0):S0,([],[],[],FN11,N1,LN1,Y1):S1].

% tmerge3-leftr3-b with lexical selector and derived selectee
tmerge([S0::[=>C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
        append(S1,S0,S),
%EPS	NewExp=[([],S,[]):Gamma,(Spec1,[],Comp1):[Req|Delta]|Chains],
	append(Spec1,Comp1,SpecComp1),
	NewExp=[([],S,[]):Gamma,SpecComp1:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[=>C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1],
	checkAndDeleteHead(_,B1,B01,Head), checkAndLeftAdjoinHead(_,B0,B00,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpDeleteHead(X1,Selected,XHead),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[XHead,Selector0/[X0]],Selected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[XHead,Selector0/[X0]],Selected]
	),
	LN=[((N0:FN0)->li(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append(Head11,Head00,Heads01),
	NewDep=[([],Heads01,[],FN01,N0,LN,Y0):S0,(Spec11,[],Comp11,FN11,N1,LN1,Y1):HN1|DChains].

% tmerge3-r3hopleft-a with lexical selector and lexical selectee
tmerge([S0::[<==C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[([],Head11,[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	append(S0,S1,S),
%EPS	NewExp=[([],[],[]):Gamma,([],S,[]):[Req|Delta]],
	NewExp=[([],[],[]):Gamma,S:[Req|Delta]],
	smc(NewExp),
	Ts=[[S0::[<==C|Gamma]]/T0,[S1::[C,Req|Delta]]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndLeftAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- now empty because head moving left
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[C/[X1],Selector0/[X0]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	LN=[((N0:FN0)->hl(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[],[],FN01,N0,LN,Y0):S0,([],[Head00|Head11],[],FN11,N1,LN1,Y1):S1].

% tmerge3-r3hopleft-b with lexical selector and derived selectee
tmerge([S0::[<==C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],[Head00],[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
	append(S0,S1,S),
%EPS	NewExp=[([],[],[]):Gamma,(Spec1,S,Comp1):[Req|Delta]|Chains],
	append2((Spec1,S,Comp1),SpecSComp1),
	NewExp=[([],[],[]):Gamma,SpecSComp1:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[<==C|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndLeftAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpLeftAdjoinHead(X1,XSelected,Selector0/[X0]),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	LN=[((N0:FN0)->hl(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[],[],FN01,N0,LN,Y0):S0,(Spec11,[Head00|Head11],Comp11,FN11,N1,LN1,Y1):HN1|DChains].

% tmerge3-r3hopright-a with lexical selector and lexical selectee
% posssible variation: put the affix into the specifier
% or complement position
tmerge([S0::[C==>|Gamma]]/T0,[S1::[C,Req|Delta]]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[([],[Head11],[],FN1,N1,LN1,Y1)::S1],NewDep
       ) :- !,
	append(S1,S0,S),
%EPS	NewExp=[([],[],[]):Gamma,([],S,[]):[Req|Delta]],
	NewExp=[([],[],[]):Gamma,S:[Req|Delta]],
	smc(NewExp),
	Ts=[[S0::[C==>|Gamma]]/T0,[S1::[C,Req|Delta]]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndRightAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first
	concat(C,'P',SelectedP),
	concat(C,'''',Selected1),
	XSelected=SelectedP/[Selected1/[C/[C/[X1],Selector0/[X0]]]],
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[XSelected,Selector0/[t/[]]]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[XSelected,Selector0/[t/[]]]
	),
	LN=[((N0:FN0)->hr(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	NewDep=[([],[],[],FN01,N0,LN,Y0):S0,([],[Head11|Head00],[],FN11,N1,LN1,Y1):S1].

% tmerge3-r3hopright-b with lexical selector and derived selectee
% tmerge1-hopright1-b with (lexical selector and) derived selectee
tmerge([S0::[C==>|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1,NewExp/Ts,
       B0,B1,'<'/[B00,B01],X0,X1,X,
       [([],Head00,[],FN0,N0,LN0,Y0)::S0],[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains],NewDep
       ) :- !,
	append(S1,S0,S),
%EPS	NewExp=[([],[],[]):Gamma, (Spec1,S,Comp1):[Req|Delta]|Chains],
	append2((Spec1,S,Comp1),SpecSComp1),
	NewExp=[([],[],[]):Gamma, SpecSComp1:[Req|Delta]|Chains],
	smc(NewExp),
	Ts=[[S0::[C==>|Gamma]]/T0,[(Spec1,S1,Comp1):[C,Req|Delta]|Chains]/T1],
	checkAndDeleteHead(_,B0,B00,Head), checkAndRightAdjoinHead(_,B1,B01,Head), 
	% now the conventional tree...
	% right branch first -- we must delete head, to move it left
	xpRightAdjoinHead(X1,XSelected,Selector0/[X0]),
	% left branch and tree assembly
	categoryOf(Gamma,Selector0),
	( Gamma=[Selector0|_] ->
	    concat(Selector0,'P',SelectorP),
	    concat(Selector0,'''',Selector1),
	    X=SelectorP/[Selector1/[Selector0/[t/[]],XSelected]]
	;   concat(Selector0,'''',Selector1),
	    X=Selector1/[Selector0/[t/[]],XSelected]
	),
	LN=[((N0:FN0)->hr(N1))|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append(Head11,Head00,Heads01),
	NewDep=[([],[],[],FN01,N0,LN,Y0):S0,(Spec11,Heads01,Comp11,FN11,N1,LN1,Y1):HN1|DChains].

% final move of -F
tmove([(Spec0,S0,Comp0):[+F|Gamma]|Chains1]/T0,NewExp/Ts,B0,'>'/[MaxF,B],X0,X,
      [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],NewDep
      ) :-
%EPS	nappend(Chains2,[(Spec1,S1,Comp1):[-F]|Chains3], Chains1, Place), !,
	nappend(Chains2,[SpecSComp1:[-F]|Chains3], Chains1, Place), !,
	append(Chains2,Chains3,Chains),
%EPS	append(Comp1,Spec0,CompSpec10),
%	append(S1,CompSpec10,SCompSpec10),
%	append(Spec1,SCompSpec10,SpecSCompSpec10),
	append(SpecSComp1,Spec0,SpecSCompSpec10),
	NewExp=[(SpecSCompSpec10,S0,Comp0):Gamma|Chains],
	smc(NewExp),
	Ts=[[(Spec0,S0,Comp0):[+F|Gamma]|Chains1]/T0],
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
	nappend(DChains2,[(Spec11,Head11,Comp11,_FN1,N1,LN1,Y1):_HN1|DChains3], DChains1, Place), !,
	append(DChains2,DChains3,DChains),
	append([((N0:FN0)->N1)|LN0],LN1,LN),
	append(Y1,Y0,Y),
	FN01 is FN0+1,
	append(Head11,Comp11,HC11),
	append(Spec11,HC11,SHC11),
	append(SHC11,Spec00,SHCSpec00),
	NewDep=[(SHCSpec00,Head00,Comp00,FN01,N0,LN,Y):HN0|DChains].

% non-final move of -F
tmove([S0:[+F|Gamma]|Chains1]/T0,NewExp/Ts,B0,'>'/[MaxF,B],X0,X,
      [(Spec00,Head00,Comp00,FN0,N0,LN0,Y0):HN0|DChains1],NewDep
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
	nappend(DChains2,[(Spec11,Head11,Comp11,FN1,N1,LN1,Y1):HN1|DChains3], DChains1, Place),
	LN=[((N0:FN0)->N1)|LN0],
	FN01 is FN0+1,	FN11 is FN1+1,
	append(DChains2,[(Spec11,Head11,Comp11,FN11,N1,LN1,Y1):HN1|DChains3], DChains4),
	NewDep=[(Spec00,Head00,Comp00,FN01,N0,LN,Y0):HN0|DChains4].

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

deleteMax(F,'>>'/[T0,T1],'>>'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM6),
	X0=YP/[A,X1],X=YP/[A,X2],
	deleteMax(F,T1,T01,MaxP,X1,X2,ZP), !.
deleteMax(F,'>>'/[T0,T1],'>>'/[T00,T1],MaxP,X0,X,ZP) :-
%	portray_clause(dM7),
	X0=YP/[A1,XP],X=YP/[A2,XP],
	deleteMax(F,T0,T00,MaxP,A1,A2,ZP), !.

deleteMax(F,'<<'/[T0,T1],'<<'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM8),
	X0=YP/[X1,A],X=YP/[X2,A],
	deleteMax(F,T1,T01,MaxP,X1,X2,ZP), !.
deleteMax(F,'<<'/[T0,T1],'<<'/[T00,T1],MaxP,X0,X,ZP) :-
%	portray_clause(dM9),
	X0=YP/[XP,A1],X=YP/[XP,A2],
	deleteMax(F,T0,T00,MaxP,A1,A2,ZP), !.

% NOT NEEDED: extract from head T0 in '<'/[T0,T1]

% extract from head of left adjunction structure T1 (head is right in bare tree and in Xtree)
deleteMax(F,'>>'/[T0,T1],'>>'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM10),
	( X0=YP/[Adj,YP/[Spec,Y1/[Y0,Comp0]]], midxcat(Y1) ->  X=YP/[Adj,YP/[Spec,Y1/[Y0,Comp]]]
	; X0=YP/[Adj,YP/[Y1/[Y0,Comp0]]], midxcat(Y1) ->  X=YP/[Adj,YP/[Y1/[Y0,Comp]]]
	),
	deleteMax(F,T1,T01,MaxP,Comp0,Comp,ZP), !.

% extract from head of right adjunction structure T1  (head is right in bare tree and LEFT in Xtree)
deleteMax(F,'<<'/[T0,T1],'<<'/[T0,T01],MaxP,X0,X,ZP) :-
%	portray_clause(dM11),
	( X0=YP/[YP/[Spec,Y1/[Y0,Comp0]],Adj], midxcat(Y1) ->  X=YP/[YP/[Spec,Y1/[Y0,Comp]],Adj]
	; X0=YP/[YP/[Y1/[Y0,Comp0]],Adj], midxcat(Y1) ->  X=YP/[YP/[Y1/[Y0,Comp]],Adj]
	),
	deleteMax(F,T1,T01,MaxP,Comp0,Comp,ZP), !.

% last char = '      danger! can be tricked
midxcat(t(_)) :- !, fail.
midxcat(X1) :- atomic(X1), name(X1,List),
	% sicstus last(List,39). 
	%last(39,List).
	last(List,39). 

% last char = P      danger! can be tricked
maxxcat(t(_)) :- !, fail.
maxxcat(MovedXP) :- MovedXP=..[_MaxCat,_Trace], !.
maxxcat(X1) :- name(X1,List),
	% sicstus last(List,80). 
	%last(80,List).
	last(List,80). 

radjunctCat(XP) :- atomic(XP), name(XP,List), append(CatL,[80],List), name(X,CatL),current_predicate(<<,_),(_<<[X]).
ladjunctCat(XP) :- atomic(XP), name(XP,List), append(CatL,[80],List), name(X,CatL),current_predicate(>>,_),([X]>>_).

showLexicon :- numlex(L), writeLexs(L).

% numlist(+LexItems,+Pairs(No=Item),-Numlist)
numlist([],_,[]).
numlist(['>>'|T],L,['>>'|NT]) :- !, numlist(T,L,NT).
numlist(['<<'|T],L,['<<'|NT]) :- !, numlist(T,L,NT).
numlist([H|T],L,[N|NT]) :- memberchk(N=H,L), numlist(T,L,NT).

% list(+Numlist,+Pairs(No=Item),-LexItems)
listnum([],_,[]).
listnum(['>>'|T],L,['>>'|NT]) :- !, listnum(T,L,NT).
listnum(['<<'|T],L,['<<'|NT]) :- !, listnum(T,L,NT).
listnum([N|NT],L,[H|T]) :- memberchk(N=H,L), numlist(T,L,NT).

buildTrace(Root,RootI,TI) :-
	( atomic(Root) -> gensym(I), TI=..[t,I], nonvar(Root), RootI=..[Root,I]
	; nonvar(Root) -> RootI=Root, RootI=..[_,I], TI=..[t,I]
	; true
	).

checkall((S:_)/T,(S:[])/T).
checkall('<o'/[T0,T1],'<o'/[T00,T1]) :- checkall(T0,T00).
checkall('>>'/[T0,T1],'>>'/[T0,T01]) :- checkall(T1,T01). % in bare tree, modifiee on left, head on right
checkall('<<'/[T0,T1],'<<'/[T0,T01]) :- checkall(T1,T01). % in bare tree, modifiee on left, head on right
checkall('<'/[T0,T1],'<'/[T00,T1]) :- checkall(T0,T00).
checkall('>'/[T0,T1],'>'/[T0,T01]) :- checkall(T1,T01).
checkall('o>'/[T0,T1],'o>'/[T0,T01]) :- checkall(T1,T01).

% utilities for head movement in bare tree:

% check(F,T0,T)=delete the first feature F of the head of T0 to get T
check(F,(S:[F|Fs])/[],(S:Fs)/[]).
check(F,'<o'/[T0,T1],'<o'/[T00,T1]) :- check(F,T0,T00).
check(F,'>>'/[T0,T1],'>>'/[T0,T01]) :- check(F,T1,T01). % in bare tree, modifiee on left, head on right
check(F,'<<'/[T0,T1],'<<'/[T0,T01]) :- check(F,T1,T01). % in bare tree, modifiee on left, head on right
check(F,'<'/[T0,T1],'<'/[T00,T1]) :- check(F,T0,T00).
check(F,'>'/[T0,T1],'>'/[T0,T01]) :- check(F,T1,T01).
check(F,'o>'/[T0,T1],'o>'/[T0,T01]) :- check(F,T1,T01).

% utilities for head movement in bare tree:
checkAndDeleteHead(F,(S:[F|Fs])/[],([]:Fs)/[],S/[]).
checkAndDeleteHead(F,'<'/[T0,T1],'<'/[T00,T1],I) :- checkAndDeleteHead(F,T0,T00,I).
checkAndDeleteHead(F,'<o'/[T0,T1],([]:Fs)/[],'<o'/[T00,T1]) :- checkAndDeleteRemainder(F,T0,T00,Fs).
checkAndDeleteHead(F,'>'/[T0,T1],'>'/[T0,T01],I) :- checkAndDeleteHead(F,T1,T01,I).
checkAndDeleteHead(F,'o>'/[T0,T1],([]:Fs)/[],'o>'/[T0,T01]) :- checkAndDeleteRemainder(F,T1,T01,Fs).

checkAndDeleteHead(F,'>>'/[T0,T1],'>>'/[T0,T01],I) :- checkAndDeleteHead(F,T1,T01,I).
checkAndDeleteHead(F,'<<'/[T0,T1],'<<'/[T0,T01],I) :- checkAndDeleteHead(F,T1,T01,I).

checkAndLeftAdjoinHead(F,(String:[F|Fs])/[],'o>'/[I,(String:Fs)/[]],I).
checkAndLeftAdjoinHead(F,'<'/[T0,T1],'<'/[T00,T1],I) :- checkAndLeftAdjoinHead(F,T0,T00,I).
checkAndLeftAdjoinHead(F,'<o'/[T0,T1],'o>'/[I,'<o'/[T00,T1]],I) :- check(F,T0,T00).
checkAndLeftAdjoinHead(F,'>'/[T0,T1],'>'/[T0,T01],I) :- checkAndLeftAdjoinHead(F,T1,T01,I).
checkAndLeftAdjoinHead(F,'o>'/[T0,T1],'o>'/[I,'o>'/[T0,T01]],I) :- check(F,T1,T01).

checkAndLeftAdjoinHead(F,'>>'/[T0,T1],'>>'/[T0,T01],I) :- checkAndLeftAdjoinHead(F,T1,T01,I).
checkAndLeftAdjoinHead(F,'<<'/[T0,T1],'<<'/[T0,T01],I) :- checkAndLeftAdjoinHead(F,T1,T01,I).

checkAndRightAdjoinHead(F,(S:[F|Fs])/[],'<o'/[(S:Fs)/[],I],I).
checkAndRightAdjoinHead(F,'<'/[T0,T1],'<'/[T00,T1],I) :- checkAndRightAdjoinHead(F,T0,T00,I).
checkAndRightAdjoinHead(F,'<o'/[T0,T1],'<o'/['<o'/[T00,T1],I],I) :- check(F,T0,T00).
checkAndRightAdjoinHead(F,'>'/[T0,T1],'>'/[T0,T01],I) :- checkAndRightAdjoinHead(F,T1,T01,I).
checkAndRightAdjoinHead(F,'o>'/[T0,T1],'<o'/['o>'/[T0,T01],I],I) :- check(F,T1,T01).

checkAndRightAdjoinHead(F,'>>'/[T0,T1],'>>'/[T0,T01],I) :- checkAndRightAdjoinHead(F,T1,T01,I).
checkAndRightAdjoinHead(F,'<<'/[T0,T1],'<<'/[T0,T01],I) :- checkAndRightAdjoinHead(F,T1,T01,I).

checkAndDeleteRemainder(F,(S:[F|Fs])/[],(S:[])/[],Fs).
checkAndDeleteRemainder(F,'<o'/[T0,T1],'<o'/[T00,T1],Fs) :- checkAndDeleteRemainder(F,T0,T00,Fs).
checkAndDeleteRemainder(F,'o>'/[T0,T1],'o>'/[T0,T01],Fs) :- checkAndDeleteRemainder(F,T1,T01,Fs).

xpDeleteHead(XPa/[XP0,Adjunct/AdjP],XPa/[XP,Adjunct/AdjP],Head) :-
	radjunctCat(Adjunct),
	xpDeleteHead(XP0,XP,Head).
xpDeleteHead(XPa/[Adjunct/AdjP,XP0],XPa/[Adjunct/AdjP,XP],Head) :-
	ladjunctCat(Adjunct),
	xpDeleteHead(XP0,XP,Head).
xpDeleteHead(XP/[Spec,X1/X1Trees0],XP/[Spec,X1/X1Trees],Head) :-
	midxcat(X1),
	xpDeleteHead(X1/X1Trees0,X1/X1Trees,Head).
xpDeleteHead(XP/[X1/X1Trees0],XP/[X1/X1Trees],Head) :-
	midxcat(X1),
	xpDeleteHead(X1/X1Trees0,X1/X1Trees,Head).
xpDeleteHead(X1/[X0/X0Trees,Comp/CompTrees],X1/[X0/[t/[]],Comp/CompTrees],X0/X0Trees) :-
	midxcat(X1),
	maxxcat(Comp),
	\+midxcat(X0).
xpDeleteHead(X1/[X0/X0Trees],X1/[X0/[t/[]]],X0/X0Trees) :-
	midxcat(X1),
	\+midxcat(X0).

%xpLeftAdjoinHead(X1,XSelected,Selector0/[X0])
% for left adjunction structure
xpLeftAdjoinHead(XP/[AdjunctP/AdjT,XP/XPTs0],XP/[AdjunctP/AdjT,XP/XPTs],Head) :-
	maxxcat(AdjunctP),
	maxxcat(XP),!,
	xpLeftAdjoinHead(XP/XPTs0,XP/XPTs,Head).
% for right adjunction structure
xpLeftAdjoinHead(XP/[XP/XPTs0,AdjunctP/AdjT],XP/[XP/XPTs,AdjunctP/AdjT],Head) :-
	maxxcat(AdjunctP),
	maxxcat(XP),!,
	xpLeftAdjoinHead(XP/XPTs0,XP/XPTs,Head).
%
xpLeftAdjoinHead(XP/[Spec,X1/X1Trees0],XP/[Spec,X1/X1Trees],Head) :-
	midxcat(X1),
	xpLeftAdjoinHead(X1/X1Trees0,X1/X1Trees,Head).
xpLeftAdjoinHead(XP/[X1/X1Trees0],XP/[X1/X1Trees],Head) :-
	midxcat(X1),
	xpLeftAdjoinHead(X1/X1Trees0,X1/X1Trees,Head).
xpLeftAdjoinHead(X1/[X0/X0Trees,Comp/CompTrees],X1/[X0/[Head,X0/X0Trees],Comp/CompTrees],Head) :-
	midxcat(X1),
	maxxcat(Comp),
	\+midxcat(X0).
xpLeftAdjoinHead(X1/[X0/X0Trees],X1/[X0/[Head,X0/X0Trees]],Head) :-
	midxcat(X1),
	\+midxcat(X0).

% for left adjunction structure
xpRightAdjoinHead(XP/[AdjunctP/AdjT,XP/XPTs0],XP/[AdjunctP/AdjT,XP/XPTs],Head) :-
	maxxcat(AdjunctP),
	maxxcat(XP),!,
	xpRightAdjoinHead(XP/XPTs0,XP/XPTs,Head).
% for right adjunction structure
xpRightAdjoinHead(XP/[XP/XPTs0,AdjunctP/AdjT],XP/[XP/XPTs,AdjunctP/AdjT],Head) :-
	maxxcat(AdjunctP),
	maxxcat(XP),!,
	xpRightAdjoinHead(XP/XPTs0,XP/XPTs,Head).
% 
xpRightAdjoinHead(XP/[Spec,X1/X1Trees0],XP/[Spec,X1/X1Trees],Head) :-
	midxcat(X1),
	xpRightAdjoinHead(X1/X1Trees0,X1/X1Trees,Head).
xpRightAdjoinHead(XP/[X1/X1Trees0],XP/[X1/X1Trees],Head) :-
	midxcat(X1),
	xpRightAdjoinHead(X1/X1Trees0,X1/X1Trees,Head).
xpRightAdjoinHead(X1/[X0/X0Trees,Comp/CompTrees],X1/[X0/[X0/X0Trees,Head],Comp/CompTrees],Head) :-
	midxcat(X1),
	maxxcat(Comp),
	\+midxcat(X0).
xpRightAdjoinHead(X1/[X0/X0Trees],X1/[X0/[X0/X0Trees,Head]],Head) :-
	midxcat(X1),
	\+midxcat(X0).

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

%comment first clause if you want to see every step
everystep(_Type,_DerivationTrees,_BareTrees,_XbarTrees,_DepStuctLinks) :- !.
everystep(lexicon,[T|Ts],Bs,[X|Xs],Ds) :- !,
	remove_branches([T|Xs],BranchlessXs),
	user:latex_tree(lexicon/BranchlessXs),
	user:wish_tree(lexicon/BranchlessXs),
	stepx:prompted_char('more? ',C),
	(	C=97 /* a */ -> abort
	;	stepx:is_endline(C) /* carriage return */ -> true
	;	C=59 /* ; */ -> fail
	;	stepx:help0,everystep(lexicon,[T|Ts],Bs,[X|Xs],Ds)
	).
everystep(Type,Ts,Bs,Xs,Ds) :-
	remove_branches(Xs,BranchlessXs),
	user:latex_tree(Type/BranchlessXs),
	user:wish_tree(Type/BranchlessXs),
	stepx:prompted_char('more? ',C),
	(	C=97 /* a */ -> abort
	;	stepx:is_endline(C) /* carriage return */ -> true
	;	C=59 /* ; */ -> fail
	;	stepx:help0,everystep(Type,Ts,Bs,Xs,Ds)
	).

remove_branches([],[]).
remove_branches([(R/T)|Ts0],['$m'(R)/T|Ts]) :- remove_branches(Ts0,Ts).

append2((L1,L2,L3),L123) :- append(L2,L3,L23), append(L1,L23,L123).
