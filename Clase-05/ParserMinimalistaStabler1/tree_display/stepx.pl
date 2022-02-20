/* File : stepx.pl
 * E. Stabler
 */

:- module(stepx, [step/0,step/5,pstep/6,dep_tree0/2,dep_tree1/2]).

step :-
	prompted_char('more? ',C),
	(	C=97 /* a */ -> abort
	;	is_endline(C) /* carriage return */ -> true
	;	C=59 /* ; */ -> fail
	;	help0,step
	).

step(D,B,X,Lex,Deps) :- pstep(D,B,X,Lex,Deps,'visualizar (enter, o h para ayuda)? ').

pstep(D,B,X,Lex,Deps,Prompt) :-
	prompted_char(Prompt,C),
	(	is_endline(C) /* carriage return */ -> true
	;	C=59 /* ; */ -> fail
	;	C=100 /* d(erivation tree) */ -> 
			user:latex_tree(D),user:wish_tree(D),
			step(D,B,X,Lex,Deps)
	;	C=98 /* b(are tree) */ -> 
			user:latex_tree(B),user:wish_tree(B),
			step(D,B,X,Lex,Deps)
	;	C=120 /* x(-bar tree) */ -> 
			user:latex_tree(X),user:wish_tree(X),
			step(D,B,X,Lex,Deps)
	;	C=112 /* p (pp derivation tree) */ -> 
			user:latex_tree(D),user:pptree(D),
			step(D,B,X,Lex,Deps)
	;	C=113 /* q (pp bare tree) */ -> 
			user:latex_tree(B),user:pptree(B),
			step(D,B,X,Lex,Deps)
	;	C=114 /* r (pp x-bar tree) */ -> 
			user:latex_tree(X),user:pptree(X),
			step(D,B,X,Lex,Deps)
	;	C=108 /* l (pp lexical sequence) */ -> 
			writeLs(Lex),
			%portray_clause(Lex),
			step(D,B,X,Lex,Deps)
	;	C=116 /* t(uple) */ ->
	                collectLicensors(Licensors),
	                user:tupleTree(D,Licensors),
	                unix(system('(latex qtree;xdvi qtree)&')),
			step(D,B,X,Lex,Deps)
	;	C=48 /* 0 dependency structure in line */ ->
	                Deps=(Phon,Links),
	    portray_clause(links=Links),
			dep_tree0(Phon,Links),
			unix(system('dot -Tps dotdep.dot -o dotdep.ps')),
			unix(system('dot -Tgif dotdep.dot -o dotdep.gif')),
			unix(system('gv dotdep.ps&')),
			step(D,B,X,Lex,Deps)
	;	C=49 /* 1 dependency structure string + tree */ ->
	                Deps=(Phon,Links),
	    portray_clause(links=Links),
			dep_tree1(Phon,Links),
			unix(system('dot -Tps dotdep.dot -o dotdep.ps')),
			unix(system('dot -Tgif dotdep.dot -o dotdep.gif')),
			unix(system('gv dotdep.ps&')),
			step(D,B,X,Lex,Deps)
	;	C=97 /* a */ -> abort
	;	help3,step(D,B,X,Lex,Deps)
	).

prompted_char(Prompt, Char) :-
	prompt(Prompt),			% write the prompt to the terminal
	get0(C1),			% read the first character
	(   is_endline(C1)		% either it was an end of line
	;   repeat,			% or else we have to skip the
		get0(C2),		% rest of the terminal line
		is_endline(C2)		
	),  !,				% we have to consume the end of line
	Char = C1.			% before doing this unfication.

prompt(ListOrPrompt) :-
	current_output(OldTell), set_output(user_output),
	  format('~N', []),	% start at the beginning of a line
	  write(ListOrPrompt),
	  ttyflush,
	set_output(OldTell).

is_endline(C) :-		% line terminator, NOT just newline
	C < 32,			% covers EOF, ^G, ^J, ^K, ^L, ^M, ^Z, ESC, ^_
	C =\= 9 /* ^I */.	% not ^I (TAB) though.

% help0/0 is for step/0
help0 :-
	format("\nEn la terminal:\t¿más?",[]),
	format("\n              \t   a\tto abort",[]),
	format("\n              \t  <cr>\tdone",[]),
	format("\n              \t   ;\tfor failure/alternatives",[]),
	format("\n          \t o algo más\tpara esta ayuda\n",[]).

% help1/0 is for step1/0
help3 :-
	format("\nEn la terminal:\t¿más?",[]),
	format("\n              \t  <cr>\tterminar",[]),
	format("\n              \t   ;\tmás respultados",[]),
	format("\n              \t   t\tvizualizar derivación con tk",[]),
	format("\n              \t   d\timprimir árbol con tk y ltree.tex",[]),
	format("\n              \t   b\timprimir árbol escueto con tk y ltree.tex",[]),
	format("\n              \t   x\timprimir árbol de x con barra con tk y ltree.tex",[]),
	format("\n              \t   p\timprimir árbol en la terminarl y ltree.tex",[]),
	format("\n              \t   q\timprimir árbol escueto en la terminal y ltree.tex",[]),
	format("\n              \t   r\timprimir árbol de x con barra en la terminal y ltree.tex",[]),
	format("\n              \t   0\tmostrar estructura de dependencias no factorizada horizontalmente",[]),
	format("\n              \t   1\tmostrar estructura de dependencia factorizada verticalmente",[]),
	format("\n              \t   a\tabortar",[]),
	format("\n          \t or anything else\tfor this help\n",[]).
%help0 :-
%	format("\nAt the prompt:\tmore?",[]),
%	format("\n              \t   a\tto abort",[]),
%	format("\n              \t  <cr>\tdone",[]),
%	format("\n              \t   ;\tfor failure/alternatives",[]),
%	format("\n          \t or anything else\tfor this help\n",[]).
%
% help1/0 is for step1/0
%help3 :-
%	format("\nAt the prompt:\tmore?",[]),
%	format("\n              \t  <cr>\tto finish",[]),
%	format("\n              \t   ;\tfor more results",[]),
%	format("\n              \t   t\tdisplay derivation with tk",[]),
%	format("\n              \t   d\tprint derivation tree to tk and ltree.tex",[]),
%	format("\n              \t   b\tprint bare tree to tk and ltree.tex",[]),
%	format("\n              \t   x\tprint x-bar tree to tk and ltree.tex",[]),
%	format("\n              \t   p\tpprint derivation tree to terminal and ltree.tex",[]),
%	format("\n              \t   q\tpprint bare tree to terminal and ltree.tex",[]),
%	format("\n              \t   r\tpprint x-bar tree to terminal and ltree.tex",[]),
%	format("\n              \t   0\tshow unfactored dependency structure horizontally",[]),
%	format("\n              \t   1\tshow factored dependency structure vertically",[]),
%	format("\n              \t   a\tabort",[]),
%	format("\n          \t or anything else\tfor this help\n",[]).

dep_tree0(Phon,Links) :-
	tell('dotdep.dot'),
	write('Digraph G {'),nl,
	write('	node [peripheries=0,fontsize=10,height=0.5,width=0.5];'),nl,
	write('	edge [style=solid,arrowhead=1];'),nl,
	write('		ordering=out;'),nl,
	write('		ranksep=3;'),nl,
	write('		nodesep=0.3;'),nl,
	string_dot(Phon,n),
	write('	{rank=same'),
	write(user,dot_freeze_string(Phon)),nl(user),
	dot_freeze_string(Phon,n),nl,
	dot_prec_string(Phon,n),
	feature_dot(Links,n),
	write('}'),nl,
	told.

dep_tree1(Phon,Links) :- 
	tell('dotdep.dot'),
	write('Digraph G {'),nl,
	write('	node [peripheries=0,fontsize=10,height=0.5,width=0.5];'),nl,
	write('	edge [style=solid,arrowhead=1];'),nl,
	write('		ranksep=0.3;'),nl,
	write('		nodesep=0.3;'),nl,
	string_dot(Phon,n),
	dot_prec_string(Phon,n),
	write(user,dot_prec_string(Phon)),nl(user),
	string_dot(Phon,m),
	feature_dot(Links,m),
	write('}'),nl,
	told.

% order ROYGBV: R B G Y O V  and BLACK=trouble
dep_color(1,'"1,1,1"') :- !. %red
dep_color(2,'"0.640,1,1"') :- !. %blue
dep_color(3,'"0.3,1,0.8"') :- !. %green
dep_color(4,'"0.175,1,1"') :- !. %yellow
dep_color(5,'"0.108,1,1"') :- !. %orange
dep_color(6,'"0.833,0.454,0.933"') :- !. %violet
dep_color(_,black).

feature_dot([],_Char).
feature_dot([((Node0:Feature)->rm(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->rm(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="rm"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->lm(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->lm(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="lm"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->li(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->li(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="li"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->ri(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->ri(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="ri"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->hl(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->hl(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="hl"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->hr(Node))|L],Char) :- !,
	write(user,((Node0:Feature)->hr(Node))),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',taillabel="hr"'),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([((Node0:Feature)->Node)|L],Char) :- !,
	write(user,((Node0:Feature)->Node)),nl(user),
	dep_color(Feature,Color),
	write('	'),write(Char),write(Node0),write('->'),write(Char),
	write(Node),write(' [color='),write(Color),
	write(',weight=1];'),nl,
	feature_dot(L,Char).
feature_dot([What|L],_Char) :- !, write(user,'ERROR':feature_dot([What|L])),nl(user),abort.

dot_freeze_string([],_Char) :- !, write('};').
dot_freeze_string([N0=_|N],Char) :- write('; '),write(Char),write(N0), dot_freeze_string(N,Char).

dot_prec_string([],_Char) :- !.
dot_prec_string([_],_Char) :- !.
dot_prec_string([N0=_,N1=_|Ns],Char) :-
	write('	'),write(Char),write(N0),write('->n'),write(N1),
	write('[arrowhead=none,style=invis,weight=100];'),nl,
	dot_prec_string([N1=x|Ns],Char),!.

string_dot([],_Char) :- nl(user),!.
string_dot([N=Phon|Nodes],Char) :-
	(Char = n -> write(user,(N:Phon)),write(user,'	'); true),
	write('	'),write(Char),write(N),write(' [label="'),write(Phon),write('"];'),nl,
	string_dot(Nodes,Char),!.

writeLs([]).
writeLs([H]) :- !, print(H).
writeLs([H|T]) :- !, print(H), nl, writeLs(T).

collectLicensors(L) :- setof(F,Str^Fs^(user:(Str::Fs),member(+F,Fs)),L), !.
collectLicensors([]).
