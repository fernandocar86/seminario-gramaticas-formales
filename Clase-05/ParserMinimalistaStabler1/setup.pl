% file: setup.pl
% origin author : E Stabler
% origin date: Jan 2001
% purpose: load files for CKY-like MG parser, swi version
% use: This is the top file for loading files and demos.
%      If this load prompts about redefining predicates, type p(roceed)
% updates: June 2001- Willemijn Vermaat 
% updates: May 2002 - Stabler - extension to standard transformational grammar
% updates: Jul 2010 - Stabler - update for SWI Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Este archivo está simplificado por Fernando Carranza para correr solo una selección 
% ilustrativa de las gramáticas minimalistas para su uso interno en las clases del 
% seminario "Gramáticas formales: formalismos e implementaciones" (Carranza y Zdrojewski) 
% durante el primer cuatrimestre de 2022, Universidad de Buenos Aires.

 
% Para la versión original de Stabler remitimos a 
% https://linguistics.ucla.edu/person/edward-stabler/
%
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% operator defs - don't touch these
:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features
:- op(500, xf, <=).		% for right incorporation
:- op(500, fx, =>). % for left incorporation
:- op(500, xf, ==>). % for right affix hop
:- op(500, fx, <==). % for left affix hop
:- op(500, xfy, <<). % for adjunction
:- op(500, xfy, >>). % for adjunction

% for tree display
:- ensure_loaded('tree_display/wish_treeSWI').
:- ensure_loaded('tree_display/latex_treeSWI').
:- ensure_loaded('tree_display/pptree').

% Recognizer and display tool
:- ensure_loaded('parser/mgpx'),ensure_loaded('parser/lpx').  % basic MG parser and lexical sequence parser

% uncomment ONE grammar
   % GRAMMARS REQUIRING ONLY PHRASAL MOVEMENT (mgpx+lpx)
:- ['grammars/anbncn']. % 
%:- ['grammars/g0spanish']. % simple SOV 
%:- ['grammars/g-ne']. % "titus praise -s lavinia" inspired by Mahajan 2000


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oraciones para probar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% anbncn.pl

gabc(1) :- showParse([a,b,c]).
gabc(2) :- showParse([a,a,b,b,c,c]).
gabc(3) :- showParse([a,a,a,b,b,b,c,c,c]).
% y así sucesivamente

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g0spanish.pl
g0(a) :- showParse([la,reina,la,torta,comió]).
g0(b) :- showParse(['cuál',torta,la,reina,'comió']).
g0(b) :- showParse(['la,reina,'rió']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g-ne.pl

gh3(X) :- gh1(X).
gh3(10) :- showParse(['Titus',know,'-s',that,'Lavinia',have,'-s',eat,'-en']).
gh3(x10) :- showParse(['Titus',wonders,'-s',that,'Lavinia',laugh,'-s']).
gh3(11) :- showParse(['Titus',think,'-s','Lavinia',laugh,'-s']).
gh3(12) :- showParse(['Titus',doubt,'-s',the,claim,that,'Lavinia',laugh,'-s']).
gh3(13) :- showParse(['Titus',know,'-s',what,'Lavinia',praise,'-s']).
gh3(14) :- showParse(['Titus',wonder,'-s',which,king,'Lavinia',praise,'-s']).
gh3(15) :- showParse(['Titus',seem,'-s',to,laugh]).
gh3(16) :- showParse(['Titus',know,'-s',that,'Lavinia',seem,'-s',to,laugh]).
gh3(17) :- showParse(['Titus',seem,'-s',to,praise,'Lavinia']).
gh3(18) :- showParse(['Titus',seem,'-s',to,be,laugh,'-ing']).
gh3(19) :- showParse(['Titus',seem,'-s',to,have,eat,'-en',the,pie]).
gh3(20) :- showParse(['Titus',seem,'-s',to,have,been,eat,'-ing',the,pie]).
gh3(21) :- showParse(['Titus',seem,'-s',happy]).
gh3(22) :- showParse(['Titus',be,'-s',happy]).
gh3(23) :- showParse(['Titus',will,'-s',be,happy]).
gh3(x23) :- showParse(['Titus',be,'-s',be,'ing',happy]).
gh3(x23) :- showParse(['Titus',be,'-s',have,'-ing',been,happy]).
gh3(24) :- showParse(['Titus',be,'-s',proud]).
gh3(25) :- showParse(['Titus',be,'-s',proud,of,'Lavinia']).
gh3(26) :- showParse(['Titus',prefer,'-s','Lavinia',happy]).
gh3(27) :- showParse(['Titus',prefer,'-s',his,coffee,black]).
gh3(28) :- showParse(['Titus',prefer,'-s',his,shirt,white]).
gh3(29) :- showParse(['Titus',prefer,'-s','Lavinia',proud]).
gh3(30) :- showParse(['Titus',prefer,'-s','Lavinia',proud,of,'Tamara']).
gh3(31) :- showParse(['Titus',prefer,'-s','Lavinia',proud,about,it]).
gh3(32) :- showParse([the,student,be,'-s',up,the,creek]).
gh3(33) :- showParse(['Titus',prefer,'-s','Lavinia',to,be,happy]).
gh3(34) :- showParse(['Titus',prefer,'-s','Lavinia',to,laugh]).
gh3(35) :- showParse(['Titus',prefer,'-s','Lavinia',to,be,laugh,'-ing']).
gh3(36) :- showParse(['Titus',prefer,'-s','Lavinia',to,have,been,eat,'-ing']).
gh3(37) :- showParse([the,student,try,'-s',to,laugh]).
gh3(38) :- showParse([the,student,want,'-s',to,laugh]).
gh3(39) :- showParse([the,student,want,'-s','Lavinia',to,be,happy]).
gh3(x39) :- showParse([the,student,try,'-s','Lavinia',to,be,happy]).
gh3(40) :- showParse([the,student,want,'-s','Lavinia',to,laugh]).
gh3(x40) :- showParse([the,student,try,'-s','Lavinia',to,laugh]).
gh3(41) :- showParse([the,student,consider,'-s','Lavinia',to,be,happy]).
gh3(x41) :- showParse([the,student,consider,'-s',to,be,happy]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

