% Fuente para todo el documento
:- encoding(utf8).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% Este archivo est� simplificado por Fernando Carranza para correr solo una selecci�n 
% ilustrativa de las gram�ticas minimalistas para su uso interno en las clases del 
% seminario "Gram�ticas formales: formalismos e implementaciones" (Carranza y Zdrojewski) 
% durante el primer cuatrimestre de 2022, Universidad de Buenos Aires.
%
% Para la versi�n original de Stabler remitimos a 
% https://linguistics.ucla.edu/person/edward-stabler/
%
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operadores
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% operator defs - don't touch these
% :- op(500, xfy, ::). % lexical items
% :- op(500, fx, =). % for selection features
%:- op(500, xf, <=).		% for right incorporation
%:- op(500, fx, =>). % for left incorporation
%:- op(500, xf, ==>). % for right affix hop
%:- op(500, fx, <==). % for left affix hop
%:- op(500, xfy, <<). % for adjunction
%:- op(500, xfy, >>). % for adjunction

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visualizado de �rboles
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for tree display
:- ensure_loaded('tree_display/wish_treeSWI').
:- ensure_loaded('tree_display/latex_treeSWI').
:- ensure_loaded('tree_display/pptree').

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parser
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recognizer and display tool
:- ensure_loaded('parser/mgpx'),ensure_loaded('parser/lpx').  % basic MG parser and lexical sequence parser

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gram�ticas
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GRAMMARS REQUIRING ONLY PHRASAL MOVEMENT (mgpx+lpx)
%:- ['grammars/anbncn']. 
%:- ['grammars/g0']. 
:- ['grammars/g0spSVO']. % 
%:- ['grammars/g0spanish']. % simple SOV 
%:- ['grammars/g0spanish2']. % simple SOV 
%:- ['grammars/g0sp-caso']. % 
%:- ['grammars/g-ne']. % "titus praise -s lavinia" inspired by Mahajan 2000
%:- ['grammars/g-nSP']. % "titus praise -s lavinia" inspired by Mahajan 2000
%:- ['grammars/g-sp3']. % "titus praise -s lavinia" inspired by Mahajan 2000


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oraciones para probar (seleecionar de la gram�tica que corresponda)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g0.pl


%anbn.pl

%anbncn.pl

% showParse([a,b,c]).
% showParse([a,a,b,b,c,c]).
% showParse([a,a,a,b,b,b,c,c,c]).
% y as� sucesivamente


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g0spSVO.pl
% showParse(['Ivan','come','la','torta']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g0spanish.pl
% showParse([la,reina,la,torta,comi�]).
% showParse(['cu�l',torta,la,reina,'comi�']).
% showParse(['la,reina,'ri�']).
% y as� sucesivamente


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g-ne.pl

% g-nSP.pl

%%%%% Para clase 7 %%%%%%%%%%%%%%%%%%%%%%%%%%
% gh1(X).
% showParse(['Titus',know,'-s',that,'Lavinia',have,'-s',eat,'-en']).
% showParse(['Titus',wonders,'-s',that,'Lavinia',laugh,'-s']).
% showParse(['Titus',think,'-s','Lavinia',laugh,'-s']).
% showParse(['Titus',doubt,'-s',the,claim,that,'Lavinia',laugh,'-s']).
% showParse(['Titus',know,'-s',what,'Lavinia',praise,'-s']).
% showParse(['Titus',wonder,'-s',which,king,'Lavinia',praise,'-s']).
% showParse(['Titus',seem,'-s',to,laugh]).
% showParse(['Titus',know,'-s',that,'Lavinia',seem,'-s',to,laugh]).
% showParse(['Titus',seem,'-s',to,praise,'Lavinia']).
% showParse(['Titus',seem,'-s',to,be,laugh,'-ing']).
% showParse(['Titus',seem,'-s',to,have,eat,'-en',the,pie]).
% showParse(['Titus',seem,'-s',to,have,been,eat,'-ing',the,pie]).
% showParse(['Titus',seem,'-s',happy]).
% showParse(['Titus',be,'-s',happy]).
% showParse(['Titus',will,'-s',be,happy]).
% showParse(['Titus',be,'-s',be,'ing',happy]).
% showParse(['Titus',be,'-s',have,'-ing',been,happy]).
% showParse(['Titus',be,'-s',proud]).
% showParse(['Titus',be,'-s',proud,of,'Lavinia']).
% showParse(['Titus',prefer,'-s','Lavinia',happy]).
% showParse(['Titus',prefer,'-s',his,coffee,black]).
% showParse(['Titus',prefer,'-s',his,shirt,white]).
% showParse(['Titus',prefer,'-s','Lavinia',proud]).
% showParse(['Titus',prefer,'-s','Lavinia',proud,of,'Tamara']).
% showParse(['Titus',prefer,'-s','Lavinia',proud,about,it]).
% showParse([the,student,be,'-s',up,the,creek]).
% showParse(['Titus',prefer,'-s','Lavinia',to,be,happy]).
% showParse(['Titus',prefer,'-s','Lavinia',to,laugh]).
% showParse(['Titus',prefer,'-s','Lavinia',to,be,laugh,'-ing']).
% showParse(['Titus',prefer,'-s','Lavinia',to,have,been,eat,'-ing']).
% showParse([the,student,try,'-s',to,laugh]).
% showParse([the,student,want,'-s',to,laugh]).
% showParse([the,student,want,'-s','Lavinia',to,be,happy]).
% showParse([the,student,try,'-s','Lavinia',to,be,happy]).
% showParse([the,student,want,'-s','Lavinia',to,laugh]).
% showParse([the,student,try,'-s','Lavinia',to,laugh]).
% showParse([the,student,consider,'-s','Lavinia',to,be,happy]).
% showParse([the,student,consider,'-s',to,be,happy]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

