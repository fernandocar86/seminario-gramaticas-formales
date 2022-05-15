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
:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection feature
:- op(500, xf, =). % for selection feature
:- op(500, xf, <=).		% for right incorporation
:- op(500, fx, =>). % para incorporación a la izquierda
:- op(500, xf, ==>). % para affix hopping a la derecha
:- op(500, fx, <==). % para affix hopping a la izquierda
:- op(500, xfy, <<). % para adjunción
:- op(500, xfy, >>). % para adjunción

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visualizado de árboles
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
% Gramáticas
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GRAMMARS REQUIRING ONLY PHRASAL MOVEMENT (mgpx+lpx)
%:- ['grammars/gprueba1']. % Prueba 1
%:- ['grammars/gprueba2']. % Prueba 2
%:- ['grammars/gprueba3']. % Prueba 3
%:- ['grammars/g-ne']. % Inglés Movimiento de Remanente
%:- ['grammars/g-nSP']. % "Gramática del español con Movimiento de Remanente basada en g-ne
:- ['grammars/g0sp-caso']. % Gramática del español con licenciamiento de caso
%:- ['grammars/g-nSP-k']. % "Gramática del español licenciamiento caso -k
%:- ['grammars/anbncn'].
%:- ['grammars/toyg']. % Inglés Movimiento de Remanente
%:- ['grammars/greverse']. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oraciones para probar (seleecionar de la gram�tica que corresponda)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% g0.pl

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

