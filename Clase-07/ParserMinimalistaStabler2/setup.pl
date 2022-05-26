% Fuente para todo el documento
:- encoding(utf8).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% ilustrativa de las gramáticas minimalistas para su uso interno en las clases del 
% seminario "Gram�ticas formales: formalismos e implementaciones" (Carranza y Zdrojewski) 
% durante el primer cuatrimestre de 2022, Universidad de Buenos Aires.
% 
% Para la versi�n original de Stabler remitimos a 
% https://linguistics.ucla.edu/person/edward-stabler/
%
% Para hacer que el parser corra l�nea por l�nea comentar en lhapx funci�n everystep
% todo:
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operadores
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
:- ensure_loaded('parser/mghapx'),ensure_loaded('parser/lhapx').  % TG parser and lexical sequence parser

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gram�ticas
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GRAMMARS REQUIRING HEAD MOVEMENT (mghapx+lhapx)
%:- ['grammars/spanish1']. % 
%:- ['grammars/gh6']. %
%:- ['grammars/larsonian1']. %
:- ['grammars/hmtest1']. %
 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oraciones para probar (seleecionar de la gram�tica que corresponda)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% spanish.pl
% showLexicon.
% showParse(['Juan',leer,'-pres',el,libro]).
% showParse(['Romi',leer,'-pdo',el,libro]).
% y as� sucesivamente



