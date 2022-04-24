% File   : g0spSVO.pl
% Gramática adaptada por Pablo Zdrojewski de la gramática g0.pl diseñada por 
% E Stabler (Mar 2000). Para uso interno del Seminario "Gramáticas formales: formalismos e implementaciones" 
% (UBA).

:- encoding(utf8).

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features

[] :: [='T','O'].               
[] :: [='V',='D','T'].             
['comió'] :: [='D','V'].         ['rió'] :: ['V'].
['come'] :: [='D','V'].         ['ríe'] :: ['V'].

[la] :: [='N','D'].         
[reina] :: ['N'].                [torta] :: ['N'].

['Ivan'] :: ['D'].     
['Gala'] :: ['D'].    

startCategory('C').

