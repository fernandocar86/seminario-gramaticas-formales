% File   : g0spanish.pl
% Gramática adaptada por Fernando Carranza de la gramática g0.pl diseñada por 
% E Stabler (Mar 2000). Para uso interno de Modelos Formales No Transformacionales
% (UBA).
:- encoding(utf8).

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features

[] :: [='T','C'].               [] :: [='T',+wh,'C'].
[] :: [=v,+k,'T'].              [] :: [='V',='D',v].
['comió'] :: [='D',+k,'V'].         ['rió'] :: ['V'].
['come'] :: [='D',+k,'V'].         ['ríe'] :: ['V'].

[la] :: [='N','D',-k].         ['cuál'] :: [='N','D',-k,-wh].
[reina] :: ['N'].                [torta] :: ['N'].

['Juan'] :: ['D',-k].      ['María'] :: ['D',-k].      ['Cata'] :: ['D',-k].
['Romi'] :: ['D',-k].      ['Rocío'] :: ['D',-k].      ['Fede'] :: ['D',-k]. 
[quiere] :: [='D',+k,'V'].

startCategory('C').

