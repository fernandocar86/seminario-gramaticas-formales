%   File   : g0sp-em.pl
%   Author : E Stabler
%   Updated: Mar 00
%
% Modificación para el español de la gramática g0,pl de Stabler,  
% para uso didactivo exclusivo 

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features

[] :: [='T','C'].               [] :: [='T',+wh,'C'].
[] :: [=v,+k,'T'].              [] :: [='V',='D',v].

% Verbos
[comió] :: [='D',+k,'V'].         [rie] :: ['V'].
[vio] :: [='D',+k,'V'].

% Determinantes
[el] :: [='N','D',-k].         [Cuál] :: [='N','D',-k,-wh].

% Nombres comunes
[perro] :: ['N'].                [hueso] :: ['N'].

% Nombres propios
[Ana] :: ['D',-k].      [Maria] :: ['D',-k].

startCategory('C').


% showParse([Ana, vio, el, hueso]).
