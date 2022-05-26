% File   : g0spanish.pl
% Gramática adaptada por Pablo Zdrojewski de la gramática g0.pl  diseñada por E Stabler (Mar 2000). Para uso interno de Seminario "Gramáticas formales: formalismos e implementaciones"  (UBA).

:- encoding(utf8).

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features


%%% Complementantes
[] :: [='T','C'].               [] :: [='T',+wh,'C'].


%% Tiempo
[] :: [='V',+k,'T'].              

%% v 
[] :: [='V',='D','V'].


%%% Verbos [V]
['comió'] :: [='D',+k,'V'].         ['rió'] :: ['V'].
['come'] :: [='D',+k,'V'].         ['ríe'] :: ['V'].
['vio'] :: [='D',+k,'V'].   
['cocina'] :: [='D',+k,'V']. 



%%% Determinantes
['el'] :: [='N','D',-k].         ['cuál'] :: [='N','D',-k,-wh].
%['la'] :: [='N','D',-k].


%%% Nombres comunes
['perro'] :: ['N'].              %  ['torta'] :: ['N'].
['hueso'] :: ['N'].


%%% Nombres propios
['Fernando'] :: ['D',-k].      
['Julia'] :: ['D',-k].   
['Macarena'] :: ['D',-k].
['Pablo'] :: ['D',-k].  

startCategory('C').



% showParse(['el','perro','el','hueso','vio']).
