% File   : g0spanish.pl
% Gramática adaptada por Fernando Carranza de la gramática g0.pl  diseñada por E Stabler (Mar 2000). Para uso interno de Seminario "Gramáticas formales: formalismos e implementaciones"  (UBA).

:- encoding(utf8).

:- op(500, xfy, ::). % lexical items
:- op(500, fx, =). % for selection features


%%% Complementantes
[] :: [='T','C'].               [] :: [='T',+wh,'C'].


%% Tiempo
[] :: [='voice',+nom,'T'].              

%% v 
[] :: [='V',+ac,'v'].

%% voice 
[] :: [='v',='Dnom','voice'].



%%% Verbos [V]
['comió'] :: [='Dac','V'].         ['rió'] :: ['V'].
['come'] :: [='Dac','V'].         ['ríe'] :: ['V'].
['vio'] :: [='Dac','V'].   
['cocina'] :: [='Dac','V']. 


%%% Determinantes
['el'] :: [='N','D',].         ['cuál'] :: [='N','Dac',-ac,-wh].
['la'] :: [='N','Dac',-ac].
['el'] :: [='N','Dnom',-ac].         ['cuál'] :: [='N','Dnom',-ac,-wh].
['la'] :: [='N','Dnom',-ac].


%%% Nombres comunes
['perro'] :: ['N'].                ['torta'] :: ['N'].
['hueso'] :: ['N'].


%%% Nombres propios
['Fernando'] :: ['Dnom',-nom].      
['Julia'] :: ['Dnom',-nom].   
['Macarena'] :: ['Dnom',-nom].
['Pablo'] :: ['Dnom',-nomk].  

startCategory('C').


