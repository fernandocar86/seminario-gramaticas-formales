%   File   : toyg.pl
%   Author : Pablo Zdrojewski
%   Updated: May 2022
%   Este archivo contiene unas gramáticas básicas para probar diversas restricciones sobre el orden de los rasgos. 

%%%%%%%%%%% Versión 1 - OD >>> Suj

%% Complementante
%% [] :: [='T','C'].                

%% %% Tiempo
%% [] :: [='Pred',+h,+k,'T'].  

%% %% Predicados
%% [] :: [='Vt',='D',+k,'Pred']. 
%% [] :: [='V',='D','Pred'].

%% %%% Verbos 
%% [vio] :: [='D','Vt',-h].   
%% [salta] :: ['V',-h].   


%% %%% Argumentos 
%% ['Juan'] :: ['D',-k].
%% ['Pedro'] :: ['D',-k].

%% startCategory('C').

%%%%%%%%%%%%%%%% Oraciones
% showParse(['Juan',salta]).
% showParse(['Juan',vio,'Pedro']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%% Versión 2 - Sin cruce de argumentos

%% Complementante
%% [] :: [='T','C'].                

%% %% Tiempo
%% [] :: [='Pred',+h,+k,'T']. 

%% %% Predicados
%% [] :: [='Vt',='D','Pred'].
%% [] :: [='V',='D','Pred'].

%% %%% Verbos 
%% [saluda] :: [='D',+k,'Vt',-h].   
%% [salta] :: ['V',-h].   


%% %%% Argumentos 
%% ['Juan'] :: ['D',-k].
%% ['Pedro'] :: ['D',-k].

%% startCategory('C').

%%%%%%%%%%%%%%%% Oraciones
% showParse(['Juan',salta]).
% showParse(['Juan',saluda,'Pedro']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%% Versión 3 - 

%Complementante
[] :: [='T','C'].                

%% Tiempo
[] :: [='Pred',+h,+k,'T']. 

%% Predicados
[] :: [='Vt',+k,='D','Pred']. %  +k precede a =D
[] :: [='V',='D','Pred'].

%%% Verbos 
[vio] :: [='D','Vt',-h].   
[salta] :: ['V',-h].   


%%% Argumentos 
['Juan'] :: ['D',-k].
['Pedro'] :: ['D',-k].

startCategory('C').

%%%%%%%%%%%%%%%% Oraciones
% showParse(['Juan',salta]).
% showParse(['Juan',vio,'Pedro']).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




