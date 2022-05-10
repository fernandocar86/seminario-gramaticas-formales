%   File   : toyg.pl
%   Author : Pablo Zdrojewski
%   Updated: May 2022
%   Este archivo contiene unas gramáticas básicas para probar diversas restricciones sobre el orden de los rasgos. 

%%%%%%%%%%% Versión 1 - Sin cruce de argumentos

%% %% Complementante
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


%%%%%%%%%%% Versión 2 - Sin cruce de argumentos

%% Complementante
%% [] :: [='T','C'].                

%% %% Tiempo
%% [] :: [='Pred',+h,+k,'T']. 

%% %% Predicados
%% [] :: [='Vt',='D','Pred'].
%% %[] :: [='V',='D','Pred'].

%% %%% Verbos 
%% [trans] :: [='D',+k,'Vt',-h].   
%% %[inerg] :: ['V',-h].   


%% %%% Argumentos 
%% [suj] :: ['D',-k].
%% [obj] :: ['D',-k].


%%%%%%%%%%% Versión 3 - OK

%% %% Complementante
%% [] :: [='T','C'].                

%% %% Tiempo
%% [] :: [='Pred',+h,+k,'T']. 

%% %% Predicados
%% [] :: [='Vt',+k,='D','Pred']. %  +k precede a =D
%% [] :: [='V',='D','Pred'].

%% %%% Verbos 
%% [trans] :: [='D','Vt',-h].   
%% [inerg] :: ['V',-h].   


%% %%% Argumentos 
%% [suj] :: ['D',-k].
%% [obj] :: ['D',-k].



[] :: [=t,'C'].
[] :: [=pred,+h,t].
% [verbo] :: [=d,v,-h]. [] :: [=v,+k,pred]. % funciona verbo > obj

[verbo] :: [=d,+k,v,-h]. [] :: [=v,pred]. % funciona obj > verbo

[obj] :: [d,-k].


startCategory('C').


% showParse([juan,salta]).
% showParse([fernando,come,la,torta]).
