%   File   : g-nSP.pl - naive english
%   Author : E Stabler
%   Updated: Mar 00

%% Complementante
[] :: [=t,c].                
[] :: [=t,+wh,c].

%% Tiempo
[] :: [=pred,+h,+k,t]. 

%% Predicados
[] :: [=vt,+k,=d,pred].    
[] :: [=v,=d,pred].

%%% Verbos 
%['comió' ] :: [=d,vt,-h].        
%['sonrió'] :: [v,-h].
%[come] :: [=d,vt,-h].  
%[canta] :: [v,-h].
[vio] :: [=d,=d,vt,-h].   
%[cocina] :: [=d,vt,-h]. 

% [] :: [=vt,+k,=d,pred].    [] :: [=v,pred].
%[praise] :: [=d,vt,-v]. 


%%% Determinantes
%[el] :: [=nm,d,-k].  
%['cuál'] :: [=nf,d,-k,-wh].
%['cuál'] :: [=nm,d,-k,-wh].
%[la] :: [=nf,d,-k].


%%% Nombres comunes
%[perro] :: [nm].              
%[torta] :: [nf].
%[hueso] :: [nm].


%%% Nombres propios
[fernando] :: [d,-k].      
%[julia] :: [d,-k].   
%[macarena] :: [d,-k].
[pablo] :: [d,-k].  


startCategory(c).


% showParse([fernando,canta]).
% showParse([fernando,come,la,torta]).