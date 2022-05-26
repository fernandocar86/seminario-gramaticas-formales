%   File   : g-nSP.pl - naive english
%   Author : E Stabler
%   Updated: Mar 00

%% Complementante
[] :: [='T','C'].                
%[] :: [=t,+wh,c].

%% Tiempo
[] :: [='Pred',+h,+k,'T']. 
%[] :: [='Pred',+vt,+k,t].  

%% Predicados
[] :: [='VT',='DS',+k,'Pred'].
%[] :: [='V','Pred'].

%%% Verbos 
%['comió' ] :: [=d,vt,-h].        
['sonrió'] :: ['V',-h].
%[come] :: [=d,vt,-h].  
%[canta] :: [=d,v].
[vio] :: [='DO','VT',-h].   
%[cocina] :: [=d,vt,-h]. 




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
['Fernando'] :: ['D',-k].
['Julia'] :: ['D',-k].
['Macarena'] :: ['DO',-k].
['Pablo'] :: ['DS',-k].


startCategory('C').


% showParse([fernando,canta]).
% showParse([fernando,come,la,torta]).
