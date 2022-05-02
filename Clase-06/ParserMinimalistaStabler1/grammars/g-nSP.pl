%   File   : g-ne.pl - naive english
%   Author : E Stabler
%   Updated: Mar 00

%% Complementante
[] :: [=T,C].                
% [] :: [=T,+wh,C].

%% Tiempo
[] :: [=Pred,+v,+k,T].  

%% Predicados
%[] :: [=VT,+k,=D,Pred].    
[] :: [=V,Pred].


%%% Verbos [V]
% [comió ] :: [=D,+k,VT].        
% [rió] :: [V].
%[come] :: [=D,+k,VT].  
[canta] :: [=D, V, -v].
%[vio] :: [=D,+k,V].   
%[cocinó] :: [=D,+k,V]. 


%%% Determinantes
%[el] :: [=N,D,-k].  
% [cuál] :: [=N,D,-k,-wh].
% [la] :: [=N,D,-k].


%%% Nombres comunes
% [perro] :: [N].              
%  [torta] :: [N].
%[hueso] :: [N].


%%% Nombres propios
[fernando] :: [D,-k].      
% [Julia] :: [D,-k].   
% [Macarena] :: [D,-k].
% [Pablo] :: [D,-k].  





startCategory(c).


% showParse([fernand,praise,'-s',titus]).