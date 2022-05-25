% gramática parte 1

%% Complementantes 
[]::[='T','C'].                 % complementante declarativo. El operador = induce external merge.


%% Tiempo
[-aron]::[=>v,+nom,'T'].         % El operador => induce el movimiento de nucleo de v a T, +nom atrae al sujeto                                 

%% v chiquito
[]::[=>'V',='DPl',v].             % El operador => induce el movimiento de nucleo de V a v.

%% Verbo transitivo 
[arm]::[='DSg',+ac,'V'].         % El rasgo +ac atrae al objeto

%% Nombre común
[liebres]::['NPl'].
[madriguera]::['NSg'].


% Determinante
[las]::[='NPl','DPl',-nom].           % El rasgo -nom es atraído por +nom en T.
[una]::[='NSg','DSg',-ac].           % El rasgo -ac es atraído por +ac en v.


startCategory('C').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% para probar:
% 1
% showParse([las,liebres,arm,-aron,una,madriguera]).

