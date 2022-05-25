% gramática parte 1 + 2

%% Complementantes 
[]::[='T','C'].                 % complementante declarativo. El operador = induce external merge.
[]::[=>'T',+f,'C'].		% foco en el objeto

%% Tiempo
[-aron]::[=>v,+nom,'T'].         % El operador => induce el movimiento de nucleo de v a T, +nom atrae al sujeto        
[-an]::[=>v,+nom,'T'].                          

%% v chiquito
[]::[=>'V',='DPl',v].             % El operador => induce el movimiento de nucleo de V a v.

%% Verbo transitivo 
[arm]::[='DSg',+ac,'V'].         % El rasgo +ac atrae al objeto

%% Verbo intransitivo 
[nad]::['V'].         

%% Nombre común
[liebres]::['NPl'].
[tortugas]::['NPl'].
[madriguera]::['NSg'].


% Determinante
[las]::[='NPl','DPl',-nom].           % El rasgo -nom es atraído por +nom en T.
[una]::[='NSg','DSg',-ac].           % El rasgo -ac es atraído por +ac en v.
[una]::[='NSg','DSg',-ac, -f].	      % -f es atraído por +f 

startCategory('C').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% para probar:
% 1
% showParse([las,liebres,arm,-aron,una,madriguera]).
% 2
% showParse([una,madriguera,arm,-aron,las,liebres]).
% showParse([las,tortugas,arm,-aron,una,madriguera]).
% showParse([las,tortugas,nad,-aron]).
% showParse([las,tortugas,nad,-an]).
% showParse([las,liebres,nad,-an]).
% showParse([las,liebres,nad,-aron]).

