% gramática parte 1 + 2 + 3

%% Complementantes 
[]::[='T','C'].                 % complementante declarativo. El operador = induce external merge.
[]::[=>'T',+f,'C'].		% foco en el objeto
[]::[=>'T',+wh,'C'].            % interrogativo

%% Tiempo
[-aron]::[=>v,+nom,'T'].         % El operador => induce el movimiento de nucleo de v a T, +nom atrae al sujeto                     
[-an]::[=>v,+nom,'T']. 
[-aban]::[=>v,+nom,'T']. 
       
%% v chiquito
[]::[=>'V',='DPl',v].             % El operador => induce el movimiento de nucleo de V a v.

%% Verbo transitivo 
[arm]::[='DSg',+ac,'V'].         % El rasgo +ac atrae al objeto, todos los objetos son singulares
[mir]::[='DSg',+ac,'V']. 

%% Verbo intransitivo 
[nad]::['V'].         

%% Nombre común
[liebres]::['NPl'].
[tortugas]::['NPl'].
[madriguera]::['NSg'].


% Determinante
[las]::[='NPl','DPl',-nom].           % El rasgo -nom es atraído por +nom en T, los sujetos son plurales
[una]::[='NSg','DSg',-ac].           % El rasgo -ac es atraído por +ac en v.
[una]::[='NSg','DSg',-ac, -f].	% -f es atraído por +f en C
[esa]::[='NSg','DSg',-ac].           
[esa]::[='NSg','DSg',-ac, -f].
['quiénes']::['DPl',-nom,-wh].	% el rasgo -wh es atraído por +wh en C interrogativo, quiénes es sujeto, plural

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
% 3
% showParse(['quiénes',arm,-aron,una,madriguera]).
% showParse(['quiénes',arm,-aron,esa,madriguera]).
% showParse(['quiénes',nad,-an]).
% showParse(['quiénes',nad,-aron]).
% showParse(['quiénes',arm,-aban,una,madriguera]).
% showParse(['quiénes',arm,-aban,esa,madriguera]).
% showParse([las,tortugas,nad,-aban]).
% showParse([las,liebres,nad,-aban]).
% showParse([las,liebres,arm,-aban,una,madriguera]).
% showParse([las,liebres,arm,-aban,esa,madriguera]).
% showParse([una,madriguera,arm,-aban,las,liebres]).
% showParse([esa,madriguera,arm,-aban,las,liebres]).
% showParse([las,liebres,mir,-aban,esa,madriguera]).
% showParse([las,tortugas,mir,-aban,esa,madriguera]).
% showParse([las,liebres,mir,-aron,esa,madriguera]).
% showParse([las,tortugas,mir,-aron,esa,madriguera]).
% showParse([las,liebres,mir,-an,esa,madriguera]).
% showParse([las,tortugas,mir,-an,una,madriguera]).
% showParse([las,liebres,mir,-aban,una,madriguera]).
% showParse([las,tortugas,mir,-aban,una,madriguera]).
% showParse([las,liebres,mir,-aron,una,madriguera]).
% showParse([las,tortugas,mir,-aron,una,madriguera]).
% showParse([las,liebres,mir,-an,una,madriguera]).
% showParse([las,tortugas,mir,-an,una,madriguera]).

