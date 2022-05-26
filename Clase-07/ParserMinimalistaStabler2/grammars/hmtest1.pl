
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Esta es la gramática estudiada en clase.                                                          %%
%%                                                                                                    %%
%% Recomiendo leer los comentarios que tiene cada item, porque les servirá para realizar el TP3       %%
%% También puede ser conveniente que lean leer las observaciones que aparecen al final del archivo.  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 

%% Complementantes 
[]::[='T','C'].                 % Complementante declararivo. El operador = induce external merge.
[]::[=>'T',+wh,'C'].            % Complementante interrogativo.
                                % El operador => induce el movimiento de nucleo T a C, +wh atrae a 'quién'.

%% Tiempo
['-ó']::[=>v,+nom,'T'].         % El operador => induce el movimiento de nucleo de v a T,
                                % +nom atrae al sujeto 'quién'/'Iván'.

%% v chiquito
[]::[=>'V',='D',v].             % El operador => induce el movimiento de nucleo de V a v.

%% Verbo transitivo 
[pint]::[='D',+ac,'V'].         % El rasgo +ac atrae al objeto 'la pared'.

%% Nombre común
[pared]::['N'].


%% Nombre propio
['Iván']::['D',-nom].          % El rasgo -nom es atraído por +nom en T.

% Determinante
[la]::[='N','D',-ac].           % El rasgo -ac es atraído por +ac en v.
['quién']::['D',-nom,-wh].      % Pronombre Interrogativo. El rasgo -nom es atraído por +nom en T
                                % y el rasgo -wh es atraído por +wh en C interrogativo.

['qué']::['D',-ac,-wh].         % Pronombre Interrogativo. El rasgo -ac es atraído por +ac en V y
                                % el rasgo -wh es atraído por +wh en C interrogativo.

startCategory('C').

%%% Oraciones para probar
% showParse(['Iván',pint,'-ó',la,pared]).
% showParse(['quién',pint,'-ó',la,pared]).
% showParse(['qué',pint,'-ó','Iván']).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Observación:                                                                                                    %%
%% La presente gramática solo genera las siguientes oraciones.                                                     %%
%%                                                                                                                 %%
%% 1. Iván pintó la pared.                                                                                         %%
%% 2. ¿Quién pintó la pared?                                                                                       %%
%% 3. ¿Qué pintó Iván?                                                                                             %%
%%                                                                                                                 %%
%% Es posible enriquecerla para que genere otras oraciones con otros tiempos verbales como en 4,                   %%
%% agregando el ítem de 5.                                                                                         %%
%%                                                                                                                 %%
%% 4. Iván pintaba la pared.                                                                                       %%
%% 5. ['-aba']::[=>v,+nom,'T'].                                                                                    %%
%%                                                                                                                 %%
%% Esta gramática tiene limitaciones para generar interrogativas de objeto y sujeto. Nótese que en                 %%
%% esta gramática "qué" siempre es objeto y "quién" siempre sujeto. No obstante en el español real                 %%
%% debería ser posible construir oraciones con "qué" con función sujeto. De hecho, la oración 6 es ambigua:        %%
%%                                                                                                                 %%
%% 6. ¿Qué golpeó la piedra?                                                                                       %%
%%    Interpretación: Qué[suj] / la piedra[obj]                                                                    %%
%%    Interpretación: Qué[obj] / la piedra[suj]                                                                    %%
%%                                                                                                                 %%
%% En este caso hay alternativa simple para enriquecer la gramática y generar las dos interpretaciones.            %%
%% Uno de los problemas surge del orden de constituyentes. El orden de 7 solo es analizado como en 8 y             %%
%% no como en 9:                                                                                                   %%
%%                                                                                                                 %%
%% 7. Qué>verbo>DP                                                                                                 %%
%% 8. Suj>verbo>Obj                                                                                                %%
%% 9. Obj>verbo>Suj                                                                                                %%
%%                                                                                                                 %%
%% IMPORTANTE: Para el TP las interrogativas son de Sujeto y las construcciones de Foco solo se dan con el objeto. %%
%% Esto implica que no van a tener ninguno de estos inconvenientes.                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
