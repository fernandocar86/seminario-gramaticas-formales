% File   : g0sp-caso.pl
% Author : Pablo Zdrojewski
% Updated: 2022 
% Gramática diseñada por Pablo Zdrojewki a partoo la gramática g0.pl  diseñada por Stabler (Mar 2000). Para uso interno de Seminario "Gramáticas formales: formalismos e implementaciones"  (UBA).

:- encoding(utf8).

%:- op(500, xfy, ::). % lexical items
%:- op(500, fx, =). % for selection features


%%% Complementantes
[] :: [=t,c].               [] :: [=t,+wh,c].


%% Tiempo
[] :: [=voice,+h,+nom,t].              

%% voice 
[] :: [=vt,=dnom,+ac,voice].
[] :: [=vt,=dnom,voice].


%%% Verbos v: verbo instransitivo / vt: verbo transitivo
[sonrie] :: [v,-h].
[canta]  :: [v,-h].   
[come] :: [=dac,vt,-h].        
[vio] :: [=dac,vt,-h].   
[cocina] :: [=dac,vt,-h]. 

%%% Determinantes
['cuál'] :: [=nf,dac,-ac,-wh].
['cuál'] :: [=nf,dnom,-nom,-wh].
['cuál'] :: [=nm,dac,-ac,-wh].
['cuál'] :: [=nm,dnom,-nom,-wh].
[el] :: [=nm,dnom,-nom].        
[el] :: [=nm,dac,-ac].        
[la] :: [=nf,dnom,-nom].
[la] :: [=nf,dac,-ac].


%%% Nombres comunes
%[perro] :: [nm].                
[torta] :: [nf].
%[hueso] :: [nm].


%%% Nombres propios
[fernando] :: [dnom,-nom].      
[julia] :: [dnom,-nom].   
[macarena] :: [dnom,-nom].
[pablo] :: [dnom,-nom].  

startCategory('c').


