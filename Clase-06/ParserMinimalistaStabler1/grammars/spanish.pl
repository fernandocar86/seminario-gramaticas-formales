% Fernando Carranza (based on John Hale's grammar larsonian1.pl)
% Para uso interno de Modelos Formales No Transformacionales (UBA)

:- encoding(utf8).

% C Category 

[]::[='T','C'].                  % regular empty complementizer

% T category

%[tiempo]::[=>'Pas',+case,'T'].
['-pres']::[v==>,+case,'T'].
['-pdo']::[=>v,+case,'T']. 
['-fut']::[=>v,+case,'T'].

% v category
% little v gets the subject

%[v]::[=>'V',='D',v].
[]::[=>'V',='D',v]. % v vacío

% Pasiva
['-do/a']::[=>v,'Pas'].

% V category

% intransitive verbs
[nadar]::['V'].
[caminar]::['V'].
[fumar]::['V'].
[bailar]::['V'].
[explotar]::['V'].


% transitive verbs
%[verbo]::[='D',+case,'V']. 

[leer]::[='D',+case,'V']. 
[contar]::[='D',+case,'V']. 
[amar]::[='D',+case,'V']. 
[odiar]::[='D',+case,'V']. 
[escribir]::[='D',+case,'V'].
[vender]::[='D',+case,'V']. 
[dejar]::[='D',+case,'V']. 
[amar]::[='D',+case,'V'].
[comprar]::[='D',+case,'V']. 
[traer]::[='D',+case,'V']. 
[tomar]::[='D',+case,'V']. 
[enviar]::[='D',+case,'V'].
[tener]::[='D',+case,'V']. 


% Determiner Phrase

% Determiners

[el]::[='N','D',-case].     % ordinary determiner
[la]::[='N','D',-case].     % ordinary determiner
[los]::[='N','D',-case].     % ordinary determiner
[las]::[='N','D',-case].     % ordinary determiner

% Demostrative Determiners

[este]::[='N','D',-case].     % demostrative determiner
[esta]::[='N','D',-case].     % demostrative determiner
[estos]::[='N','D',-case].     % demostrative determiner
[estas]::[='N','D',-case].     % demostrative determiner


% proper nouns
['Juan']::['D',-case].
['María']::['D',-case].
['Cata']::['D',-case].
['Fede']::['D',-case].
['Chafa']::['D',-case].
['Martín']::['D',-case].
['Vicky']::['D',-case].
['Fer']::['D',-case].
['Maca']::['D',-case].
['Santi']::['D',-case].


% common nouns
[historia]::['N'].
[revista]::['N'].
[libro]::['N'].
[chico]::['N']. 
[chica]::['N'].



startCategory('C').
