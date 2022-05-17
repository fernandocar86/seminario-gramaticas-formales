[]::[='T','C'].  
[]::[=>'T',+wh,'C'].  

['-ó']::[=>v,+nom,'T'].
[]::[=>'V',='D',v].
[pint]::[='D',+ac,'V'].
[pared]::['N'].
[la]::[='N','D',-ac].
['Iván']::['D',-nom].
['qué']::['D',-ac,-wh].
['quién']::['D',-nom,-wh].
startCategory('C').

% showParse(['Iván',pint,'-ó',la,pared]).
% showParse(['quién',pint,'-ó',la,pared]).
% showParse(['qué',pint,'-ó','Iván']).