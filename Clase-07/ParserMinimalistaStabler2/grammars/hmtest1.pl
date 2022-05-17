[]::[='T','C'].  
[]::[=>'T',+wh,'C'].  

[]::[=>v,+nom,'T'].
[]::[=>'V',='D',v].
[pinta]::[='D',+ac,'V'].
[pared]::['N'].
[la]::[='N','D',-ac].
['Ivan']::['D',-nom].
['que']::['D',-ac,-wh].
['quien']::['D',-nom,-wh].
startCategory('C').

% showParse(['Ivan',pinta,la,pared]).
% showParse(['quien',pinta,la,pared]).