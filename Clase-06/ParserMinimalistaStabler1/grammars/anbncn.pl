%   File   : anbncn.pl
%   Author : E Stabler
%   Updated: Oct 1999
%   Purpose: sample grammar for mgp. This one generates a^nb^nc^n4^n5^n as an s


[] :: ['S'].      [] :: [='T1',+a,'S'].      
[] :: [='T2',+b,'T1'].       [] :: [='A',+c,'T2'].   

[a] :: [='B','A',-a].        [a] :: [='B',+a,'A',-a].
[b] :: [='C','B',-b].        [b] :: [='C',+b,'B',-b].
[c] :: ['C',-c].             [c] :: [='A',+c,'C',-c].

startCategory('S').
