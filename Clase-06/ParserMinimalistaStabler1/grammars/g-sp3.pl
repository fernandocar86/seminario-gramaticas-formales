%   File   : g-ne.pl - naive english
%   Author : E Stabler
%   Updated: Mar 00

[] :: [=i,c].                [] :: [=i,+wh,c].
[] :: [=pred,+v,+k,i].   [] :: [=vt,+k,=d,pred].    [] :: [=v,pred].
[come] :: [=d,vt,-v].      [canta] :: [=d,v,-v].
['la torta'] :: [d,-k].         [fernando] :: [d,-k].         [que] :: [d,-k,-wh].

startCategory(c).


% showParse([who,praise,'-s',titus]).
% showParse([fernando,canta]).