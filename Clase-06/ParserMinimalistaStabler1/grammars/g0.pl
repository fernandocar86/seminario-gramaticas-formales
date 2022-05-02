%   File   : g0.pl
%   Author : E Stabler
%   Updated: Mar 00

%:- op(500, xfy, ::). % lexical items
%:- op(500, fx, =). % for selection features

[] :: [='T','C'].               [] :: [='T',+wh,'C'].
[] :: [=v,+k,'T'].              [] :: [='V',='D',v].
[eat] :: [='D',+k,'V'].   %      [laugh] :: ['V'].
[the] :: [='N','D',-k].    %     [which] :: [='N','D',-k,-wh].
[king] :: ['N'].            %    [pie] :: ['N'].

['John'] :: ['D',-k].  
['Mary'] :: ['D',-k].
[likes] :: [='D',+k,'V'].

startCategory('C').

% showParse([John,Mary,likes]).
% showParse([which,pie,John,eat]).