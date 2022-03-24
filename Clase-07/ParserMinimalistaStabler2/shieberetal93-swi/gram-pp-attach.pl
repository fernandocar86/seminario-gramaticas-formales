%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -*- Mode: Prolog -*- %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gram-pp-attach.pl --- 
%% Author          : Frank Morawietz
%% Created On      : Tue Oct 26 17:36:57 1999
%% Last Modified By: Frank Morawietz
%% Last Modified On: Tue Oct 26 17:47:22 1999
%% Update Count    : 1
%% Status          : Unknown, Use with caution!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Simple PP attachment grammar

lex(a,det(det(a))).
lex(the,det(det(the))).
lex(beautiful,adj(adj(beautiful))).
lex(ugly,adj(adj(ugly))).
lex(black,adj(adj(black))).
lex(white,adj(adj(white))).
lex(man,nou(nou(man))).
lex(dog,nou(nou(dog))).
lex(woman,nou(nou(woman))).
lex(telescope,nou(nou(telescope))).
lex(stick,nou(nou(stick))).
lex(saw,ver(ver(saw))).
lex(hit,ver(ver(hit))).
lex(with,pre(pre(with))).


s(s(NP,VP)) ---> [np(NP), vp(VP)].
vp(vp(TV,NP)) ---> [ver(TV), np(NP)].
vp(vp(TV,NP,PP)) ---> [ver(TV), np(NP), pp(PP)].
pp(pp(PRE,NP)) ---> [pre(PRE), np(NP)].
np(np(Det,NB)) ---> [det(Det), nb(NB)].
nb(nb(NOU)) ---> [nou(NOU)].
nb(nb(NOU,PP)) ---> [nou(NOU),pp(PP)].
nb(nb(ADJ,NB)) ---> [adj(ADJ),nb(NB)].


startsymbol(s(_)).
