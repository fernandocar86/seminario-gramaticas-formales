s(s(NP,VP)) ---> [np(NP), vp(VP)].
np(np(Det,N,Rel)) ---> 
    [det(Det), n(N), optrel(Rel)].
np(np(PN)) ---> [pn(PN)].
vp(vp(TV,NP)) ---> [tv(TV), np(NP)].
vp(vp(IV)) ---> [iv(IV)].
optrel(rel(that,VP)) ---> [relpro, vp(VP)].
%optrel(rel(epsilon)) ---> [].

lex(that, relpro).
lex(terry, pn(pn(terry))).
lex(shrdlu, pn(pn(shrdlu))).
lex(halts, iv(iv(halts))).
lex(a, det(det(a))).
lex(program, n(n(program))).
lex(writes, tv(tv(writes))).

startsymbol(s(_)).
