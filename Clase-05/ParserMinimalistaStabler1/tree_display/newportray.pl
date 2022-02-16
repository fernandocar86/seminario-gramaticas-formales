%    File: newportray.pl
%  Author: E.P. Stabler
% Updated: Aug 1998
% Purpose: minor adjustments in portray for glc code
%      CHanged lines marked with NEW,
%      for MG with only overt movement -- no strong,weak distinctions

:- op(500, fx, <).	% prefix for historical info
:- op(500, fx, =).	% as prefix, indicates selection feature
:- op(500, xf, @).	% as suffix, indicates selection feature

portray('~'(X)) :- write('~'),portray(X).
portray([H|T]) :- write(''''), portray_list([H|T]).
%portray([H|T]+L) :- portray_list([H|T]), write('{'), portray(L), write('}').
%portray([H|T]+L+M) :- 
%	portray_list([H|T]), 
%	write('{'), portray(L), write('}{'), portray(M),write('}').
%portray([]) :- !, write(e).
portray([]).
%portray([]+L):- write('{'), portray(L), write('}').
%portray([]+L+M) :- write('{'), portray(L), write('}{'), portray(M),write('}').

%portray_list(F) :- var(F), print(F).
%portray_list([ph(F),ii(F)]) :- !, write(F).
%portray_list([ph(F),ii(F)|Fs]) :- !, write(F), write(' '), portray_list(Fs).
%portray_list([ph(F),ph(F1)|Fs]) :- !, 
%	write('/'),write(F),write(' '),
%	portray_phon([ph(F1)|Fs]).
%portray_list([ii(F),ii(F1)|Fs]) :- !, 
%	write('('),write(F),write(' '),
%	portray_int([ii(F1)|Fs]).

portray_list([F]) :- !, portray(F), write('''').
portray_list([F|Fs]) :- portray(F), write(' '), portray_list(Fs).
portray_list([]) :-  write('''').

%portray_phon([ph(F),ph(F1)|Fs]) :- !, 
%	write(F),write(' '),
%	portray_phon([ph(F1)|Fs]).
%portray_phon([ph(F)]) :- !, 
%	write(F),write('/').
%portray_phon([ph(F)|Fs]) :- !,
%	write(F),write('/'),
%	portray_list(Fs).

%portray_int([ii(F),ii(F1)|Fs]) :- !, 
%	write(F),write(' '),
%	portray_int([ii(F1)|Fs]).
%portray_int([ii(F)]) :- !, 
%	write(F),write('/').
%portray_int([ii(F)|Fs]) :- !,
%	write(F),write('/'),
%	portray_list(Fs).

portray([]).

portray(ph(X)) :- write('/'), write(X), write('/').
portray(ii(X)) :- write('('), write(X), write(')').

% avoid parens in these prefix operators:
portray(-X) :- write('-'), print(X).
%NEW portray(+s(X)) :- !, write('+'), word_to_upper(X, U), print(U).
portray(+s(X)) :- !, write('+'), print(X).
portray(+w(X)) :- !, write('+'), print(X).
%NEW portray(=s(X)) :- !, write('='), word_to_upper(X, U), print(U).
portray(=s(X)) :- !, write('='), print(X).
portray(=w(X)) :- !, write('='), print(X).
portray(s(X)@) :- !, word_to_upper(X, U), print(U), write('=').
portray(+X) :- write('+'), print(X).
portray(<(+s(X))) :- !, write('<+'), word_to_upper(X, U), print(U).
portray(<(+w(X))) :- !, write('<+'), print(X).
portray(X/[]) :- portray(X).
portray(X/[T]) :- T==0, portray(X).
portray('<') :- !, write(''''),write('<'),write('''').
portray('>') :- !, write(''''),write('>'),write('''').
portray(' ') :- !, writeq(' ').
portray(X) :- write(X).

word_to_upper(Word,UpperWord) :-
	name(Word, Chars),
	chars_to_upper(Chars, UpperChars),
	name(UpperWord, UpperChars).

chars_to_upper([],[]).
chars_to_upper([C|Cs],[U|Us]) :-
        (   C >= "a", C =< "z", !, U is C-"a"+"A"
        ;   C =:= "_" , !, U = C
        ;   C >= "A", C =< "Z", !, U = C
        ;   C >= "0", C =< "9", !, U = C
        ),
	chars_to_upper(Cs,Us).
