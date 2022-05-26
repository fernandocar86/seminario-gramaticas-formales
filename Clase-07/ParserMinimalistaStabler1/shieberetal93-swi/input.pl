/*----------------------------------------------------------
        READING SENTENCES AND PREPARING PARSER INPUT
----------------------------------------------------------*/

%%% sentencelength(L)
%%% =================
%%%
%%%     L is the length of the sentence being parsed.

:- dynamic sentencelength/1.


%%% word(I, W)
%%% ==========
%%%
%%%     W is the Ith word in the sentence being parsed.

:- dynamic word/2.


%%% read_input
%%% ==========
%%%
%%%     Read a sentence from the user and assert its words
%%%     and length.  

read_input :-
    read_in(S),
    encode_sentence(S).


%%% encode_sentence(+Sentence)
%%% =========================
%%%
%%%     Clear input, store and encode input Sentence.

encode_sentence(Sentence) :-
    retractall(word(_,_)),
    retractall(sentencelength(_)),
    encode_words(Sentence, 0, Length),
    assert(sentencelength(Length)).


%%% encode_words(+Words, +P0, -P)
%%% =============================
%%%
%%%     Store input Words from position P0 + 1 to P.

encode_words(['.'], Length, Length) :- !.
encode_words([Word|Words], Length0, Length) :-
    Length1 is Length0 + 1,
    assert(word(Length1,Word)),
    encode_words(Words, Length1, Length).

