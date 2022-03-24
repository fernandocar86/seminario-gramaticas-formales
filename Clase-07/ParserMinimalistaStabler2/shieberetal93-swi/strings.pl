%   Module : strings
%   Author : Richard A. O'Keefe
%   Updated: 19 Aug 1994
%   Purpose: define string_size/2, string_char/3, substring/5.
%   SeeAlso: strings.c

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(strings, [
	atom_chars1/2,		% atom_chars/2 is now built in
	cgensym/2,
	char_atom/2,
	compare_strings/3,
	compare_strings/4,
	concat/3,
	concat_atom/2,
	concat_atom/3,
	concat_chars/2,
	concat_chars/3,
%	concat_string/2,
%	concat_string/3,
	gensym/1,
	gensym/2,
	index/3,
	midstring/3,
	midstring/4,
	midstring/5,
	midstring/6,
	name/1,
	name1/2,
	nth_char/3,
	number_chars1/2,	% number_chars/2 is now built in
	span_left/3,
	span_left/4,
	span_left/5,
	span_right/3,
	span_right/4,
	span_right/5,
	span_trim/2,
	span_trim/3,
	span_trim/5,
	string/1,
	string_append/3,
	string_char/3,
%	string_chars/2,
	string_length/2,
	string_search/3,
	string_size/2,
	subchars/4,
	subchars/5,
	substring/4,
	substring/5,
	system/1
   ]).

/*
:- use_module(library(types), [
%       must_be/3,
	should_be/3
   ]).
*/

:- ensure_loaded(types).



:- mode				% 'quoted preds' for internal use ONLY!
	'Enum'(+, -),
	'Enum'(+, +, -),
	'Enum'(+, +, +, -),
	'Plus'(?, ?, ?, +),
	'QgChars'(+, +, ?),
	'QpChars'(+),
	cgensym(+, ?),
	char_atom(?, ?),
	concat(?, +, ?),
	concat_atom(+, ?),
	concat_atom(+, +, ?),
	concat_chars(+, ?),
	concat_chars(+, +, ?),
	concat_string(+, ?),
	concat_string(+, +, ?),
	'deposit set'(+, -),
	'deposit texts'(+),
	'deposit texts'(+, -),
	'deposit text 2'(+),
	'deposit texts 2'(+, +),
	'deposit texts 2'(+, +, -),
	gensym(?),
	gensym(+, ?),
	index(+, +, ?),
	index(+, +, +, ?),
	midstring(?, ?, ?),		% + ? ? or - + +
	midstring(?, ?, ?, ?),		% + ? ? or - + +
	midstring(?, ?, ?, ?, ?),	% + ? ? or - + +
	midstring(?, ?, ?, ?, ?, ?),	% + ? ? or - + +
	name(+),
	name1(?, ?),
	'name error'(+, +, +, +),
	nth_char(?, +, ?),
	'proper chars'(+, -, -),
	'proper chars'(+, +, -, +, -),
	span_left(+, +, ?),
	span_left(+, +, ?, ?),
	span_left(+, +, ?, ?, ?),
	span_right(+, +, ?),
	span_right(+, +, ?, ?),
	span_right(+, +, ?, ?, ?),
	span_trim(+, ?),
	span_trim(+, +, ?),
	span_trim(+, +, ?, ?, ?),
	string(+),
	string_append(?, ?, ?),
	string_char(+, +, ?),
	string_length(+, ?),
	string_search(?, +, ?),
	string_size(+, ?),
	subchars(+, ?, ?, ?),
	subchars(+, ?, ?, ?, ?),
	substring(+, ?, ?, ?),
	substring(+, ?, ?, ?, ?),
	system(+).

sccs_id('"@(#)94/08/19 strings.pl	73.1"').

:- dynamic
	gensym_counter/2.


/*  Xerox Quintus Prolog supports strings, so that it can do something
    sensible when Lisp gives it a string.  Strings are in fact pretty
    pointless in Prolog.  If you want to perform the classical string
    operations, this library provides everything you I've seen suggested
    for Prolog using atoms (and Prolog atoms are isomorphic to Snobol
    strings).  If you want to do pattern matching, the power and clarity
    of grammar rules operating on lists of character codes (the "chars"
    data type) have only been equalled by Snobol and have never been
    surpassed in any language.  Accordingly, Quintus Prolog doesn't have
    a separate string data type except when sharing the world with
    another language which has them, and currently Xerox Quintus Prolog
    is the only such share.  We can do much fancier things using
    character lists and grammar rules.  So we really do recommend that
    you should look for almost any alternative before using these
    predicates, otherwise you may be seriously underusing Prolog.
    Some of the predicates are provided for Arity/Prolog compatibility.
    nth_char/3 is a variant of string_char/3.
    string_length/2 is a variant of string_size/2.
    string_search/3 is a variant of substring/5.
    These three predicates are included as a gesture of courtesy
    to Arity/Prolog.  substring/4 is already close enough.
*/

%   string(X)
%   is true when X is a string.  Hence, on stock hardware,

string(_) :-
	fail.


%   name(X)
%   is true when X is the kind of thing that has a name; that is,
%   it is an atom or a string.  See the comment above.

name(X) :-
	atom(X).


%   'proper chars'(Chars, Length, State)
%   checks an argument which is supposed to be a list of character codes.
%   There are three successful cases:
%	State = -1 : Chars ends with a variable
%	State =  0 : Chars is a proper list, but some elements are vars
%	State =  1 : Chars is a proper list of characters
%   In the latter two cases, Length is bound to the length of the list.

'proper chars'(Chars, Length, State) :-
	'proper chars'(Chars, 0, Length, 1, S),
	State = S.

'proper chars'(Chars, _, _, _, -1) :-
	var(Chars),
	!.
'proper chars'([], Length, Length, State, State).
'proper chars'([Char|Chars], L, Length, S, State) :-
	integer(Char), Char >= 0, Char =< 255,
	!,
	M is L+1,
	'proper chars'(Chars, M, Length, S, State).
'proper chars'([Char|Chars], L, Length, _, State) :-
	var(Char),
	M is L+1,
	'proper chars'(Chars, M, Length, 0, State).


%   system(ListOfTextObjects)
%   is a version of unix(shell(_)) or vms(dcl(_)) which builds
%   the command up out of pieces without the cost of interning a
%   new atom which may never be used again.  It does whatever the
%   C function system() does.

system(Texts) :-
	'deposit texts'(Texts, Length),
	Length > 0,
	'Qsystem'(0).


%   concat_atom(+ListOfTextObjects, -Combined)
%   concatenates the names of all the text objects in the list
%   and returns an atom whose name is the combined characters.
%   Valid text objects are either character lists, atoms or numbers.
%   Xerox Quintus Prolog implements concat_string/2 non-trivially.
%   concat_chars/2 returns a list of character codes.
%   Note: The limit on the length of Combined is ALIMIT in strings.c.
%   Even though QP 3.2 supports atoms of length upto 32K,
%   ALIMIT is set to 1023 by default in strings.c. If you want concat_atom/2
%   to build longer atoms change ALIMIT in strings.c.
%   This limit holds for concat_chars/2 as well!

concat_atom(Texts, Atom) :-
	'deposit texts'(Texts, Length),
	'QgAtom'(0, Length, Atom, 0).

/*
concat_string(Texts, String) :-
	'deposit texts'(Texts, Length),
	'QgString'(0, Length, Atom, 0).
*/
/*
    Now leave this undefined -- will get an existence error

concat_string(Texts, String) :-
	format(user_error,
	    '~N! No strings in this version; use atoms.~n! Goal: ~q~n',
	    [concat_string(Texts,String)]),
	fail.
*/
concat_chars(Texts, Chars) :-
	'deposit texts'(Texts, Length),
	'QgChars'(0, Length, Chars).

'deposit texts'(Texts, Length) :-
	'QpInit',
	'deposit texts'(Texts),
	'QgInit'(Length).

'deposit texts'([]).
'deposit texts'([Text|Texts]) :-
	(   atom(Text) -> 'QpAtom'(Text, _)
	;   integer(Text) -> 'QpInt'(Text, _)
	;   float(Text) -> 'QpFlt'(Text, _)
	;   'QpChars'(Text)	%  only works for a "chars"
	),
	'deposit texts'(Texts).


%   concat_atom(+ListOfTextObjects, +Separator, -Combined)
%   concatenates the names of all the text objects in the list
%   and returns an atom whose name is the combined characters.
%   The Separator is inserted between elements of ListOfTextObjects,
%   so that concat_atom([a,b,c], +, X) binds X + 'a+b+c'.
%   Valid text objects are either character lists, atoms or numbers.
%   Xerox Quintus Prolog implements concat_string/3 non-trivially.
%   concat_chars/3 returns a list of character codes.

concat_atom(Texts, Separator, Atom) :-
	'deposit texts 2'(Texts, Separator, Length),
	'QgAtom'(0, Length, Atom, 0).

/*
concat_string(Texts, Separator, String) :-
	'deposit texts 2'(Texts, Separator, Length),
	'QgString'(0, Length, Atom, 0).
*/
/*
concat_string(Texts, Separator, String) :-
	format(user_error,
	    '~N! No strings in this version; use atoms.~n! Goal: ~q~n',
	    [concat_string(Texts,Separator,String)]),
	fail.
*/

concat_chars(Texts, Separator, Chars) :-
	'deposit texts 2'(Texts, Separator, Length),
	'QgChars'(0, Length, Chars).

'deposit texts 2'([], _, Length) :- !,
	'QpInit',
	'QgInit'(Length).	% sets Length=0
'deposit texts 2'([Text|Texts], Separator, Length) :-
	'QpInit',
	'deposit text 2'(Text),
	'deposit texts 2'(Texts, Separator),
	'QgInit'(Length).

'deposit texts 2'([], _).
'deposit texts 2'([Text|Texts], Separator) :-
	'deposit text 2'(Separator),
	'deposit text 2'(Text),
	'deposit texts 2'(Texts, Separator).

'deposit text 2'(Text) :-
	(   atom(Text) -> 'QpAtom'(Text, _)
	;   integer(Text) -> 'QpInt'(Text, _)
	;   float(Text) -> 'QpFlt'(Text, _)
	;   'QpChars'(Text)	%  only works for a "chars"
	).



%   concat(?Name1, +Name2, ?Name3)
%   is like append on atoms.  That is, it appends the name of
%   Name1 and the name of Name2, and binds Name3 to the atom named
%   by the result.  It can be used to find either Name1 or Name3,
%   but not to find Name2.  The reason for that is that
%   concat('a', X, 'a1') has two solutions:  X = 0 and X = '0',
%   and there is no easy way to find the latter.  Examples:
%   concat(a, b, ab), concat(10, 23, 1023), concat(gs, 46, gs46).
%   Name2 may be numeric, but Name1 and Name3 may not be.
%   When strings are supported, Name1 and Name3 may both be(come)
%   strings or may both be(come) atoms, but mixing is not allowed.
%   Name2 may be a string, which is yet another ambiguity, X = $0$.
%   For rather hacky reasons having to do with avoiding append/3,
%   you can now pass a non-empty chars as the second argument.

concat(N1, N2, N3) :-
	(   atom(N1) ->
		concat_atom([N1,N2], N3)
	;   atom(N3) ->
		(   atomic(N2) ->
		    'deposit texts'([N2], L), 'QgChars'(0, L, Chars)
		;   Chars = N2, 'proper chars'(Chars, L, 1)
		),
		'QpInit',
		'QpAtom'(N3, Length),
		M is Length-L,
		'QgChars'(M, Length, Chars),
		'QgAtom'(0, M, N1, 0)
	).



%   string_append(A, Z, AZ)
%   is true when A, Z, and AZ are all three atoms or (Lisp) strings,
%   and the name of AZ is the concatenation of the names of A and Z.
%   We could define this as
%   string_append(A, Z, AZ) :-
%	midstring(AZ, A, Z, 0).
%   but doing it this way is a bit faster and produces a better
%   error message.

string_append(A, Z, AZ) :-
	var(AZ),
	atom(A),
	atom(Z),
	!,
	'QpInit',
	'QpAtom'(A, _),
	'QpAtom'(Z, Length),
	'QgAtom'(0, Length, AZ, 0).
string_append(A, Z, AZ) :-
	atom(AZ),
	string_size(AZ, AZ_len),
	(   atom(A) ->
	    string_size(A, A_len),
	    (   atom(Z) ->
		string_size(Z, Z_len),
		!,
		Z_len is AZ_len-A_len,
		'Qsubchk'(AZ, 0,     A_len, A, 0),
		'Qsubchk'(AZ, A_len, Z_len, Z, 0)
	    ;   var(Z) ->
		!,
		Z_len is AZ_len-A_len,
		'Qsubchk'(AZ, 0,     A_len, A, 0),
		'Qsubstr'(AZ, A_len, Z_len, Z, 0)
	    )
	;   var(A) ->
	    (   atom(Z) ->
		!,
		string_size(Z, Z_len),
		A_len is AZ_len-Z_len,
		'Qsubchk'(AZ, A_len, Z_len, Z, 0),
		'Qsubstr'(AZ, 0,     A_len, A, 0)
	    ;   var(Z) ->
		!,
		'Enum'(0, AZ_len, A_len),		
		Z_len is AZ_len-A_len,
		'Qsubstr'(AZ, 0,     A_len, A, 0),
		'Qsubstr'(AZ, A_len, Z_len, Z, 0)
	    )
	).
string_append(A, Z, AZ) :-
	Goal = string_append(A,Z,AZ),
	(   atom(A), atom(Z) ->
	    should_be(atom, 3, Goal)
	;   should_be(atom, 1, Goal),
	    should_be(atom, 2, Goal)
	).



%   string_size(+StringOrAtom, ?Length)
%   is defined entirely in C, for the moment.  It unifies Length with the
%   number of characters in (the name of) StringOrAtom.
%   string_length/2 is included as a courtesy to Arity; it is slower but
%   does better checking.

string_length(StringOrAtom, Length) :-
	(   atom(StringOrAtom) ->	% should be name(StringOrAtom)
	    string_size(StringOrAtom, Length)
	;   should_be(atom, 1, string_length(StringOrAtom, Length))
	).


%   string_char(?Index, +StringOrAtom, ?Char)
%   unifies Char with the ISO8859 code of the Indexth character of the
%   StringOrAtom.  If Index is not an integer, is less than one, or exceeds
%   the length of StringOrAtom, it just fails quietly.  If StringOrAtom is
%   not an atom, it just fails quietly.  It should report an error if the
%   StringOrAtom is unbound, and it would be good in other non-atom cases too.

string_char(Index, StringOrAtom, Char) :-
	atom(StringOrAtom),	% should be name(StringOrAtom)
	(   integer(Index), !,	% first two arguments well formed
	    'Qstrchr'(Index, StringOrAtom, C),
	    C >= 0,		% Index out of range <=> C = -1
	    Char = C
	;   var(Index), !,	% first two arguments well formed
	    string_size(StringOrAtom, Size),
	    'Enum'(1, Size, Index),
	    'Qstrchr'(Index, StringOrAtom, Char)
	).
string_char(Index, StringOrAtom, Char) :-
	%   The first two arguments are not well formed.
	Goal = string_char(Index, StringOrAtom, Char),
	(   atom(StringOrAtom) ->
	    should_be(integer, 1, Goal)
	;   should_be(atom, 2, Goal)
	).


%   nth_char(?Index, +StringOrAtom, ?Char)
%   is identical to string_char, except that Index counts from 0, which
%   is inconsistent with almost every other string package including
%   this one, and doesn't fit well into the algebra of strings.
%   Read it as, "if you throw away the first Index characters
%   of StringOrAtom, the ISO8859 code of the next character is Char".

nth_char(Index, StringOrAtom, Char) :-
	atom(StringOrAtom),	% should be name(StringOrAtom)
	(   integer(Index), !,	% first two arguments well formed
	    K is Index+1,
	    'Qstrchr'(K, StringOrAtom, C),
	    C >= 0,		% Index out of range <=> C = -1
	    Char = C
	;   var(Index), !,	% first two arguments well formed
	    string_size(StringOrAtom, Size),
	    'Enum'(1, Size, K),
	    'Qstrchr'(K, StringOrAtom, Char),
	    Index is K-1
	).
nth_char(Index, StringOrAtom, Char) :-
	%   The first two arguments are not well formed.
	Goal = nth_char(Index, StringOrAtom, Char),
	(   atom(StringOrAtom) ->
	    should_be(integer, 1, Goal)
	;   should_be(atom, 2, Goal)
	).



%   char_atom(Char, Atom)
%   is true when Char is the ISO8859 code of some character and Atom is
%   an atom whose name is that single character, e.g. char_atom(46, .).

char_atom(Char, Atom) :-
    (	integer(Char) ->
	Char >= 1, Char =< 255,
	'Qchatom'(Char, Atom)
    ;	nonvar(Atom) ->
	'Qatomch'(Atom, Char),		% tests that atom(Atom)
	Char >= 0
    ;	var(Char) ->
	'Enum'(1, 255, Char),
	'Qchatom'(Char, Atom)
    ).


'Enum'(L, U, X) :-
	(   L =:= U -> X = L
	;   L < U, ( X = L ; M is L+1, 'Enum'(M, U, X) )
	).



%   index(Pattern, String, Offset)
%   is true when Pattern occurs in String after skipping Offset characters.
%   Apart from the fact that String cannot be generated (because there are
%   generally infinitely many suitable values) this is fully relational;
%   index(Pattern, String, 0) is not only an easy way to check whether
%   Pattern is a prefix of String, it is an efficient way too.

index(Pattern, String, Offset) :-
	substring(String, Pattern, Offset, _, _).


%   string_search(Pattern, String, Offset)
%   is identical to index/3; and is included as a courtesy to Arity.

string_search(Pattern, String, Offset) :-
	substring(String, Pattern, Offset, _, _).


%   substring(StringOrAtom, SubString, Offset, Length)
%   unifies SubString with the substring of StringOrAtom starting Offset
%   characters from the beginning and continuing for Length characters.
%   For example, substring(lamentation, mentat, 2, 6).

substring(StringOrAtom, SubString, Offset, Length) :-
	substring(StringOrAtom, SubString, Offset, Length, _).


%   substring(StringOrAtom, SubString, F, S, B)
%   unifies SubString with the substring of StringOrAtom which contains
%   S(elect) characters, is preceded by F(ront) characters in StringOr-
%   Atom, and is followed by B(ack) characters in StringOrAtom.  For
%   example, substring(othello, hell, _, 4, 1).  Only StringOrAtom has
%   to be supplied; this predicate is capable of generating the rest.

substring(Whole, Part, DropFromHead, LengthOfPart, DropFromTail) :-
	var(Part),
	!,
	string_size(Whole, LengthOfWhole),	% checks that atom(Whole)
	'Plus'(DropFromHead, LengthOfPart, DropFromTail, LengthOfWhole),
	'Qsubstr'(Whole, DropFromHead, LengthOfPart, Part, 0).
substring(Whole, Part, DropFromHead, LengthOfPart, DropFromTail) :-
	string_size(Part, LengthOfPart),	% checks that atom(Part)
	!,
	string_size(Whole, LengthOfWhole),	% checks that atom(Whole)
	(   nonvar(DropFromHead) ->
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead,
		'Qsubchk'(Whole, DropFromHead, LengthOfPart, Part, 0)
	;   nonvar(DropFromTail) ->
		DropFromHead is LengthOfWhole-LengthOfPart-DropFromTail,
		'Qsubchk'(Whole, DropFromHead, LengthOfPart, Part, 0)
	;   /* otherwise we must search */
		'Enum'(Whole, Part, -1, DropFromHead),
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead
	).


%   subchars(StringOrAtom, Chars, Offset, Length)
%   unifies Chars with the substring of StringOrAtom starting Offset
%   characters from the beginning and continuing for Length characters.
%   For example, substring(lamentation, "mentat", 2, 6).
%   More generally, this will accept ANY kind of string as first
%   argument, and always takes a chars second argument.

subchars(StringOrAtom, Chars, Offset, Length) :-
	subchars(StringOrAtom, Chars, Offset, Length, _).


%   subchars(StringOrAtom, Chars, F, S, B)
%   unifies Chars with the substring of StringOrAtom which contains
%   S(elect) characters, is preceded by F(ront) characters in StringOr-
%   Atom, and is followed by B(ack) characters in StringOrAtom.  For
%   example, subchars(othello, "hell", _, 4, 1).  Only StringOrAtom has
%   to be supplied; this predicate is capable of generating the rest.
%   More generally, this will accept ANY kind of string as first
%   argument, and always takes a chars second argument.

subchars(Whole, Chars, DropFromHead, LengthOfPart, DropFromTail) :-
	string_size(Whole, LengthOfWhole),	% checks that atom(Whole)
	'proper chars'(Chars, LengthOfPart, Flag),
	(   nonvar(DropFromHead), Flag >= 0 ->
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead,
		DropFromTail >= 0,
		'QpInit',
		'QpPart'(Whole, DropFromHead, LengthOfPart, LengthOfPart),
		'QgChars'(0, LengthOfPart, Chars)
	;   nonvar(DropFromTail), Flag >= 0 ->
		DropFromTail >= 0,
		DropFromHead is LengthOfWhole-LengthOfPart-DropFromTail,
		'QpInit',
		'QpPart'(Whole, DropFromHead, LengthOfPart, LengthOfPart),
		'QgChars'(0, LengthOfPart, Chars)
	;   Flag > 0 ->				% fully specified
		atom_chars(Part, Chars),	% so we can do this!
		'Enum'(Whole, Part, -1, DropFromHead),
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead
	;   /* otherwise, we don't know what Chars looks like */
		'Plus'(DropFromHead, LengthOfPart, DropFromTail, LengthOfWhole),
		'QpInit',
		'QpPart'(Whole, DropFromHead, LengthOfPart, LengthOfPart),
		'QgChars'(0, LengthOfPart, Chars)
	).


%   midstring(Whole, Part, Fringes, Before, Length, After)
%   is true when Whole, Part, and Fringes are all three atoms or are
%   all three strings,
%	Whole = Alpha || Part || Omega,
%	Fringes = Alpha || Omega,
%	string_size(Alpha, Before),
%	string_size(Part,  Length),
%	string_size(Omega, After)
%   midstring/[5,4,3] leave the trailing arguments unspecified.
%   This family has many uses.
%   To divide Whole into a Front and Back from the left:
%	midstring(Whole, Front, Back, 0, FrontLength, _)
%   To divide Whole into a Front and Back from the right:
%	midstring(Whole, Front, Back, 0, _, BackLength)
%   To concatenate Front and Back giving Whole:
%	midstring(Whole, Front, Back, 0)
%   To insert Part into Fringes at a given Position, yielding Whole:
%	midstring(Whole, Part, Fringes, Position)
%   To delete Drop characters from Whole, yielding Fringes:
%	midstring(Whole, _, Fringes, Take, Drop)
%   Either Whole must be a non-variable, or
%   both Part and Fringes must be non-variables.
%   If neither is the case, an instantiation fault is reported.
%   If either is the case, the relation can go ahead.

midstring(Whole, Part, Fringes) :-
	midstring(Whole, Part, Fringes, _, _, _).

midstring(Whole, Part, Fringes, Before) :-
	midstring(Whole, Part, Fringes, Before, _, _).

midstring(Whole, Part, Fringes, Before, Length) :-
	midstring(Whole, Part, Fringes, Before, Length, _).

midstring(Whole, Part, Fringes, DropFromHead, LengthOfPart, DropFromTail) :-
	var(Whole),
	!,
	string_size(Part, LengthOfPart),
	string_size(Fringes, LengthOfFringes),
	(   integer(DropFromHead) ->
	    DropFromTail is LengthOfFringes-DropFromHead,
	    DropFromTail >= 0,
	    DropFromHead >= 0
	;   integer(DropFromTail) ->
	    DropFromHead is LengthOfFringes-DropFromTail,
	    DropFromHead >= 0,
	    DropFromTail >= 0
	;   var(DropFromHead), var(DropFromTail) ->
	    'Enum'(LengthOfFringes, DropFromTail),
	    DropFromHead is LengthOfFringes-DropFromTail
	),
	'Qinsstr'(Fringes, DropFromHead, Part, Whole, 0).
midstring(Whole, Part, Fringes, DropFromHead, LengthOfPart, DropFromTail) :-
	var(Part),
	!,
	string_size(Whole, LengthOfWhole),	% checks that atom(Whole)
	(   var(Fringes) -> true
	;   string_size(Fringes, LengthOfFringes),
	    LengthOfPart is LengthOfWhole-LengthOfFringes,
	    LengthOfPart >= 0
	),
	'Plus'(DropFromHead, LengthOfPart, DropFromTail, LengthOfWhole),
	'Qsubstr'(Whole, DropFromHead, LengthOfPart, Part, 0),
	'Qoutstr'(Whole, DropFromHead, LengthOfPart, Fringes).
midstring(Whole, Part, Fringes, DropFromHead, LengthOfPart, DropFromTail) :-
	string_size(Part, LengthOfPart),	% checks that atom(Part)
	!,
	string_size(Whole, LengthOfWhole),	% checks that atom(Whole)
	(   var(Fringes) -> true
	;   string_size(Fringes, LengthOfFringes),
	    LengthOfFringes+LengthOfPart =:= LengthOfWhole
	),
	(   nonvar(DropFromHead) ->
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead,
		'Qsubchk'(Whole, DropFromHead, LengthOfPart, Part, 0)
	;   nonvar(DropFromTail) ->
		DropFromHead is LengthOfWhole-LengthOfPart-DropFromTail,
		'Qsubchk'(Whole, DropFromHead, LengthOfPart, Part, 0)
	;   /* otherwise we must search */
		'Enum'(Whole, Part, -1, DropFromHead),
		DropFromTail is LengthOfWhole-LengthOfPart-DropFromHead
	),
	'Qoutstr'(Whole, DropFromHead, LengthOfPart, Fringes).


'Qoutstr'(Whole, DropFromHead, LengthOfPart, Fringes) :-
	nonvar(Fringes),
	!,
	'Qoutchk'(Whole, DropFromHead, LengthOfPart, Fringes, 0).
'Qoutstr'(Whole, DropFromHead, LengthOfPart, Fringes) :-
	'Qoutstr'(Whole, DropFromHead, LengthOfPart, Fringes, 0).


%   'Enum'(String, Pattern, Start, Found)
%   is true when String = (.RHO String) .TAKE Found .DROP Pattern
%   and Found is the smallest offset *greater than* Start for
%   which this is true.

'Enum'(String, Pattern, Start, Found) :-
	'Qindex'(String, Pattern, Start, Trial),
	Trial >= 0,		% Qindex found an occurrence
	(   Found = Trial
	;   'Enum'(String, Pattern, Trial, Found)
	).


%   'Plus'(A, B, C, T)  is true when, for a fixed value T,
%   A, B, and C are non-negative integers and A+B+C = T.
%   If they have to be generated, A is enumerated before
%   B, which is in turn enumerated before C.

'Plus'(A, B, C, T) :-
	(   nonvar(A) ->
		A >= 0,
		(   nonvar(B) ->
			B >= 0, C is T-A-B, C >= 0
		;   nonvar(C) ->
			C >= 0, B is T-A-C, B >= 0
		;   /* otherwise */
			D is T-A, D >= 0, 'Enum'(D, C), B is D-C
		)
	;   nonvar(B) ->
		B >= 0,
		(   nonvar(C) ->
			C >= 0, A is T-B-C, A >= 0
		;   /* otherwise */
			D is T-B, D >= 0, 'Enum'(D, C), A is D-C
		)
	;   nonvar(C) ->
		D is T-C, D >= 0, 'Enum'(D, B), A is D-B
	;   /* otherwise */
		'Enum'(T, D), A is T-D, 'Enum'(D, C), B is D-C
	).


'Enum'(X, X).
'Enum'(U, X) :-
	U > 0, V is U-1,
	'Enum'(V, X).



%   span_left(String, Set, Before, Length, After)
%   succeeds when name(String) &
%   Set is an atom (or a string) or a character (ISO8859 code) or a
%   non-empty list of character codes, or not(X) where X is one of
%   those permitted forms, and String can be decomposed as
%	String = Protos ^ Mesos ^ Eschatos
%	string_size(Protos, Before)
%	string_size(Mesos, Length)
%	string_size(Eschatos, After)
%	no character in Protos belongs to Set
%	every character in Mesos belongs to Set
%	Protos and Mesos are as long as possible
%	and Mesos is non-empty.
%   This predicate is functional in the first two arguments.
%   span_left(String, Set, Before, After) and
%   span_left(String, Set, Before)
%   are identical to span_left/5 except that they lack the parameter or two.
%   Thus to skip leading spaces, one might do
%	span_left(String, " ", Before),
%	substring(String, Trimmed, Before, _, 0).
%   while to extract a single blank-delimited token, one might do
%	span_left(String, " ", Before, Length, After),
%	substring(String, Token, Before, Length, After).

span_left(String, Set, Before) :-
	span_left(String, Set, Before, _, _).

span_left(String, Set, Before, Length) :-
	span_left(String, Set, Before, Length, _).

span_left(String, Set, Before, Length, After) :-
	'QpInit',
	'deposit set'(Set, SetFlag),
	Operation is SetFlag+4,
	'Qspan'(String, Operation, Before, Length, After),
	Length > 0.

%   span_right(String, Set, Before, Length, After),
%   span_right(String, Set,         Length, After), and
%   span_right(String, Set,                 After)
%   are similar to span_left/[3,4,5], except that they scan the string from the
%   right to the left.  So the meaning of span_right/5 is that
%	String = Protos ^ Mesos ^ Eschatos
%	string_size(Protos, Before)
%	string_size(Mesos, Length)
%	string_size(Eschatos, After)
%	no character in Eschatos belongs to Set
%	every character in Mesos belongs to Set
%	Protos and Mesos are as long as possible
%	and Mesos is non-empty.
%   Thus to remove trailing spaces, one might do
%	span_right(String, " ", After),
%	substring(String, Trimmed, 0, _, After)
%   while to trim a string at both ends one would do
%   span_trim(String, Trimmed) :-
%	span_left(String, " ", Before), !,
%	span_right(String, " ", After),
%	substring(String, Trimmed, Before, _, After).
%   span_trim(String, Trimmed) :-
%	substring(String, Trimmed, 0, 0).

span_right(String, Set, After) :-
	span_right(String, Set, _, _, After).

span_right(String, Set, Length, After) :-
	span_right(String, Set, _, Length, After).

span_right(String, Set, Before, Length, After) :-
	'QpInit',
	'deposit set'(Set, SetFlag),
	Operation is SetFlag+2,
	'Qspan'(String, Operation, Before, Length, After),
	Length > 0.


span_trim(String, Trimmed) :-
	span_trim(String, " ", Trimmed).

span_trim(String, Set, Trimmed) :-
	span_trim(String, Set, Before, Length, After),
	substring(String, Trimmed, Before, Length, After).

span_trim(String, Set, Before, Length, After) :-
	'QpInit',
	'deposit set'(Set, SetFlag),
	Operation is 7-SetFlag,
	'Qspan'(String, Operation, Before, Length, After),
	Length > 0.


'deposit set'(not(Set), Flag) :- !,
	nonvar(Set),
	'deposit set'(Set, F),
	Flag is 1-F.
'deposit set'(Set, 0) :-
	(   atom(Set) -> 'QpAtom'(Set, _)
	;   integer(Set) -> 'QpOne'(Set)
	;   'QpChars'(Set)
	).



%   gensym(V)
%   unifies V with a new atom.  The prefix used is '%'.

gensym(V) :-
	gensym('%', V).


%   gensym(Prefix, V)
%   unifies V with a new atom whose name begins with Prefix and ends
%   with a number.  E.g. gensym(a,X), gensym(a,Y), gensym(a,Z) might
%   bind X to a1, Y to a2, Z to a3.  It only succeeds once per call,
%   to get another binding for V you have to call it again.  There's
%   a separate counter for each prefix.

gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	(   retract(gensym_counter(Prefix, M))
	;   M = 0
	),
	N is M+1,
	asserta(gensym_counter(Prefix, N)),
	concat(Prefix, N, V),
	!.



%   cgensym(Prefix, V)
%   binds V to a new atom unless it is already bound.  Thus
%   cgensym(a, fred) would succeed, but cgensym(a, X) would bind
%   X to a new atom, maybe a4.  "c" standard for "conditional".

cgensym(Prefix, V) :-
	nonvar(V), !,
	atomic(V),
	atomic(Prefix).
cgensym(Prefix, V) :-
	gensym(Prefix, V).



%   In Xerox Quintus Prolog, there is a difference between
%   compare_strings(R, S1, S2) and compare(R, S1, S2).
%   compare_strings/3 accepts atoms and strings, but doesn't
%   care which, so that compare_strings(=, 'a', $a$).
%   In other versions of Quintus Prolog, the only difference
%   between compare_strings/3 and compare/3 is that the former
%   reports an error if its last two arguments are not atoms.
%   compare_strings(R, S1, S2) and
%   compare_strings(R, S1, S2, -1) have the same effect,
%   except for the wording of the error message.  A typical
%   padding character for compare_strings/4 is "0' " = 32.

compare_strings(Rel, String1, String2) :-
	(   atom(String1), atom(String2) ->
	    'Qcmpstr'(Rel, String1, 0, -1, -1, String2, 0, -1, -1)
	;   Goal = compare_strings(Rel,String1,String2),
	    should_be(atom, 2, Goal),
	    should_be(atom, 3, Goal)
	).


compare_strings(Rel, String1, String2, Pad) :-
	(   atom(String1), atom(String2), integer(Pad) ->
	    'Qcmpstr'(Rel, String1, 0, -1, Pad, String2, 0, -1, Pad)
	;   Goal = compare_strings(Rel,String1,String2,Pad),
	    should_be(atom, 2, Goal),
	    should_be(atom, 3, Goal),
	    should_be(integer,  4, Goal)
	).


/*  
    These definitions are now replaced by direct calls to the corresponding
    built-in predicates, but the old source is kept as a comment in case
    anyone is interested.
*/

name1(Atomic, Chars) :- 
	name(Atomic, Chars).

atom_chars1(Atom, Chars) :-
	atom_chars(Atom, Chars).

number_chars1(Number, Chars) :- 
	number_chars(Number, Chars).

% name1(Atomic, Chars) :-
% 	'QpInit',
% 	(   atom(Atomic) ->
% 		'QpAtom'(Atomic, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   integer(Atomic) ->
% 		'QpInt'(Atomic, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   float(Atomic) ->
% 		'QpFlt'(Atomic, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   var(Atomic), nonvar(Chars), 'QpChars'(Chars) ->
% 		'QgInit'(Length),
% 		'QgNum'(0, Length, Int, Flt, Flag),
% 		(   Flag =:= 0 -> Atomic = Int
% 		;   Flag =:= 1 -> Atomic = Flt
% 		;   'QgAtom'(0, Length, Atomic, 0)
% 		)
% 	;   /* otherwise */
% 		'name error'(name1, atomic, Atomic, Chars)
% 	).
% 
% 
% atom_chars1(Atom, Chars) :-
% 	'QpInit',
% 	(   atom(Atom) ->
% 		'QpAtom'(Atom, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   var(Atom), nonvar(Chars), 'QpChars'(Chars) ->
% 		'QgInit'(Length),
% 		'QgAtom'(0, Length, Atom, 0)
% 	;   /* otherwise */
% 		'name error'(atom_chars1, atom, Atom, Chars)
% 	).
% 
% 
% number_chars1(Number, Chars) :-
% 	'QpInit',	
% 	(   integer(Number) ->
% 		'QpInt'(Number, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   float(Number) ->
% 		'QpFlt'(Number, Length),
% 		'QgChars'(0, Length, Chars)
% 	;   var(Number), nonvar(Chars), 'QpChars'(Chars) ->
% 		'QgInit'(Length),
% 		'QgNum'(0, Length, Int, Flt, Flag),
% 		(   Flag =:= 0 -> Number = Int
% 		;   Flag =:= 1 -> Number = Flt
% 		)
% 	;   /* otherwise */
% 		'name error'(number_chars1, number, Number, Chars)
% 	).
% 
% /*
% string_chars(String, Chars) :-
% 	format(user_error,
% 	    '~N! No strings in this version; use atoms.~n! Goal: ~q~n',
% 	    [string_chars(String,Chars)]),
% 	fail.
% */
% 
% /*  'name error'(PredicateName, TypeName, Thing, Chars)
%     is called when one of the three name/2-style predicates is unhappy
%     with its arguments.   If Thing is a variable, Chars should be a
%     'proper chars'.   The predicate 'QpChars'/1 will fail if this is
%     not the case.  Otherwise, Thing should satisfy the type test
%     named by TypeName.  TypeName/1 should be a Prolog predicate!
% 
%     This predicate should always raise an exception.
% */
% 'name error'(PredicateName, TypeName, Thing, Chars) :-
% 	Goal =.. [PredicateName,Thing,Chars],
% 	(   nonvar(Thing) ->
% 	        must_be(TypeName, 1, Goal)
% 	;   'proper chars'(Chars, _, _) ->
% 		/* can't be (Chars,_,1) or we'd not have been called */
% 	        raise_exception(instantiation_error(Goal,2))
% 	;   /* var Goal, not proper chars Thing */
% 	        must_be(chars, 2, Goal)
% 	).


'QgChars'(Drop, Take, Chars) :-
	Left is Take-Drop,
	(   Left >= 4 ->
		'QgFour'(Drop, C1, C2, C3, C4, 0),
		Chars = [C1,C2,C3,C4|Rest],
		Next is Drop+4,
		'QgChars'(Next, Take, Rest)
	;   Left >= 1 ->
		'QgOne'(Drop, C1, 0),
		Chars = [C1|Rest],
		Next is Drop+1,
		'QgChars'(Next, Take, Rest)
	;   Left =:= 0 ->
		Chars = []
	).


'QpChars'([C1,C2,C3,C4|Chars]) :- !,
	'QpFour'(C1, C2, C3, C4),
	'QpChars'(Chars).
'QpChars'([C1|Chars]) :- !,
	'QpOne'(C1),
	'QpChars'(Chars).
'QpChars'([]).



foreign_file(library(system(libpl)),
	['Qstrlen','Qstrchr','Qsubchk','Qsubstr','Qsystem','Qcmpstr',
	 'Qatomch','Qchatom','Qoutchk','Qoutstr','Qindex', 'Qspan',
	 'QpAtom', 'QpInit', 'QpFour', 'QpOne', 'QpInt', 'QpFlt',
	 'QgAtom', 'QgInit', 'QgFour', 'QgOne', 'QgNum', 'QpPart',
	 'Qinsstr'
	]).

foreign('Qsystem',	'Qsystem'([-integer])).
foreign('Qstrlen',	string_size(+atom,[-integer])).
foreign('Qstrchr',	'Qstrchr'(+integer,+atom,[-integer])).
foreign('Qsubchk',	'Qsubchk'(+atom,+integer,+integer,+atom,[-integer])).
foreign('Qsubstr',	'Qsubstr'(+atom,+integer,+integer,-atom,[-integer])).
foreign('Qoutchk',	'Qoutchk'(+atom,+integer,+integer,+atom,[-integer])).
foreign('Qoutstr',	'Qoutstr'(+atom,+integer,+integer,-atom,[-integer])).
foreign('Qinsstr',	'Qinsstr'(+atom,+integer,+atom,-atom,[-integer])).
foreign('Qatomch',	'Qatomch'(+atom,[-integer])).
foreign('Qchatom',	'Qchatom'(+integer,[-atom])).
foreign('Qindex',	'Qindex'( +atom,+atom,+integer,[-integer])).
foreign('Qcmpstr',	'Qcmpstr'([-atom],
				   +atom,+integer,+integer,+integer,
				   +atom,+integer,+integer,+integer)).
foreign('Qspan',	'Qspan'(+atom,+integer,-integer,[-integer],-integer)).
foreign('QpInit',	'QpInit').
foreign('QpAtom',	'QpAtom'(+atom,[-integer])).
foreign('QpPart',	'QpPart'(+atom,+integer,+integer,[-integer])).
foreign('QpInt',	'QpInt'(+integer,[-integer])).
foreign('QpFlt',	'QpFlt'(+float,[-integer])).
foreign('QpFour',	'QpFour'(+integer,+integer,+integer,+integer)).
foreign('QpOne',	'QpOne'(+integer)).
foreign('QgInit',	'QgInit'([-integer])).
foreign('QgAtom',	'QgAtom'(+integer,+integer,-atom,[-integer])).
foreign('QgNum',	'QgNum'(+integer,+integer,-integer,-float,
				  [-integer])).
foreign('QgFour',	'QgFour'(+integer,-integer,-integer,
				  -integer,-integer,[-integer])).
foreign('QgOne',	'QgOne'(+integer,-integer,[-integer])).

/*
:- load_foreign_executable(library(system(libpl))),
   abolish(foreign_file, 2),
   abolish(foreign, 2).
*/

