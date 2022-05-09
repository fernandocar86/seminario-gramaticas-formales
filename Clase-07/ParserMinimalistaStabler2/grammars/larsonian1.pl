% file: larsonian1.pl
% author: John Hale
% created: Thu Jul 18 11:46:02 EDT 2002
% updated: Sun Jul 21 15:40:05 EDT 2002
% updated 31 Jul 02:  small modifications for a couple of examples -- EPS
%

[]::[='T','C'].                  % regular empty complementizer
[]::[='T',+wh,'Rel'].      % wh-hoisting complementizer
[that]::[='T','Ce'].           % embedding complementizer

[the]::[='Rel','D',-case].     % relative determiner
[the]::[='Num','D',-case].     % ordinary determiner
[a]::[='Num','D',-case].
[my]::[='Num','D',-case].      % genitive determiner
[one]::[='Num','D',-case].     % numerical determiner

% number
[]::[='N','Num'].              ['-s']::['N'==>,'Num'].

% proper noun
['John']::['D',-case]. ['Chris']::['D',-case].
['Dick']::['D',-case]. ['David']::['D',-case]. ['Penny']::['D',-case].
['Sally']::['D',-case]. ['Paul']::['D',-case]. ['Stephen']::['D',-case].
['Mary']::['D',-case]. ['Pat']::['D',-case]. ['Sue']::['D',-case].
['Joe']::['D',-case]. ['Patrick']::['D',-case]. ['Jim']::['D',-case].
['Jenny']::['D',-case]. ['Clare']::['D',-case]. ['Ann']::['D',-case].

% for a simple example:
['Mommy']::['D',-case].   ['cookies']::['D',-case].

% indefinites, nominalization
[eggs] :: ['D',-case]. [apples] :: ['D',-case]. [lies] :: ['D',-case].
[reading] :: ['D',-case].

% pronouns
[he]::['D',-case]. [they]::['D',-case]. ['I']::['D',-case]. [it]::['D',-case].
[us]::['D',-case].

% possessive
% should be the complement of an external determiner
% in order to be pied-piped during wh-promotion
['s']::[='Num',='Num','N'].

[which]::[='Num',+f,'D',-case,-wh].     % `promoting' wh-words
  [who]::[='Num',+f,'D',-case,-wh].     % semantic selection features (?)

% common nouns
[story]::['N'].    [story]::['N',-f].         % w and w/o promotion feature
[boy]::['N'].      [boy]::['N',-f].
[girl]::['N'].     [girl]::['N',-f].
[man]::['N'].      [man]::['N',-f].
[woman]::['N'].    [woman]::['N',-f].
[dog]::['N'].      [dog]::['N',-f].
[cat]::['N'].      [cat]::['N',-f].
[book]::['N'].     [book]::['N',-f].
[house]::['N'].    [house]::['N',-f].
[town]::['N'].     [town]::['N',-f].
[trick]::['N'].    [trick]::['N',-f].
[treat]::['N'].    [treat]::['N',-f].
[lie]::['N'].      [lie]::['N',-f].
[sweet]::['N'].    [sweet]::['N',-f].
[present]::['N'].  [present]::['N',-f].
[ticket]::['N'].   [ticket]::['N',-f].
[letter]::['N'].   [letter]::['N',-f].
[picture]::['N'].  [picture]::['N',-f].
[answer]::['N'].   [answer]::['N',-f].
[accident]::['N']. [accident]::['N',-f].
[box]::['N'].      [box]::['N',-f].
[apple]::['N'].    [apple]::['N',-f].
[food]::['N'].     [food]::['N',-f].
[cake]::['N'].     [cake]::['N',-f].
[ship]::['N'].     [ship]::['N',-f].
[car]::['N'].      [car]::['N',-f].
[sailor]::['N'].   [sailor]::['N',-f].
[uncle]::['N'].    [uncle]::['N',-f].
[mother]::['N'].   [mother]::['N',-f].
[father]::['N'].   [father]::['N',-f].
[brother]::['N'].  [brother]::['N',-f].
[friend]::['N'].   [friend]::['N',-f].
[bill]::['N'].     [bill]::['N',-f].
[leg]::['N'].      [leg]::['N',-f].
[clothe]::['N'].   [clothe]::['N',-f].

% ....with complements
[fact]::[='Ce','N'].

% preposition is a case assigner (Haegeman p193)
[to]::[='D',+case,'Pto']. [on]::[='D',+case,'Pon']. [for]::[='D',+case,'Pfor'].
[with]::[='D',+case,'Pwith']. [in]::[='D',+case,'Pin'].
[]::[='D',+case,'Pto',-dat].	% P is empty in dative

% little p
[]::[=>'Pto',p_to]. []::[=>'Pin',p_in].  []::[=>'Pwith',p_with].
[]::[=>'Pon',p_on]. []::[=>'Pfor',p_for].

% ditransitive verbs
[tell]::[=p_to,='D',+case,'V']. [give]::[=p_to,='D',+case,'V']. [show]::[=p_to,='D',+case,'V'].
[explain]::[=p_to,='D',+case,'V']. [teach]::[=p_to,='D',+case,'V']. [sell]::[=p_to,='D',+case,'V'].

% intransitive verbs
[matter]::['V']. [wait]::['V']. [rule]::['V']. % yeah...yeah...like Slayer!

% transitive verbs
[tell]::[='D',+case,'V']. [love]::[='D',+case,'V']. [hate]::[='D',+case,'V']. [write]::[='D',+case,'V'].
[sell]::[='D',+case,'V']. [get]::[='D',+case,'V']. [leave]::[='D',+case,'V']. [like]::[='D',+case,'V'].
[buy]::[='D',+case,'V']. [bring]::[='D',+case,'V']. [take]::[='D',+case,'V']. [send]::[='D',+case,'V'].
[have]::[='D',+case,'V']. [surprise]::[='D',+case,'V'].

[pay]::[=p_for,'V'].		% pay...for services
[pay]::[='D',+case,'V'].		% pay...the piper

[come]::[='A','V'].		% came late

% CP-taking verbs
[know]::[='Ce','V']. [forget]::[='Ce','V']. [remember]::[='Ce','V'].

% auxilliary verbs
[will]::[='Have','Modal'].   [will]::[='Be','Modal'].    [will]::[='V','Modal'].
[have]::[='Been','Have'].    [have]::[=ven,'Have'].
[be]::[=ving,'Be'].          [been]::[=ving,'Been'].

% little v gets the subject
[]::[=>'V',='D',v].
[]::[=>'V',+dat,='D',v].	% v moves dative argument, gets SUBJ
['-en']::[=>'V',='D',ven].
['-ing']::[=>'V',='D',ving].

% tense
['-s']::[v==>,+case,'T']. ['-ed']::[=>v,+case,'T']. []::[=>v,+case,'T'].
['-s']::[=>'Modal',+case,'T']. ['-s']::[=>'Have',+case,'T']. ['-s']::[=>'Be',+case,'T'].
['-ed']::[=>'Modal',+case,'T']. ['-ed']::[=>'Have',+case,'T']. ['-ed']::[=>'Be',+case,'T'].
[]::[=>'Modal',+case,'T']. []::[=>'Have',+case,'T']. []::[=>'Be',+case,'T'].

% ignore do-support and negation for now
['doesnt']::[=v,+case,'T'].

% predicative/copular be
[be]::[=a,'Be'].

% little a
[]::[='A',='D',a].
[]::[='D',+case,='D',a].   % [ADJ the bomb] [ADJ a dear]

% adjectives
[young]::['A']. [poor]::['A']. [clever]::['A']. [gentle]::['A'].
[kind]::['A']. [proud]::['A']. [lost]::['A']. [cheap]::['A'].
[interesting]::['A']. [sad]::['A']. [late]::['A']. [ill]::['A'].
[important]::['A']. [angry]::['A']. [pretty]::['A']. [honest]::['A'].
[right]::['A']. [strange]::['A']. [old]::['A']. [long]::['A'].

% for testing only: [rright]::['RA'].  [sso]::[rdeg].  ['N']<<['RA'].  ['RA']<<[rdeg].

% optional intensifiers
[so]::[deg]. [very]::[deg]. [always]::[deg]. [too]::[deg].

% which can left-adjoin to adjectives
[deg]>>['A'].

% adjectives can also left-adjoin onto nouns
['A']>>['N'].

% temporal modifiers
[yesterday]::[tmp]. [today]::[tmp].

% which right adjoin to verbs
['V']<<[tmp].

% oblique modifiers can right adjoin to VPs
['V']<<[p_in]. ['V']<<[p_with]. ['V']<<[p_on]. ['V']<<[p_for].

startCategory('C').
