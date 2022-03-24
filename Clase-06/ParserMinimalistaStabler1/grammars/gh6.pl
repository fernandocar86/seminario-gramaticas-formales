%   File   : gh6.pl
%   Author : E Stabler
%   Updated: Feb 2002

% complementizers
[]::[='T','C'].         []::[=>'T','C'].     []::[=>'T',+wh,'C'].  []::[='T',+wh,'C'].
[that]::[='T','Ce'].    []::[='T','Ce'].        % embedded clause
[]::[='T',+wh,'Cwh'].   []::[=>'T',+wh,'Cwh'].  % embedded wh-clause

% finite tense
['-s']::[v==>,+k,'T'].	% for affix hopping
['-s']::[=>'Modal',+k,'T'].  ['-s']::[=>'Have',+k,'T'].   ['-s']::[=>'Be',+k,'T'].     ['-s']::[=v,+k,'T'].

% simple nouns
[king]::['N'].               [pie]::['N'].           [human]::['N'].      [car]::['N'].
[coffee]::['N'].             [shirt]::['N'].         [language]::['N'].

% determiners
[the]::[='Num','D',-k].        [every]::[='Num','D',-k].    [a]::[='Num','D',-k].   [an]::[='Num','D',-k].
[some]::[='Num','D',-k].       [some]::['D',-k].

% number marking (singular, plural)
[]::[='N','Num'].              ['-s']::['N'==>,'Num'].

% names as lexical DPs
['Titus']::['D',-k].         ['Lavinia']::['D',-k].      ['Tamara']::['D',-k].      ['Sunday']::['D',-k].

% pronouns as lexical determiners
[she]::['D',-k].  [he]::['D',-k].   [it]::['D',-k].  ['I']::['D',-k]. [you]::['D',-k]. [they]::['D',-k].  % nom
[her]::['D',-k].  [him]::['D',-k].                   [me]::['D',-k].  [us]::['D',-k].  [them]::['D',-k].  % acc
[her]::[='Num','D',-k]. [his]::[='Num','D',-k].  [its]::[='Num','D',-k].  % gen

% wh determiners
[which]::[='Num','D',-k,-wh].  [which]::['D',-k,-wh].
[what]::[='Num','D',-k,-wh].   [what]::['D',-k,-wh].

% auxiliary verbs
[will]::[='Have','Modal'].   [will]::[='Be','Modal'].    [will]::[=v,'Modal'].
[have]::[='Been','Have'].    [have]::[=ven,'Have'].
[be]::[=ving,'Be'].          [been]::[=ving,'Been'].

% little v
[]::[=>'V',='D',v].     ['-en']::[=>'V',='D',ven].  ['-ing']::[=>'V',='D',ving].
['-en']::[=>'V',ven].       ['-ing']::[=>'V',ving].

% DP-selecting (transitive) verbs - select an object, and take a subject too (via v)
[praise]::[='D',+k,'V'].    [sing]::[='D',+k,'V'].  [eat]::[='D',+k,'V'].  [have]::[='D',+k,'V'].

% intransitive verbs - select no object, but take a subject
[laugh]::['V'].          [sing]::['V'].       [charge]::['V'].    [eat]::['V'].

% CP-selecting verbs
[know]::[='Ce','V'].    [know]::[='Cwh','V'].    [know]::[='D',+k,'V'].   [know]::['V'].
[doubt]::[='Ce','V'].   [doubt]::[='Cwh','V'].   [doubt]::[='D',+k,'V'].  [doubt]::['V'].
[think]::[='Ce','V'].                                                     [think]::['V'].
                        [wonder]::[='Cwh','V'].                           [wonder]::['V'].

% CP-selecting nouns
[claim]::[='Ce','N'].         [proposition]::[='Ce','N'].

% raising verbs - select only propositional complement, no object or subject
[seem]::[='T',v].

% infinitival tense 
[to]::[=v,'T'].     [to]::[='Have','T'].        [to]::[='Be','T'].   % nb does not select modals

% little a
[]::[=>'A',='D',a].

% simple adjectives
[black]::['A'].  [white]::['A'].      [human]::['A'].  [mortal]::['A'].
[happy]::['A'].  [unhappy]::['A'].

% verbs with AP complements: predicative be, seem
[be]::[=a,'Be'].    [seem]::[=a,v].

% adjectives with complements
[proud]::[=p,'A'].   [proud]::['A'].   [proud]::[='T',a].

% little p (no subject?)
[]::[=>'P',p].

% prepositions with no subject
[of]::[='D',+k,'P'].        [about]::[='D',+k,'P'].        [on]::[='D',+k,'P'].

% verbs with AP,TP complements: small clause selectors as raising to object
[prefer]::[=a,+k,'V'].      [prefer]::[='T',+k,'V'].
[consider]::[=a,+k,'V'].    [consider]::[='T',+k,'V'].

% nouns with PP complements
[student]::[=p,'N'].        [student]::['N'].
[citizen]::[=p,'N'].        [citizen]::['N'].

% more verbs with PP complements
[be]::[=p,v].    [seem]::[=p,v].
[]::[=>'P',='D',p].
[up]::[='D',+k,'P'].
[creek]::['N'].

% control verbs
[try]::[='T','V'].
[want]::[='T','V'].      [want]::[='T',+k,'V'].

% verbs with causative alternation: using little v that does not select subject
[break]::[='D',+k,'V']. 
% one idea, but this little v can cause trouble, esp. without island constraints
%[break]::[='D','V'].  []::[=>'V',v]. 
% better:
[break]::[='D',v]. 

% simple idea about PRO that does not work: []::['D'].
% second idea: "null case" feature k0
[]::['D',-k0].
[to]::[=v,+k0,'T'].     [to]::[='Have',+k0,'T'].        [to]::[='Be',+k0,'T'].   % nb does not select modals

% modifiers
['N']<<[p].    [v]<<[p].      [v]<<['Adv'].  ['D',-k]<<['D',-k].   ['N']<<['A']. % for testing only
['A']>>['N'].  ['Adv']>>[v].  [deg]>>['A'].  ['Adv']>>['P'].  
 [emph]>>['D',-k].  [qu]>>['Num'].

[completely]::['Adv'].  [happily]::['Adv'].  [very]::[deg].   [only]::[emph].   [1]::[qu].

startCategory('C').


% coordinators
[and]::[coord].  [or]::[coord].  [but]::[coord].  [yet]::[coord].  [nor]::[coord].
