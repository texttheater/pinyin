:- module(pinyin,[
  word//2,
  num_dia/2]).

/** <module> Parsing and generation of Hanyu Pinyin

This module implements a grammar that parses and generates words written in
Hanyu Pinyin, the standard romanization system for Mandarin Chinese. It also
provides a utility to convert whole texts between diacritics and numbers for
writing tones.

*/

:- use_module(casing, [
    casing/3]).

:- set_prolog_flag(double_quotes, codes). % SWI-7 ready

% TODO support v for ü?
% TODO support tone number 0/5?
% TODO What about "Harbin"?

%%	num_dia(?Num, ?Dia)
%
%	Converts between Pinyin with diacritics and numbers as tone marks.
%	Num and Dia are code lists.
%
%	Maximal substrings of Pinyin letters, the numbers 1-4 as well as the
%	characters =|'|= and =|-|= are converted if they can be parsed as
%	lower-case, capitalized or all-caps Pinyin words in the input format,
%	everything else is left alone. Case is preserved.
%
%	The assumed input format is diacritics if Num is variable, numbers
%	otherwise.
%
%	Example usage:
%
%	==
%	?- set_prolog_flag(double_quotes, codes).
%	true.
%
%	?- num_dia("Wo3 xian4zai4 dui4 jing1ju4 hen3 gan3 xing4qu4.",
%	|    Codes), atom_codes(Atom, Codes).
%	Codes = [87, 466, 32, 120, 105, 224, 110, 122, 224|...],
%	Atom = 'Wǒ xiànzài duì jīngjù hěn gǎn xìngqù.'.
%
%	?- num_dia(Codes, "Nǐ ne?"), atom_codes(Atom, Codes).
%	Codes = [78, 105, 51, 32, 110, 101, 63],
%	Atom = 'Ni3 ne?'.
%	==
num_dia(Num, Dia) :-
  var(Num),
  !,
  convert(dia, num, Dia, Num).
num_dia(Num, Dia) :-
  convert(num, dia, Num, Dia).

% PINYIN WORD GRAMMAR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	word(?Morphs, ?ND)//
%
%	A DCG that parses or generates a single word written in Hanyu Pinyin.
%
%       Morphs is a list of "morphs" (not in the strictest linguistic sense)
%	that make up the word. They take one of three forms:
%
%	    1. =|Initial-Final-Tone|= where =Initial= and =Final= are atoms.
%	       The final takes has the "underlying" form, which may be
%	       different from the written form. =Tone= is one of the integers
%	       from =|0|= to =|4|=.
%	    2. =r= (for the erhuayin suffix)
%	    3. =|-|= (for word-internal hyphens)
%	
%	ND is either =num= or =dia=, depending on how tones are represented
%	(numbers or diacritics).
%
%	The grammar implements the following tricky aspects of Hanyu Pinyin:
%
%	    * placement of w/y before i/u/ü as required
%	    * abbreviation of triphthongs into two vowel letters as required
%	    * change of ü to u after j/q/x/y
%	    * placement of tone marks
%	    * placement of apostrophes between syllables as required
%	    * the possibility of word-internal hyphens
%	    * the possibility of erhuayin, placing r at the end of a word
%
%	The following aspects are currently not supported:
%
%	    * Case. This DCG only handles lower-case letters.
%	    * The spelling variant where ü is written as v.
%	    * The spelling variant where the neutral tone is written as 0 or 5.
%	    * Phonological restrictions. Not every initial, final and tone
%	      combine into a syllable in Mandarin. This grammar, however, will
%	      happily parse and generate any combination. It only enforces that
%	      the erhuayin suffix not appear after the e final, and that j/x/q
%	      not be followed by an u sound, because either would make the
%	      grammar ambiguous.
%	    * Dropping apostrophes when using numbers instead of tone marks.
%
%	Example usage:
%
%	==
%	?- set_prolog_flag(double_quotes, codes).
%	true.
%
%	?- phrase(word([n-ü-3, ''-er-2], ND), Codes), atom_codes(Atom, Codes).
%	ND = dia,
%	Codes = [110, 474, 39, 233, 114],
%	Atom = 'nǚ\'ér' ;
%	ND = num,
%	Codes = [110, 252, 51, 39, 101, 114, 50],
%	Atom = 'nü3\'er2' ;
%	false.
%
%	?- phrase(word(Morphs, ND), "yìjué").
%	Morphs = [''-i-4, j-üe-2],
%	ND = dia ;
%	false.
%	==
word([Morph], ND) -->
  syll1(ND, Morph, _).
% erhuayin
word([Morph, r], ND) -->
  syll1(ND, Morph, _),
  { Morph \= _-e-_
  },
  "r".
word([Morph|Morphs], ND) -->
  syll1(ND, Morph, _),
  sylls(ND, Morphs).
word([''-Final-Tone], ND) -->
  syllabic_nasal(ND, Final, Tone).

% initial syllable
syll1(ND, ''-Final-Tone, RequiresApostrophe) -->
  bare_final(ND, Final, Tone, RequiresApostrophe).
syll1(ND, Initial-Final-Tone, no) -->
  nonempty_initial(Initial),
  { requires_modification(Initial, RequiresModification)
  },
  nonbare_final(ND, RequiresModification, Final, Tone).

% sequence of non-initial syllables
sylls(ND, [Morph]) -->
  syll(ND, Morph).
% erhuayin
sylls(ND, [Morph, r]) -->
  syll(ND, Morph),
  { Morph \= _-e-_
  },
  "r".
sylls(ND, [Morph|Morphs]) -->
  syll(ND, Morph),
  sylls(ND, Morphs).

% non-initial syllable
syll(ND, [-|Morph]) -->
  "-",
  syll1(ND, Morph).
syll(ND, Initial-Final-Tone) -->
  syll1(ND, Initial-Final-Tone, no).
syll(ND, Initial-Final-Tone) -->
  "'",
  syll1(ND, Initial-Final-Tone, yes).

% GRAPHEMIC TRANSFORMATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	render_final_bare(+FinalUnderlying, -FinalSurface)
%	Converts a final from underlying to written form for an empty initial.
%
%	Introduce y resp. w before [iuü]. Also delete [iu] if another vowel
%	follows.
render_final_bare([Glide, Vowel|Rest],[Substitute, Vowel|Rest]) :-
  member(Glide, "iu"),
  vowel_letter(Vowel),
  !,
  glide_substitute([Glide], [Substitute]).
render_final_bare([Glide|Rest], [Substitute, ModifiedGlide|Rest]) :-
  [Glide] = "ü",
  !,
  [Substitute, ModifiedGlide] = "yu".
render_final_bare([Glide|Rest], [Substitute, Glide|Rest]) :-
  member(Glide, "iuü"),
  !,
  glide_substitute([Glide], [Substitute]).
% Otherwise, written form is identical to underlying.
render_final_bare(Final, Final).

%%	render_final_nonbare(+JXQ, +FinalUnderlying, -FinalSurface)
%	Converts a final from underlying to written form for a nonempty
%	initial.
%
%	Three finals drop a vowel in spelling after a non-empty initial.
%	If JXQ==yes, ü changes to u.
render_final_nonbare(_, "uei", "ui") :-
  !.
render_final_nonbare(_, "iou", "iu") :-
  !.
render_final_nonbare(_, "uen", "un") :-
  !.
% After j, x, q, render ü as u:
render_final_nonbare(yes, [Glide|Rest], [ModifiedGlide|Rest]) :-
  [Glide] = "ü",
  !,
  [ModifiedGlide] = "u".
% Disallow u after j, x, q:
render_final_nonbare(yes, [Glide|_], _) :-
  [Glide] = "u",
  !,
  fail.
render_final_nonbare(_, Final, Final).

%%	tonalize(+RenderedFinal, +Tone, ?ND, -Tonalized)
%	Adds a tone diacritic or number to a rendered final.
tonalize(Final, Tone, dia, Tonalized) :-
  add_tone_mark(Final, Tone, Tonalized).
tonalize(Final, 0, num, Final) :-
  !.
tonalize(Final, Tone, num, Tonalized) :-
  atom_codes(Tone, ToneCodes),
  append(Final, ToneCodes, Tonalized).

% add tone mark to a final
add_tone_mark("m", Tone, [Tonal]) :-
  !, 
  tonal(Tone, "m", [Tonal]). % no vowel - place mark on m
add_tone_mark("n", Tone, [Tonal]) :-
  !, 
  tonal(Tone, "n", [Tonal]). % no vowel - place mark on n
add_tone_mark("ng", Tone, [Tonal|"g"]) :-
  !, 
  tonal(Tone, "n", [Tonal]). % no vowel - place mark on n
add_tone_mark([First|Rest], Tone, [Tonal|Rest]) :-
  member(First, "aeo"),  % these three greedily attract the tone mark
  !, 
  tonal(Tone, [First], [Tonal]).
add_tone_mark([First, Vowel|Rest], Tone, [First|Tonal]) :-
  vowel_letter(Vowel),  % otherwise,  do not yet place the mark if another vowel follows 
  !, 
  add_tone_mark([Vowel|Rest], Tone, Tonal).
add_tone_mark([First|Rest], Tone, [Tonal|Rest]) :-
  vowel_letter(First),  % last vowel - last chance to place the mark
  !, 
  tonal(Tone, [First], [Tonal]).

% GRAPH/PHONE PROPERTIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Indicates whether a final must be preceded by an apostrophe if it is bare (no
% initial preceding) and non-initial.
requires_apostrophe([First|_], yes) :-
  member(First, "aeo"),
  !.
requires_apostrophe(_, no).

% Indicates whether ü changes to u after the given initial.
requires_modification(j, yes) :-
  !.
requires_modification(x, yes) :-
  !.
requires_modification(q, yes) :-
  !.
requires_modification(_, no).

vowel_letter(X) :-
  member(X, "aeiouü").

glide_substitute("i", "y").
glide_substitute("u", "w").
glide_substitute("ü", "y").

% map between letters with and without tone marks
% TODO m only available with 0 and 2 in Unicode, are others not needed for
% Mandarin? Otherwise we have to use combining characters and modify
% add_tone_mark/3.
tonal(0, "a", "a").
tonal(0, "e", "e").
tonal(0, "i", "i").
tonal(0, "o", "o").
tonal(0, "u", "u").
tonal(0, "ü", "ü").
tonal(0, "m", "m").
tonal(0, "n", "n").
tonal(1, "a", "ā").
tonal(1, "e", "ē").
tonal(1, "i", "ī").
tonal(1, "o", "ō").
tonal(1, "u", "ū").
tonal(1, "ü", "ǖ").
tonal(1, "n", "ñ").
tonal(2, "a", "á").
tonal(2, "e", "é").
tonal(2, "i", "í").
tonal(2, "o", "ó").
tonal(2, "u", "ú").
tonal(2, "ü", "ǘ").
tonal(2, "m", "ḿ").
tonal(2, "n", "ń").
tonal(3, "a", "ǎ").
tonal(3, "e", "ě").
tonal(3, "i", "ǐ").
tonal(3, "o", "ǒ").
tonal(3, "u", "ǔ").
tonal(3, "ü", "ǚ").
tonal(3, "n", "ň").
tonal(4, "a", "à").
tonal(4, "e", "è").
tonal(4, "i", "ì").
tonal(4, "o", "ò").
tonal(4, "u", "ù").
tonal(4, "ü", "ǜ").
tonal(4, "n", "ǹ").

% Inventory of tones.
tone(0).
tone(1).
tone(2).
tone(3).
tone(4).

% TERM EXPANSION RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generate terminal DCG rules for initials.
term_expansion(nonempty_initial(Initial),
    (nonempty_initial(InitialAtom) --> Initial)) :-
  atom_codes(InitialAtom, Initial).
% Generate terminal DCG rules for each final, depending on the final, whether
% it follows a nonempty initial ("nonbare"), which tone it carries and how
% tones are written (diacritic or number).
term_expansion(final(Final), Rules) :-
  findall(
      ( bare_final(ND, FinalAtom, Tone, RequiresApostrophe) --> String ),
      ( render_final_bare(Final, Rendered),
        requires_apostrophe(Final, RequiresApostrophe),
        member(ND, [dia, num]),
        tone(Tone),
        tonalize(Rendered, Tone, ND, String),
        atom_codes(FinalAtom, Final)
      ),
      BareRules),
  findall(
      ( nonbare_final(ND, JXQ, FinalAtom, Tone) --> String ),
      ( member(JXQ, [yes, no]),
        render_final_nonbare(JXQ, Final, Rendered),
        member(ND, [dia, num]),
        tone(Tone),
        tonalize(Rendered, Tone, ND, String),
        atom_codes(FinalAtom, Final)
      ),
      NonbareRules),
  append(BareRules, NonbareRules, Rules0),
  % This translation should happen automatically, but doesn't:
  maplist(dcg_translate_rule, Rules0, Rules).
% Generate terminal DCG rules for syllabic nasals.
term_expansion(syllabic_nasal(Final), Rules) :-
  findall(
      ( syllabic_nasal(ND, FinalAtom, Tone) --> String ),
      ( member(ND, [dia, num]),
        tone(Tone),
        tonalize(Final, Tone, ND, String),
        atom_codes(FinalAtom, Final)
      ),
      Rules0),
  maplist(dcg_translate_rule, Rules0, Rules).

% PHONE INVENTORIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are translated to DCG rules so are not available in this form in the
% compiled program.

:- discontiguous bare_final//4.
:- discontiguous nonbare_final//4.

% Inventory of initials.
nonempty_initial("b").
nonempty_initial("p").
nonempty_initial("m").
nonempty_initial("f").
nonempty_initial("d").
nonempty_initial("t").
nonempty_initial("n").
nonempty_initial("l").
nonempty_initial("g").
nonempty_initial("k").
nonempty_initial("h").
nonempty_initial("j").
nonempty_initial("q").
nonempty_initial("x").
nonempty_initial("zh").
nonempty_initial("ch").
nonempty_initial("sh").
nonempty_initial("r").
nonempty_initial("z").
nonempty_initial("c").
nonempty_initial("s").

% Inventory of finals in canonical form. Actual written form depends on
% initial.
final("i").
final("u").
final("ü").
final("a").
final("ia").
final("ua").
final("o").
final("uo").
final("e").
final("ie").
final("üe").
final("er").
final("ai").
final("uai").
final("ei").
final("uei").
final("ao").
final("iao").
final("ou").
final("iou").
final("an").
final("ian").
final("uan").
final("üan").
final("en").
final("in").
final("uen").
final("ün").
final("ang").
final("iang").
final("uang").
final("eng").
final("ing").
final("ueng").
final("ong").
final("iong").

% Inventory of syllabic nasals.
syllabic_nasal("m").
syllabic_nasal("n").
syllabic_nasal("ng").

% CONVERSION BETWEEN NUMBERS AND DIACRITICS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert(InputFormat, OutputFormat, Input, Output) :-
  phrase(l(InputSegments), Input),
  maplist(convert_segment(InputFormat, OutputFormat), InputSegments,
      OutputSegments),
  append(OutputSegments, Output).

convert_segment(_, _, o(C), [C]) :-
  !.
convert_segment(InputFormat, OutputFormat, w(InputWord), OutputWord) :-
  casing(InputWord, UncasedInputWord, CasePattern),
  phrase(word(Morphs, InputFormat), UncasedInputWord),
  phrase(word(Morphs, OutputFormat), UncasedOutputWord),
  casing(OutputWord, UncasedOutputWord, CasePattern),
  !.
% word does not conform to rules, we leave it alone
convert_segment(_, _, w(Word), Word).

% TEXT PARSING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A mini-DCG that parses a string into a list of words and intervening non-word
% characters.

% string starting with word
l([w(W)|L]) -->
  w(W),
  !,
  l(L).
% string starting with non-word character
l([o(C)|L]) -->
  [C],
  !,
  l(L).
% empty string
l([]) -->
  "".

% word with more than one character
w([C|W]) -->
  c(C),
  w(W),
  !.
% word with one character
w([C]) -->
  c(C).

% word character
c(C) -->
  [C],
  { member(C, "AÃÁǍÀBCDEĒÉĚÈFGHIĪÍǏÌJKLMNOŌÓǑÒPQRSTUŪÚǓÙÜǕǗǙǛWXYZaãáǎàbcdeēéěèfghiīíǐìjklmḿnñńňǹoōóǒòpqrstuūúǔùüǖǘǚǜwxyz1234'-")
  }.

