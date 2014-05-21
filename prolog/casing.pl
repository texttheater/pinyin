:- module(casing, [
    casing/3]).

/** <module> Utilities for letter case
*/

%%	casing(?Codes, ?Lower, ?CasePattern)
%
%	True if a word, given as a code-list Codes, obeys CasePattern, which is
%	one of =allcaps=, =capitalized=, and =lower=, and Lower is the
%	downcased version of Codes. Useful for "factoring out" the casedness
%	information of a string. Extensions to arbitrary case patterns are
%	conceivable but not currently implemented.
casing(Codes, Lower, CasePattern) :-
  var(Codes),
  !,
  synthesize_case(Codes, Lower, CasePattern).
casing(Codes, Lower, CasePattern) :-
  analyze_case(Codes, Lower, CasePattern).

analyze_case(Codes, Lower, allcaps) :-
  upcase_codes(Codes, Codes),
  !,
  downcase_codes(Codes, Lower).
analyze_case(Codes, Lower, capitalized) :-
  Codes = [Code|_],
  upcase_codes([Code], [Code]),
  !,
  downcase_codes(Codes, Lower).
analyze_case(Codes, Codes, lower) :-
  downcase_codes(Codes, Codes).

synthesize_case(Codes, Lower, allcaps) :-
  !,
  upcase_codes(Lower, Codes).
synthesize_case([Code|Codes], [Low|Codes], capitalized) :-
  !,
  upcase_codes([Low], [Code]).
synthesize_case(Codes, Codes, lower).

%% upcase_codes(+AnyCaseCodes, -UpperCaseCodes) is det
upcase_codes(AnyCaseCodes, UpperCaseCodes) :-
  atom_codes(AnyCaseAtom, AnyCaseCodes),
  upcase_atom(AnyCaseAtom, UpperCaseAtom),
  atom_codes(UpperCaseAtom, UpperCaseCodes).

%% downcase_codes(+AnyCaseCodes, -LowerCaseCodes) is det
downcase_codes(AnyCaseCodes, LowerCaseCodes) :-
  atom_codes(AnyCaseAtom, AnyCaseCodes),
  downcase_atom(AnyCaseAtom, LowerCaseAtom),
  atom_codes(LowerCaseAtom, LowerCaseCodes).
