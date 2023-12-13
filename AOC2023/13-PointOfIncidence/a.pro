:- initialization main, halt.
:- consult("../common.pro").

pattern_VreflectionId(Pattern, Id) :-
    length(Pattern, Len), Lm1 is Len - 1,
    between(1, Lm1, Id),
    take(Id, Pattern, Up),
    drop(Id, Pattern, Down),
    reverse(Up, RUp),
    (prefix(RUp, Down); prefix(Down, RUp)), !.
pattern_VreflectionId(_, 0).

pattern_HreflectionId(Pattern, Id) :-
    transpose(Pattern, TPattern),
    pattern_VreflectionId(TPattern, Id).

value(Pattern, Value) :-
    pattern_VreflectionId(Pattern, VId),
    pattern_HreflectionId(Pattern, HId),
    Value is HId + 100*VId.

main :-
    % ---------- parsing ----------
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    split_by('', Lines, PatternsStr),
    maplist([X,Y]>>(maplist(string_chars,X,Y)), PatternsStr, Patterns),
    % ---------- parsing ----------
    maplist(value, Patterns, Values),
    write(Values), nl,
    sum_list(Values, Sum),
    write(Sum),
    nl.
