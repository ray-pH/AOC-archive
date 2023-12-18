:- initialization main, halt.
:- consult("../common.pro").

parse_line(Line, Numbers) :-
    split_string(Line, " ", "", NumbersStr),
    map(number_string, Numbers, NumbersStr).

vec_sub([],[],[]).
vec_sub([A|As], [B|Bs], [C|Cs]) :- C is A - B, vec_sub(As, Bs, Cs).

generate_diff_single(Numbers, Diff) :-
    init(Numbers, Head),
    tail(Numbers, Tail),
    vec_sub(Tail, Head, Diff).

generate_diffs([X],[[X]]).
generate_diffs(Numbers, [Numbers|Diffs]) :-
    generate_diff_single(Numbers, Diff),
    generate_diffs(Diff, Diffs).

extrapolate_rdiffs(Val, [], Val).
extrapolate_rdiffs(Val, [D|Ds], Extrapolated) :-
    last(D, Last),
    NVal is Val + Last,
    extrapolate_rdiffs(NVal, Ds, Extrapolated).

extrapolate_diffs(Diffs, Val) :-
    reverse(Diffs, RDiffs),
    extrapolate_rdiffs(0, RDiffs, Val).

next_value(Numbers, Value) :-
    generate_diffs(Numbers, Diffs),
    extrapolate_diffs(Diffs, Value).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    map(parse_line, Lines, Numbers),
    map(next_value, Numbers, NextValues),
    sum_list(NextValues, Sum),
    write(NextValues), nl,
    write(Sum),
    nl.
