:- initialization main, halt.
:- consult("../../common.pro").

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

extrapolate_rdiffs_forward(Val, [], Val).
extrapolate_rdiffs_forward(Val, [D|Ds], Extrapolated) :-
    last(D, Last),
    NVal is Val + Last,
    extrapolate_rdiffs_forward(NVal, Ds, Extrapolated).

extrapolate_rdiffs_backward(Val, [], Val).
extrapolate_rdiffs_backward(Val, [D|Ds], Extrapolated) :-
    D = [First|_],
    NVal is First - Val,
    extrapolate_rdiffs_backward(NVal, Ds, Extrapolated).

extrapolate_diffs(Diffs, ValBack-ValForw) :-
    reverse(Diffs, RDiffs),
    extrapolate_rdiffs_backward(0, RDiffs, ValBack),
    extrapolate_rdiffs_forward(0, RDiffs, ValForw).

extrapolate_numbers(Numbers, ENumbers) :-
    generate_diffs(Numbers, Diffs),
    extrapolate_diffs(Diffs, ValBack-ValForw),
    append([ValBack|Numbers], [ValForw], ENumbers).

all_zero([]).
all_zero([0|Xs]) :- all_zero(Xs).

diffs_order([], -1).
diffs_order([D|_], -1) :- all_zero(D).
diffs_order([_|Ds], Ord) :- 
    diffs_order(Ds, NOrd), Ord is NOrd + 1.

order(Numbers, Order) :-
    generate_diffs(Numbers, Diffs),
    diffs_order(Diffs, Order).

main :-
    % read_file_lines('../inpex.txt', Lines),
    read_file_lines('../input.txt', Lines),
    map(parse_line, Lines, Numbers),
    write(Numbers), nl,
    map(order, Numbers, Orders),
    write(Orders), nl,
    map(extrapolate_numbers, Numbers, NNumbers),
    write(NNumbers), nl.
