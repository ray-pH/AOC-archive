:- initialization main, halt.

read_file_lines(File, Lines):-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, []):-
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]):-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes),
    read_lines(Stream, Rest).

map(_, [], []).
map(Pred, [A|As], [B|Bs]) :-
    call(Pred, A, B),
    map(Pred, As, Bs).

add(A,B,C) :- C is A + B.
max(A,B,C) :- C is max(A, B).
elementwise_list(_, [], [], []).
elementwise_list(Predicate, [A|As], [B|Bs], [C|Cs]) :-
    call(Predicate, A, B, C),
    elementwise_list(Predicate, As, Bs, Cs).

add_ew_list(A, B, C) :- elementwise_list(add, A, B, C).
max_ew_list(A, B, C) :- elementwise_list(max, A, B, C).

singleColorStr_vec(SColorStr, [R, 0, 0]):-
    split_string(SColorStr, " ", "", [RStr, "red"]),
    number_string(R, RStr).
singleColorStr_vec(SColorStr, [0, G, 0]):-
    split_string(SColorStr, " ", "", [GStr, "green"]),
    number_string(G, GStr).
singleColorStr_vec(SColorStr, [0, 0, B]):-
    split_string(SColorStr, " ", "", [BStr, "blue"]),
    number_string(B, BStr).

% "3 blue, 4 red"
parseColors(ColorStr, [R, G, B]) :-
    split_string(ColorStr, ",", " ", Colors),
    map(singleColorStr_vec, Colors, ColorVecs),
    foldl(add_ew_list, ColorVecs, [0, 0, 0], [R, G, B]).

parseLine(Line, ColorVecs) :-
    split_string(Line, ":", " ", [_, RestStr]),
    split_string(RestStr, ";", " ", Colors),
    map(parseColors, Colors, ColorVecs).

% minimumColorVec(ColorVecs, MinColorVec)
minimumColorVec([], [0, 0, 0]).
minimumColorVec([CV|CVs], MinColorVec) :-
    minimumColorVec(CVs, RestMinColorVec),
    max_ew_list(CV, RestMinColorVec, MinColorVec).

line_power(Line, Power) :-
    parseLine(Line, ColorVecs),
    minimumColorVec(ColorVecs, [Rmin, Gmin, Bmin]),
    Power is Rmin * Gmin * Bmin.

main :-
    read_file_lines('./input.txt', Lines),
    % read_file_lines('./inpex.txt', Lines),
    map(line_power, Lines, Powers),
    sum_list(Powers, TotalPower),
    write(TotalPower), nl,
    write(Powers), nl.
