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

add_list([], [] ,[]).
add_list([A|As], [B|Bs], [C|Cs]) :-
    C is A + B,
    add_list(As, Bs, Cs).

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
    foldl(add_list, ColorVecs, [0, 0, 0], [R, G, B]).

parseLine(Line, [GameNum, ColorVecs]) :-
    split_string(Line, ":", " ", [GameStr, RestStr]),
    split_string(GameStr, " ", "", [_, GameNumStr]),
    number_string(GameNum, GameNumStr),
    split_string(RestStr, ";", " ", Colors),
    map(parseColors, Colors, ColorVecs).

colorVecPossible([R, G, B], [Rmax, Gmax, Bmax]) :-
    R =< Rmax, G =< Gmax, B =< Bmax.
% gamePossible(ColorVecs, MaxColorVec) :-
gamePossible([], _).
gamePossible([CV|CVs], MaxColorVec) :-
    colorVecPossible(CV, MaxColorVec),
    gamePossible(CVs, MaxColorVec).

% sumPossibleGameNumber(Lines, Number)
sumPossibleGameNumber([], _, 0).
sumPossibleGameNumber([L|Ls], MaxColorVec, Sum):-
    parseLine(L, [GameNum, ColorVecs]),
    sumPossibleGameNumber(Ls, MaxColorVec, RestSum),
    (gamePossible(ColorVecs, MaxColorVec) ->
        Sum is GameNum + RestSum
    ;
        Sum is RestSum
    ).

main :-
    read_file_lines('./input.txt', Lines),
    sumPossibleGameNumber(Lines, [12, 13, 14], Sum),
    write(Sum), nl.
