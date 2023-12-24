:- initialization main, halt.
:- consult("../common.pro").
:- dynamic visited/1.
:- dynamic calc_pathlength_memo/2.

dir_vector('^', (-1, 0)).
dir_vector('v', (1, 0)).
dir_vector('<', (0, -1)).
dir_vector('>', (0, 1)).
reverse_dir('^', 'v').
reverse_dir('v', '^').
reverse_dir('<', '>').
reverse_dir('>', '<').
add_vector((X1, Y1), (X2, Y2), (X3, Y3)) :- X3 is X1 + X2, Y3 is Y1 + Y2.
char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowStr),
    nth0(Col, RowStr, Char).

pos_neighbours(Map, Pos, Neighbours) :-
    char_at(Pos, Map, '.'),
    findall(NPos, (
        dir_vector(NC, Dir),
        add_vector(Pos, Dir, NPos),
        char_at(NPos, Map, C),
        \+ reverse_dir(C, NC),
        \+ visited(NPos),
        member(C, ['.', '^', 'v', '<', '>'])
    ), Neighbours).
pos_neighbours(Map, Pos, Neighbours) :-
    char_at(Pos, Map, C),
    member(C, ['^', 'v', '<', '>']),
    dir_vector(C, Dir),
    add_vector(Pos, Dir, NPos),
    char_at(NPos, Map, NC),
    member(NC, ['.', '^', 'v', '<', '>']),
    Neighbours = [NPos].

calc_pathlength(_, Pos, PathLengths) :-
    calc_pathlength_memo(Pos, PathLengths), 
    write('cache hit: '), write(Pos), nl,!.
calc_pathlength(Map, Pos, PathLengths) :-
    assertz(visited(Pos)),
    pos_neighbours(Map, Pos, Neighbours),
    (Neighbours = [] ->
        PathLengths = [0]
    ;
        maplist(calc_pathlength(Map), Neighbours, NeighbourPathLengths),
        flatten1(NeighbourPathLengths, PathLengths0),
        maplist(plus(1), PathLengths0, PathLengths)
    ),
    assertz(calc_pathlength_memo(Pos, PathLengths)).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    Start = (0,1),
    calc_pathlength(Map, Start, PathLengths),
    write(PathLengths), nl,
    max_list(PathLengths, MaxPathLength),
    write(MaxPathLength),
    nl.
