:- initialization main, halt.
:- consult("../common.pro").
:- dynamic energized/1.

% (row, col)
direction_vector('>', (0,1)).
direction_vector('<', (0,-1)).
direction_vector('^', (-1,0)).
direction_vector('v', (1,0)).

add_vectors((X1,Y1), (X2,Y2), (X3,Y3)) :-
    X3 is X1 + X2, Y3 is Y1 + Y2.

char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowChars),
    nth0(Col, RowChars, Char).

% handle_contraption(Pos-Dir, Contraption, NewBeams)
handle_contraption(Beam,    '.',  [Beam]) :- !.
handle_contraption(Pos-'>', '/',  [Pos-'^']) :- !.
handle_contraption(Pos-'<', '/',  [Pos-'v']) :- !.
handle_contraption(Pos-'^', '/',  [Pos-'>']) :- !.
handle_contraption(Pos-'v', '/',  [Pos-'<']) :- !.
handle_contraption(Pos-'>', '\\', [Pos-'v']) :- !.
handle_contraption(Pos-'<', '\\', [Pos-'^']) :- !.
handle_contraption(Pos-'^', '\\', [Pos-'<']) :- !.
handle_contraption(Pos-'v', '\\', [Pos-'>']) :- !.
handle_contraption(Pos-'>', '-',  [Pos-'>']) :- !.
handle_contraption(Pos-'<', '-',  [Pos-'<']) :- !.
handle_contraption(Pos-'^', '|',  [Pos-'^']) :- !.
handle_contraption(Pos-'v', '|',  [Pos-'v']) :- !.
handle_contraption(Pos-'^', '-',  [Pos-'>', Pos-'<']) :- !.
handle_contraption(Pos-'v', '-',  [Pos-'>', Pos-'<']) :- !.
handle_contraption(Pos-'>', '|',  [Pos-'^', Pos-'v']) :- !.
handle_contraption(Pos-'<', '|',  [Pos-'^', Pos-'v']) :- !.

step_single(Pos-Dir, Layout, NewBeams) :-
    direction_vector(Dir, DirVec),
    add_vectors(Pos, DirVec, NewPos),
    (char_at(NewPos, Layout, Contraption) ->
        handle_contraption(NewPos-Dir, Contraption, NewBeams)
    ;
        NewBeams = []
    ).
step_group(Beams, Layout, NewBeams) :-
    map3(step_single, Beams, Layout, NewBeamsList),
    flatten1(NewBeamsList, NewBeams).

store_beams([]) :- !.
store_beams([B|Beams]) :-
    assertz(energized(B)),
    store_beams(Beams).

prune_beams([], []) :- !.
prune_beams([B|Beams], NewBeams) :-
    energized(B),
    prune_beams(Beams, NewBeams).
prune_beams([B|Beams], [B|NewBeams]) :-
    \+ energized(B),
    prune_beams(Beams, NewBeams).

step([], _) :- !.
step(Beams, Layout) :-
    step_group(Beams, Layout, NewBeams1),
    prune_beams(NewBeams1, NewBeams2),
    store_beams(NewBeams2),
    write(NewBeams2), nl,
    step(NewBeams2, Layout).

get_energized_count(Count) :-
    setof(P, D^energized(P-D), EnergizedList),
    % write(EnergizedList), nl,
    length(EnergizedList, Count).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Layout),
    StartPos = (0,-1),
    StartDir = '>',
    Beams = [StartPos-StartDir],
    %
    step(Beams, Layout),
    get_energized_count(EnergizedCount),
    write(EnergizedCount), nl,
    %
    nl.
