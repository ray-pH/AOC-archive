:- initialization main, halt.
:- consult("../common.pro").

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

prune_beams([], _, []) :- !.
prune_beams([B|Beams], BeamHistory, NewBeams) :-
    get_assoc(B, BeamHistory, 0),
    prune_beams(Beams, BeamHistory, NewBeams).
prune_beams([B|Beams], BeamHistory, [B|NewBeams]) :-
    \+ get_assoc(B, BeamHistory, 0),
    prune_beams(Beams, BeamHistory, NewBeams).

store_beams([], BH, BH) :- !.
store_beams([B|Beams], BeamHistory, NewBeamHistory) :-
    put_assoc(B, BeamHistory, 0, NewBeamHistory1),
    store_beams(Beams, NewBeamHistory1, NewBeamHistory).

step([], _, BH, BH) :- !.
step(Beams, Layout, BeamHistory, FinalBeamHistory) :-
    step_group(Beams, Layout, NewBeams1),
    prune_beams(NewBeams1, BeamHistory, NewBeams2),
    write(NewBeams2), nl,
    store_beams(NewBeams2, BeamHistory, NewBeamHistory),
    step(NewBeams2, Layout, NewBeamHistory, FinalBeamHistory).

step(Beams, Layout, FinalBeamHistory) :-
    empty_assoc(EmptyBeamHistory),
    step(Beams, Layout, EmptyBeamHistory, FinalBeamHistory).

get_energized_count(BeamHistory, Count) :-
    assoc_to_keys(BeamHistory, BeamHistoryKeys),
    setof(P, D^member(P-D, BeamHistoryKeys), EnergizedList),
    write(EnergizedList), nl,
    length(EnergizedList, Count).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Layout),
    StartPos = (0,-1),
    StartDir = '>',
    Beams = [StartPos-StartDir],
    %
    step(Beams, Layout, BeamHistory),
    get_energized_count(BeamHistory, EnergizedCount),
    write(EnergizedCount), nl,
    %
    nl.
