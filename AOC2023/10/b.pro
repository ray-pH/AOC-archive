:- initialization main, halt.
:- consult("../common.pro").

direction_vector('v', (1,0)).
direction_vector('^', (-1,0)).
direction_vector('>', (0,1)).
direction_vector('<', (0,-1)).

direction_reverse('v', '^').
direction_reverse('^', 'v').
direction_reverse('>', '<').
direction_reverse('<', '>').

add_vector((X1,Y1), (X2,Y2), (X3,Y3)) :-
    X3 is X1 + X2,
    Y3 is Y1 + Y2.

% pipe_direction(InitialDirection, PipeChar, NewDirection)
pipe_direction('v', '|', 'v').
pipe_direction('^', '|', '^').
pipe_direction('>', '-', '>').
pipe_direction('<', '-', '<').
pipe_direction('v', 'L', '>').
pipe_direction('<', 'L', '^').
pipe_direction('v', 'J', '<').
pipe_direction('>', 'J', '^').
pipe_direction('^', '7', '<').
pipe_direction('>', '7', 'v').
pipe_direction('^', 'F', '>').
pipe_direction('<', 'F', 'v').

is_pipe_char(Char) :- member(Char, ['|', '-', 'L', 'J', '7', 'F']).
is_pipeconnectbefore_char(Char) :- member(Char, ['-', '7', 'J']).
is_hopeningpipe_char(Char) :- member(Char, ['F', 'L']).
is_hclosingpipe_char(Char) :- member(Char, ['J', '7']).

% F-7 : no change
% F-J : flip
% L-7 : flip
% L-J : no change
pipepair_flipstate('F','J').
pipepair_flipstate('L','7').

char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowChars),
    nth0(Col, RowChars, Char).

find_start(Map, StartPos) :-
    char_at(StartPos, Map, 'S').

% valid_start_neighbout(Map, StartPos, NeighbourPos) :-
valid_start_neighbour(Map, StartPos, NeighbourPos, StartDirection) :-
    direction_vector(StartDirection, DV), 
    add_vector(StartPos, DV, NeighbourPos),
    char_at(NeighbourPos, Map, Char),
    pipe_direction(StartDirection, Char, _).

move_once(Map, Pos, Direction, NewPos, NewDirection) :-
    char_at(Pos, Map, Char),
    pipe_direction(Direction, Char, NewDirection),
    direction_vector(NewDirection, DV),
    % write(Direction), write(' '), write(Char), write(' '), write(NewDirection), nl,
    add_vector(Pos, DV, NewPos).

traverse_loop(Map, Pos, _, []):- 
    char_at(Pos, Map, 'S').
traverse_loop(Map, Pos, Direction, [Pos|VisitedRest]) :-
    move_once(Map, Pos, Direction, NewPos, NewDirection),
    traverse_loop(Map, NewPos, NewDirection, VisitedRest).

map_pipe_coords(Map, PipeCoords) :-
    find_start(Map, Start),
    valid_start_neighbour(Map, Start, StartNeighbour, StartDirection),
    traverse_loop(Map, StartNeighbour, StartDirection, Visited),
    PipeCoords = [Start|Visited].

% state : 0 "Outside", 1 "Inside"
flip_state(0, 1).
flip_state(1, 0).

is_row_pipe(Col, PolygonRowCoords) :- member(Col, PolygonRowCoords).


get_hclosing_pipe(Col, MapRow, Col) :-
    nth0(Col, MapRow, C), is_hclosingpipe_char(C).
get_hclosing_pipe(Col, MapRow, ClosingCol) :-
    nth0(Col, MapRow, C),
    \+ is_hclosingpipe_char(C),
    NCol is Col + 1,
    get_hclosing_pipe(NCol, MapRow, ClosingCol).


need_to_flip(Col, MapRow) :- nth0(Col, MapRow, '|').
need_to_flip(Col, MapRow) :- 
    nth0(Col, MapRow, C),
    is_hopeningpipe_char(C),
    get_hclosing_pipe(Col, MapRow, ClosingCol),
    nth0(ClosingCol, MapRow, ClosingC),
    pipepair_flipstate(C, ClosingC).

% polygon_row_insides(PoygonRowCoords, Map, Insides) :-
polygon_row_insides(Col, _, _, MapRow, []) :-
    length(MapRow, MapRowLength), Col >= MapRowLength.
%pipe:
polygon_row_insides(Col, State, PoygonRowCoords, MapRow, Insides) :-
    is_row_pipe(Col, PoygonRowCoords),
    (need_to_flip(Col, MapRow) -> 
        flip_state(State, NState)
    ;
        NState = State),
    NCol is Col + 1,
    % nth0(Col, MapRow, C),
    % write('Pipe '), write(Col), write(': '), write(State), write(' '), write(C), write(' '), write(NState), nl,
    polygon_row_insides(NCol, NState, PoygonRowCoords, MapRow, Insides).
% not a pipe:
polygon_row_insides(Col, State, PoygonRowCoords, MapRow, Insides) :-
    \+ is_row_pipe(Col, PoygonRowCoords),
    % if inside, add to insides
    (State = 1 -> Insides = [Col|NInsides]; Insides = NInsides),
    NCol is Col + 1,
    % write('Not  '), write(Col), write(' '), write(State), nl,
    polygon_row_insides(NCol, State, PoygonRowCoords, MapRow, NInsides).

polygon_insides(Row, _, Map, []) :-
    length(Map, MapLength), Row >= MapLength.
polygon_insides(Row, PolygonCoords, Map, [Row-RowInsides|NInsides]) :-
    nth0(Row, Map, MapRow),
    findall(Col, (member(Coord, PolygonCoords), Coord = (Row,Col)), PolygonRowCoords),
    % string_chars(MapRowStr, MapRow),
    % write(MapRowStr), nl,
    % write(PolygonRowCoords), nl,
    polygon_row_insides(0, 0, PolygonRowCoords, MapRow, RowInsides),
    NRow is Row + 1,
    polygon_insides(NRow, PolygonCoords, Map, NInsides).

polygon_insides(PolygonCoords, Map, Insides) :-
    polygon_insides(0, PolygonCoords, Map, Insides).

startpipe_type(Map, StartPos, SPipeChar) :-
    findall(StartDirection, valid_start_neighbour(Map, StartPos, _, StartDirection), StartDirections),
    length(StartDirections, 2), % make sure correct length
    [SD1Rev, SD2] = StartDirections,
    direction_reverse(SD1, SD1Rev),
    pipe_direction(SD1, SPipeChar, SD2).
map_patch_startpipe(Map, PatchedMap) :-
    find_start(Map, StartPos),
    startpipe_type(Map, StartPos, SPipeChar),
    (Row,Col) = StartPos,
    list2D_change_element(Map, [Row,Col], SPipeChar, PatchedMap).

insides_count(Insides, Count) :-
    maplist([X,Y]>>(X = _-Y), Insides, Insides2),
    maplist(length, Insides2, Lengths),
    sumlist(Lengths, Count).

main :-
    % read_file_lines('./inpex3.txt', Lines),
    % read_file_lines('./inpex4.txt', Lines),
    % read_file_lines('./inpex5.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    % write(Map), nl,
    map_patch_startpipe(Map, PatchedMap),
    % write(PatchedMap), nl,
    %
    map_pipe_coords(Map, PipeCoords),
    polygon_insides(PipeCoords, PatchedMap, Insides),
    write(Insides), nl,
    insides_count(Insides, Count),
    write(Count), nl,
    %
    nl.
