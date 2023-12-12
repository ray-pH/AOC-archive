:- initialization main, halt.
:- consult("../common.pro").

direction_vector('v', (1,0)).
direction_vector('^', (-1,0)).
direction_vector('>', (0,1)).
direction_vector('<', (0,-1)).

add_vector((X1,Y1), (X2,Y2), (X3,Y3)) :-
    X3 is X1 + X2,
    Y3 is Y1 + Y2.
sub_vector((X1,Y1), (X2,Y2), (X3,Y3)) :-
    X3 is X1 - X2,
    Y3 is Y1 - Y2.

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
% is_cornerpipe_char(Char) :- member(Char, ['L', 'J', '7', 'F']).
is_cornerpipe_char(Char) :- member(Char, ['L', 'J', '7', 'F', 'S']).

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

% filter_corner_pipe(Map, PipeCoords, CornerPipeCoords) :-
filter_corner_pipe(_, [], []).
filter_corner_pipe(Map, [C|Cs], CornerPipeCoords) :-
    char_at(C, Map, Char),
    (is_cornerpipe_char(Char) -> 
        CornerPipeCoords = [C|CornerPipeCoordsRest]
    ; 
        CornerPipeCoords = CornerPipeCoordsRest),
    filter_corner_pipe(Map, Cs, CornerPipeCoordsRest).

get_signed_angle((X, Y), (X1, Y1), (X2, Y2), Angle) :-
    DX1 is X1 - X, DY1 is Y1 - Y,
    DX2 is X2 - X, DY2 is Y2 - Y,
    Angle is atan2(DX2 * DY1 - DY2 * DX1, DX1 * DX2 + DY1 * DY2).

f_point_winding(_, [], 0).
f_point_winding(_, [_], 0).
% f_point_winding(P, CornerPipeCoordsExtra, Winding)
f_point_winding(P, [C1,C2|Cs], Winding) :-
    get_signed_angle(P, C1, C2, Angle),
    f_point_winding(P, [C2|Cs], WindingRest),
    Winding is Angle + WindingRest.

    
point_winding(P, CornerPipeCoordsExtra, Winding) :-
    f_point_winding(P, CornerPipeCoordsExtra, WindingRaw),!,
    % if number is small enough, consider it 0
    (abs(WindingRaw) < 1e-8 -> Winding = 0 ; Winding = WindingRaw).

map_allindex(Map, AllIndex) :-
    length(Map, RowCount),
    nth0(0, Map, Row),
    length(Row, ColCount),
    RCm1 is RowCount - 1, CCm1 is ColCount - 1,
    findall((R,C), (between(0, RCm1, R), between(0, CCm1, C)), AllIndex).

% filter_inside(Coords, CornerPipeCoords, Insides).
filter_inside([], _, []).
filter_inside([C|Cs], CornerPipeCoordsExtra, Insides) :-
    length(Cs, Len),
    write(Len), nl,
    point_winding(C, CornerPipeCoordsExtra, Winding),!,
    (Winding \= 0 -> 
        Insides = [C|InsidesRest]
    ; 
        Insides = InsidesRest),
    filter_inside(Cs, CornerPipeCoordsExtra, InsidesRest).

main :-
    % read_file_lines('./inpex3.txt', Lines),
    % read_file_lines('./inpex4.txt', Lines),
    % read_file_lines('./inpex5.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    %
    map_pipe_coords(Map, PipeCoords),
    map_allindex(Map, AllIndex),
    filter_corner_pipe(Map, PipeCoords, CornerPipeCoords),
    write(CornerPipeCoords), nl,
    %
    write("here0"), nl,
    subtract(AllIndex, PipeCoords, EmptyCoords),
    write("here1"), nl,
    CornerPipeCoords = [C1|_], append(CornerPipeCoords, [C1], CornerPipeCoordsExtra),
    filter_inside(EmptyCoords, CornerPipeCoordsExtra, Insides),
    write("here2"), nl,
    write(Insides), nl,
    length(Insides, Count),
    write(Count), nl,
    nl.
