:- initialization main, halt.
:- consult("../common.pro").

direction_vector('v', (1,0)).
direction_vector('^', (-1,0)).
direction_vector('>', (0,1)).
direction_vector('<', (0,-1)).

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

main :-
    % read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./inpex2.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    find_start(Map, Start),
    % write(Start), nl,
    valid_start_neighbour(Map, Start, StartNeighbour, StartDirection),
    % write(StartNeighbour), write(StartDirection), nl,
    %
    traverse_loop(Map, StartNeighbour, StartDirection, Visited),
    History = [Start|Visited],
    % write(History), nl,
    %
    length(History, Length),
    Distance is floor(Length / 2),
    write(Distance),
    nl.
