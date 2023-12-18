:- initialization main, halt.
:- consult("../common.pro").

border_direction_char('R', '-').
border_direction_char('D', '|').
border_direction_char('L', '-').
border_direction_char('U', '|').
border_direction_char('R', 'D', '7').
border_direction_char('D', 'L', 'J').
border_direction_char('L', 'U', 'L').
border_direction_char('U', 'R', 'F').
border_direction_char('R', 'U', 'J').
border_direction_char('U', 'L', '7').
border_direction_char('L', 'D', 'F').
border_direction_char('D', 'R', 'L').

direction_vector('R', (0, 1)).
direction_vector('D', (1, 0)).
direction_vector('L', (0, -1)).
direction_vector('U', (-1, 0)).
add_vector((X1, Y1), (X2, Y2), (X3, Y3)) :- X3 is X1 + X2, Y3 is Y1 + Y2.
% c = a + m*b
add_vector_scaled((X1, Y1), (X2, Y2), M, (X3, Y3)) :- X3 is X1 + X2 * M, Y3 is Y1 + Y2 * M.

string_inner(Str, Inner) :-
    string_chars(Str, Chars),
    append([['('], InnerChars, [')']], Chars),
    string_chars(Inner, InnerChars).

parse_line(Line, Parsed) :-
    split_string(Line, " ", "", [DirStr, NumStr, ColorInParen]),
    number_string(Num, NumStr),
    string_inner(ColorInParen, Color),
    string_chars(DirStr, [Dir]),
    Parsed = Dir-Num-Color.

% Plan : list of Dir-Num-Color
% BorderList : list of (X, Y)-Char
% gen_border(Plan, CurrentPos, BorderList)

gen_line_border(CurrentPos, Dir, Num, Nextdir, BorderList) :-
    border_direction_char(Dir, CharLine),
    direction_vector(Dir, Vector),
    Nm1 is Num - 1,
    findall((X,Y)-CharLine, (between(1, Nm1, I), add_vector_scaled(CurrentPos, Vector, I, (X,Y))), BorderList0),
    add_vector_scaled(CurrentPos, Vector, Num, NextPos),
    border_direction_char(Dir, Nextdir, CharCorner),
    append(BorderList0, [(NextPos-CharCorner)], BorderList).

gen_border([], _, []).
gen_border([_], _, []).
gen_border([Pl1,Pl2|Pls], CurrentPos, BorderList):-
    Pl1 = Dir1-Num1-_,
    Pl2 = Dir2-_-_,
    gen_line_border(CurrentPos, Dir1, Num1, Dir2, BorderList0),
    reverse(BorderList0, [(NextPos-_)|_]),
    append(BorderList0, BorderList1, BorderList),
    gen_border([Pl2|Pls], NextPos, BorderList1).
gen_border(Plan, BorderList) :-
    Plan = [P|_],
    append(Plan, [P], PlanWithTail),
    gen_border(PlanWithTail, (0,0), BorderList).

col_rowchar(Col, [Row,RowBorderList], Char) :-
    member((Row,Col)-Char, RowBorderList),!.
col_rowchar(_, _, '.').

generate_row(Row, BorderList, MapSize, RowChars) :-
    MapSize = (_,_)-(MaxCol,MinCol),
    findall((Row,C)-Char, member((Row,C)-Char, BorderList), RowBorderList),
    findall(C, between(MinCol, MaxCol, C), Cols),
    map3(col_rowchar, Cols, [Row,RowBorderList], RowChars).
    
generate_map(BorderList, Map) :-
    maplist([X,Y]>>(X=(Y,_)-_), BorderList, Rows),
    maplist([X,Y]>>(X=(_,Y)-_), BorderList, Cols),
    max_list(Rows, MaxRow),
    max_list(Cols, MaxCol),
    min_list(Rows, MinRow),
    min_list(Cols, MinCol),
    MapSize = (MaxRow,MinRow)-(MaxCol,MinCol),
    findall(RowChars, (between(MinRow, MaxRow, Row), generate_row(Row, BorderList, MapSize, RowChars)), Map).


% -------------------- polygon insides --------------------

is_pipe_char(Char) :- member(Char, ['|', '-', 'L', 'J', '7', 'F']).
is_pipe_flipping_char(Char) :- member(Char, ['|', 'L', 'J']).

flip_state(0, 1).
flip_state(1, 0).
% row_insides(Col, State, RowChar, RowInsides) :-
row_insides(_, _, [], []).
row_insides(Col, State, [RC|RCs], RowInsides) :-
    is_pipe_char(RC),
    (is_pipe_flipping_char(RC) -> flip_state(State, NextState); NextState = State),
    NextCol is Col + 1,
    row_insides(NextCol, NextState, RCs, RowInsides).
row_insides(Col, State, [RC|RCs], RowInsides) :-
    \+ is_pipe_char(RC),
    (State = 1 -> RowInsides = [Col|NextRowInsides]; RowInsides = NextRowInsides),
    NextCol is Col + 1,
    row_insides(NextCol, State, RCs, NextRowInsides).

row_insides(RowChar, RowInsides) :-
    row_insides(0, 0, RowChar, RowInsides).

% row_insides(RowChar, RowInsides) :-
polygon_insides(Map, Insides) :-
    maplist(row_insides, Map, RowInsides),
    enumerate(RowInsides, Insides).
insides_count(Insides, Count) :-
    maplist([X,Y]>>(X = _-Y), Insides, Insides2),
    maplist(length, Insides2, Lengths),
    sumlist(Lengths, Count).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(parse_line, Lines, Plan),
    % write(Plan), nl,
    gen_border(Plan, BorderList),
    % write(BorderList),nl,
    length(BorderList, BorderListLength),
    generate_map(BorderList, Map),
    % maplist(string_chars, MapStr, Map), maplist(writeln, MapStr),
    polygon_insides(Map, Insides),
    % write(Insides), nl,
    insides_count(Insides, Count),
    % write(Count), nl,
    TotalCount is Count + BorderListLength,
    write(TotalCount),
    nl.

