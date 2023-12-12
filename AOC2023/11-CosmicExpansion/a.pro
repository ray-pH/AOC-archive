:- initialization main, halt.
:- consult("../common.pro").

char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowChars),
    nth0(Col, RowChars, Char).
is_row_empty(Row, Map) :- \+ char_at((Row,_), Map, '#').
is_col_empty(Col, Map) :- \+ char_at((_,Col), Map, '#').

empty_rows_cols(Map, EmptyRows, EmptyCols) :-
    length(Map, RowLen), RL is RowLen - 1,
    nth0(0, Map, FirstRow), length(FirstRow, ColLen), CL is ColLen - 1,
    findall(Row, (between(0,RL,Row), is_row_empty(Row, Map)), EmptyRows),
    findall(Col, (between(0,CL,Col), is_col_empty(Col, Map)), EmptyCols).

% double_empty_col(Id, Row, EmptyCols, PaddedRow) :-
double_empty_col(_, Row, [], Row).
double_empty_col(Id, [C|Cs], [Id|ECs], [C,C|PaddedCs]) :-
    NId is Id + 1, double_empty_col(NId, Cs, ECs, PaddedCs).
double_empty_col(Id, [C|Cs], EmptyCols, [C|PaddedCs]) :-
    NId is Id + 1, double_empty_col(NId, Cs, EmptyCols, PaddedCs).
double_empty_col(Row, EmptyCols, PaddedRow) :- double_empty_col(0, Row, EmptyCols, PaddedRow).

% double_empty_row(Id, Map, EmptyRows, PaddedMap)
double_empty_row(_, Map, [], Map).
double_empty_row(Id, [Row|Rows], [Id|ERs], [Row,Row|PaddedRows]) :-
    NId is Id + 1, double_empty_row(NId, Rows, ERs, PaddedRows).
double_empty_row(Id, [Row|Rows], EmptyRows, [Row|PaddedRows]) :-
    NId is Id + 1, double_empty_row(NId, Rows, EmptyRows, PaddedRows).
double_empty_row(Map, EmptyRows, PaddedMap) :- double_empty_row(0, Map, EmptyRows, PaddedMap).

pad_map(Map, EmptyRows, EmptyCols, PaddedMap) :-
    map3(double_empty_col, Map, EmptyCols, PaddedMap1),
    double_empty_row(PaddedMap1, EmptyRows, PaddedMap).

map_galaxyCoords(Map, GalaxyCoords) :-
    findall((Row,Col), char_at((Row,Col), Map, '#'), GalaxyCoords).

list_tuplepair(List, TuplePair) :-
    findall(A-B, (member(A,List), member(B,List), compare(<,A,B)), TuplePair).

dist_tuplepair(Dist, (X1,Y1)-(X2,Y2)) :-
    DX is abs(X2-X1),
    DY is abs(Y2-Y1),
    Dist is DX + DY.

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), 
    maplist(string_chars, Lines, Map),
    % write(Map), nl,
    empty_rows_cols(Map, EmptyRows, EmptyCols),
    % write(EmptyRows), nl,
    % write(EmptyCols), nl,
    pad_map(Map, EmptyRows, EmptyCols, PaddedMap),
    % write(PaddedMap), nl,
    map_galaxyCoords(PaddedMap, GalaxyCoords),
    % write(GalaxyCoords), nl,
    list_tuplepair(GalaxyCoords, TuplePairs),
    % write(TuplePairs), nl,
    maplist(dist_tuplepair, Dists, TuplePairs),
    % write(Dists), nl,
    sum_list(Dists, Sum),
    write(Sum),
    nl.
