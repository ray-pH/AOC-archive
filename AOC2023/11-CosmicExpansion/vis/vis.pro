:- initialization main, halt.
:- consult("../../common.pro").

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

map_galaxyCoords(Map, GalaxyCoords) :-
    findall((Row,Col), char_at((Row,Col), Map, '#'), GalaxyCoords).

list_tuplepair(List, TuplePair) :-
    findall(A-B, (member(A,List), member(B,List), compare(<,A,B)), TuplePair).

dist_tuplepair(Dist, (X1,Y1)-(X2,Y2)) :-
    DX is abs(X2-X1),
    DY is abs(Y2-Y1),
    Dist is DX + DY.

coord_col_nexpansion((_,Col), EmptyCols, NExpansion) :-
    findall(C, (member(C,EmptyCols), C < Col), LeftCols),
    length(LeftCols, NExpansion).
coord_row_nextpansion((Row,_), EmptyRows, NExpansion) :-
    findall(R, (member(R,EmptyRows), R < Row), UpRows),
    length(UpRows, NExpansion).
pos_expanded((Row,Col), [EmptyRows, EmptyCols, ExpFactor], ExpandedPos) :-
    coord_col_nexpansion((Row,Col), EmptyCols, ColNExpansion),
    coord_row_nextpansion((Row,Col), EmptyRows, RowNExpansion),
    ColExpansion is ColNExpansion * (ExpFactor - 1),
    RowExpansion is RowNExpansion * (ExpFactor - 1),
    ExpandedRow is Row + RowExpansion,
    ExpandedCol is Col + ColExpansion,
    ExpandedPos = (ExpandedRow,ExpandedCol).

expfactor_expandedcoords(ExpFactor, [Coords, EmptyRows, EmptyCols], ExpandedCoords) :-
    map3(pos_expanded, Coords, [EmptyRows, EmptyCols, ExpFactor], ExpandedCoords).

main :-
    % read_file_lines('../inpex.txt', Lines),
    read_file_lines('../input.txt', Lines),
    maplist(string_chars, Lines, Map),
    % write(Map), nl,
    empty_rows_cols(Map, EmptyRows, EmptyCols),
    % write(EmptyRows), nl,
    % write(EmptyCols), nl,
    map_galaxyCoords(Map, GalaxyCoords),
    % write(GalaxyCoords), nl,
    % ExpFactors = 1..=100
    findall(X, between(1,100,X), ExpFactors),
    map3(expfactor_expandedcoords, ExpFactors, [GalaxyCoords, EmptyRows, EmptyCols], ExpandedCoords),
    write_list(ExpandedCoords).
