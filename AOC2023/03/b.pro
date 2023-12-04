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
map3(_, [], _, []).
map3(Pred, [A|As], B, [C|Cs]) :-
    call(Pred, A, B, C),
    map3(Pred, As, B, Cs).
flatten1([], []).
flatten1([[]|Cs], Flattened) :-
    flatten1(Cs, Flattened).
flatten1([C|Cs], Flattened) :-
    flatten1(Cs, FlattenedRest),
    append(C, FlattenedRest, Flattened).
drop(0, L, L).
drop(N, [_|L], Dropped) :-
    N > 0, N1 is N - 1,
    drop(N1, L, Dropped).
dedup([], []).
dedup([X|Xs], [X|Ys]) :-
    dedup(Xs, Ys),
    \+ member(X, Ys).
dedup([X|Xs], Ys) :-
    dedup(Xs, Ys),
    member(X, Ys).

list_from_repeat(0, _, []).
list_from_repeat(N, X, [X|Xs]) :-
    N > 0, N1 is N - 1, list_from_repeat(N1, X, Xs).
string_from_repeat(N, C, String) :-
    list_from_repeat(N, C, List),
    string_chars(String, List).

padSchematicRow(Row, PaddedRow) :-
    string_concat('.', Row, PaddedRow1),
    string_concat(PaddedRow1, '.', PaddedRow).
padSchematicColumn(Schematic, PaddedSchematic) :-
    nth0(0, Schematic, Row),
    string_length(Row, Len),
    string_from_repeat(Len, '.', RowPadding),
    append([RowPadding|Schematic], [RowPadding], PaddedSchematic).

char_at(R,C, Schematic, Char) :-
    nth0(R, Schematic, Row),
    string_chars(Row, Chars),
    nth0(C, Chars, Char).

schematic_coordlist(Schematic, CoordList) :-
    length(Schematic, RowLen),
    nth0(0, Schematic, Row0),
    string_length(Row0, ColLen),
    RowLenm1 is RowLen-1,
    ColLenm1 is ColLen-1,
    findall([R,C], (between(1, RowLenm1, R), between(1, ColLenm1, C)), CoordList).
pos_neighbours([R,C], Neighbours) :-
    Rm1 is R-1, Cm1 is C-1,
    Rp1 is R+1, Cp1 is C+1,
    findall([R1,C1], (member(R1, [Rm1,R,Rp1]), member(C1, [Cm1,C,Cp1]), \+ (R1 = R, C1 = C)), Neighbours).

is_gear(Schematic, [R,C]) :- char_at(R,C, Schematic, '*').
schematic_gearLocs(Schematics, GearLocs) :-
    schematic_coordlist(Schematics, CoordList),
    findall([R,C], (member([R,C], CoordList), is_gear(Schematics, [R,C])), GearLocs).

parse_numberChars_left([], []).
parse_numberChars_left([CnotDigit|_], []) :-
    \+ char_type(CnotDigit, digit).
parse_numberChars_left([Cdigit|Cs], [Cdigit|NumberCharsRest]) :-
    char_type(Cdigit, digit),
    parse_numberChars_left(Cs, NumberCharsRest).

% if C-1 is a digit:
pos_schematic_PNdata([R,C], Schematic, PNdata) :-
    char_at(R,C, Schematic, Char),
    char_type(Char, digit),
    Cm1 is C-1,
    char_at(R,Cm1, Schematic, PrevCharDigit),
    char_type(PrevCharDigit, digit),
    pos_schematic_PNdata([R,Cm1], Schematic, PNdata).
% else, parse from there
pos_schematic_PNdata([R,C], Schematic, PNdata) :-
    char_at(R,C, Schematic, Char),
    char_type(Char, digit),
    Cm1 is C-1,
    char_at(R,Cm1, Schematic, PrevCharDigit),
    \+ char_type(PrevCharDigit, digit),
    nth0(R, Schematic, Row),
    string_chars(Row, RowChars),
    drop(C, RowChars, RowCharsDropped),
    parse_numberChars_left(RowCharsDropped, NumberChars),
    string_chars(NumberString, NumberChars),
    number_string(Number, NumberString),
    PNdata = [R,C,Number].

pos_neighbours_PNdata(Pos, Schematic, PNdatas) :-
    pos_neighbours(Pos, Neighbours),
    findall(PNdata, (member(N, Neighbours), pos_schematic_PNdata(N, Schematic, PNdata)), PNdatasdup),
    dedup(PNdatasdup, PNdatas).

gearpos_PNdatas(GearPos, Schematic, PNdatas) :-
    pos_neighbours_PNdata(GearPos, Schematic, PNdatas).

gearPNdatas_gearRatio([PN1,PN2], GearRatio) :-
    PN1 = [_,_,N1],
    PN2 = [_,_,N2],
    GearRatio is N1 * N2.
% len is not 2 :
gearPNdatas_gearRatio(_, 0).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(padSchematicRow, Lines, PaddedLines),
    padSchematicColumn(PaddedLines, Schematic),
    schematic_gearLocs(Schematic, GearLocs),
    map3(gearpos_PNdatas, GearLocs, Schematic, GearPNdatas),
    map(gearPNdatas_gearRatio, GearPNdatas, GearRatios),
    sumlist(GearRatios, GearRatiosSum),
    % write(Schematic), nl, nl,
    % write(GearLocs), nl, nl,
    % write(GearPNdatas), nl, nl,
    % write(GearRatios), nl, nl,
    write(GearRatiosSum), nl, nl,
    nl.
