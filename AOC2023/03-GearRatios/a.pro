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
flatten1([], []).
flatten1([[]|Cs], Flattened) :-
    flatten1(Cs, Flattened).
flatten1([C|Cs], Flattened) :-
    flatten1(Cs, FlattenedRest),
    append(C, FlattenedRest, Flattened).

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

% row_indexedRow(Row, IndexedRow) :-
%     string_chars(Row, Chars),
%     maplist(row_indexedRowChar, Chars, IndexedRowChars),
%     string_chars(IndexedRow, IndexedRowChars).

row_PartNumberLocations(RowId, Row, PartNumberLocs) :-
    string_chars(Row, Chars),
    f_row_PNLocs(RowId, Chars, 0, "", PartNumberLocs).
% f_row_PNLocs(Row, Index, CurentStr, PartNumberLocs, NewPartNumberLocs) :-
f_row_PNLocs(_,[],_,_,[]).
f_row_PNLocs(RId, [Cdigit|Cs], Id, CurrentStr, PNLocs) :-
    char_type(Cdigit,digit),
    string_concat(CurrentStr, Cdigit, NewCurrentStr),
    NextId is Id + 1,
    f_row_PNLocs(RId, Cs, NextId, NewCurrentStr, PNLocs).
f_row_PNLocs(RId,[_|Cs], Id, "", PNLocs) :-
    NextId is Id + 1,
    f_row_PNLocs(RId, Cs, NextId, "", PNLocs).
f_row_PNLocs(RId, [_|Cs], Id, CurrentStr, PNLocs) :-
    NextId is Id + 1,
    f_row_PNLocs(RId, Cs, NextId, "", NextPNLocs),
    string_length(CurrentStr, Len),
    FrontId is Id - Len,
    PNLocs = [[RId, FrontId, CurrentStr]|NextPNLocs].

schematic_PartNumberLocations(Schematic, PartNumberLocs) :-
    f_schema_PNLocs(Schematic, 0, PartNumberLocsUnflattened),
    flatten1(PartNumberLocsUnflattened, PartNumberLocs).
% f_schema_PNLocs(Schematic, Id, PartNumberLocs).
f_schema_PNLocs([], _, []).
f_schema_PNLocs([Row|Rows], Id, [RowPartNumberLocs|NextPartNumberLocs]):-
    row_PartNumberLocations(Id, Row, RowPartNumberLocs),
    NextId is Id + 1,
    f_schema_PNLocs(Rows, NextId, NextPartNumberLocs).

pos_range(R, Cend, Cend, Poss) :-
    Poss = [[R,Cend]].
pos_range(R, Ccurr, Cend, Poss) :-
    Ccurr < Cend,
    Cnext is Ccurr + 1,
    pos_range(R, Cnext, Cend, NextPoss),
    Poss = [[R,Ccurr]|NextPoss].
%
pos_neighbour([R,Chead], [R,Ctail], Neighbours) :-
    NLeft  = [R,Cleft],  Cleft is Chead - 1,
    NRight = [R,Cright], Cright is Ctail + 1,
    NMid   = [NLeft, NRight],
    pos_range(Rtop, Cleft, Cright, NTop), Rtop is R - 1,
    pos_range(Rbot, Cleft, Cright, NBot), Rbot is R + 1,
    flatten1([NTop, NMid, NBot], Neighbours).

is_symbol(C) :- \+ char_type(C, digit), C \= '.'.
% is_symbol(C) :- C \= '.'.
no_symbol_in_pos(_, []).
no_symbol_in_pos(Schematic, [P|Ps]) :-
    [R,C] = P,
    nth0(R, Schematic, Row), 
    string_chars(Row, Chars),
    nth0(C, Chars, Char),
    (is_symbol(Char) -> 
        false 
    ; 
        no_symbol_in_pos(Schematic, Ps)
    ).

is_valid_PN(Schematic, [R,Chead,PNStr]) :-
    string_length(PNStr, Len),
    Ctail is Chead + Len - 1,
    pos_neighbour([R,Chead], [R,Ctail], Neighbours),
    \+ no_symbol_in_pos(Schematic, Neighbours).

% sum_valid_PNs(Schematic, PNLocs, Sum) :-
sum_valid_PNs(_, [], 0).
sum_valid_PNs(Schematic, [PNLoc|PNLocs], Sum) :-
    sum_valid_PNs(Schematic, PNLocs, NextSum),
    PNLoc = [_,_,PNStr],
    number_string(PN, PNStr),
    (is_valid_PN(Schematic, PNLoc) -> 
        Sum is NextSum + PN
    ;
        Sum is NextSum
    ).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(padSchematicRow, Lines, PaddedLines),
    padSchematicColumn(PaddedLines, Schematic),
    % write(Schematic), nl, nl,
    schematic_PartNumberLocations(Schematic, PNLocs),
    % write(PNLocs), nl, nl,
    sum_valid_PNs(Schematic, PNLocs, Sum),
    write(Sum),
    nl.
