:- initialization main, halt.
:- consult("../common.pro").

parse_line(Line, [CharArr, Nums]) :-
    split_string(Line, " ", "", [RowStr, NumsStr]),
    string_chars(RowStr, CharArr),
    split_string(NumsStr, ",", "", NumArr),
    maplist(number_string, Nums, NumArr).

% f_possible_row(Row, Nums, DamagedAcc, Counting)
f_possible_row([], [N], N, 1).
f_possible_row([], [], 0, _).
f_possible_row(['?'|_], _, _, _).
f_possible_row(['.'|Cs], Nums, 0, S) :-
    f_possible_row(Cs, Nums, 0, S).
f_possible_row(['#'|Cs], Nums, 0, 0) :-
    f_possible_row(Cs, Nums, 1, 1).
f_possible_row(['.'|Cs], [N|Ns], DamagedAcc, 1) :-
    N = DamagedAcc,
    f_possible_row(Cs, Ns, 0, 0).
f_possible_row(['#'|Cs], Nums, DamagedAcc, 1) :-
    DamagedAcc1 is DamagedAcc + 1,
    f_possible_row(Cs, Nums, DamagedAcc1, 1).

possible_row(Row, Nums):-
    f_possible_row(Row, Nums, 0, 0).

row_done(Row) :- \+ member('?', Row).
rows_not_done(Rows) :- member(Row, Rows), \+ row_done(Row), !.
rows_done(Rows) :- \+ rows_not_done(Rows).

% row_generate_nextpossible(Row, _, [Row]) :- row_done(Row).
row_generate_nextpossible(Row, Nums, Rows) :- 
    nth0(I, Row, '?'),!,
    list_change_element(Row, I, '.', Row1),
    list_change_element(Row, I, '#', Row2),
    findall(R, (member(R,[Row1,Row2]), possible_row(R,Nums)), Rows).

rows_generate_nextpossible(Rows, Nums, PossibleRows) :-
    map3(row_generate_nextpossible, Rows, Nums, PossibleRows1),
    flatten1(PossibleRows1, PossibleRows).

rows_generate_possible(Rows, _, Rows) :-
    rows_done(Rows), !.
rows_generate_possible(Rows, Nums, PossibleRows) :-
    rows_generate_nextpossible(Rows, Nums, PossibleRows1),
    % write(PossibleRows1), nl,
    rows_generate_possible(PossibleRows1, Nums, PossibleRows).

record_count_possible([Row, Nums], Count) :-
    rows_generate_possible([Row], Nums, PossibleRows),
    length(PossibleRows, Count).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    maplist(parse_line, Lines, Records),
    % write(Records), nl,
    maplist(record_count_possible, Records, Counts),
    write(Counts), nl,
    sumlist(Counts, Sum),
    write(Sum), nl,
    nl.
