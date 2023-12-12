:- initialization main, halt.
:- consult("../common.pro").

parse_line(Line, CharArr-Nums) :-
    split_string(Line, " ", "", [RowStr, NumsStr]),
    string_chars(RowStr, CharArr),
    split_string(NumsStr, ",", "", NumArr),
    maplist(number_string, Nums, NumArr).

row_done(Row) :- \+ member('?', Row).
rows_not_done(Rows) :- member(Row, Rows), \+ row_done(Row), !.
rows_done(Rows) :- \+ rows_not_done(Rows).

row_trimfront([], []).
row_trimfront(['.'|Rs], TrimmedRow) :-
    row_trimfront(Rs, TrimmedRow).
row_trimfront([R|Rs], [R|Rs]) :- R \= '.'.

trow_frontNum(TRow, Num, false) :-
    row_done(TRow),
    \+ member('.', TRow), % no dot
    length(TRow, Num).
trow_frontNum(TRow, Num, false) :-
    row_done(TRow),!,
    nth0(Num, TRow, '.'),!.
trow_frontNum(TRow, Num, Partial) :-
    nth0(IDot, TRow, '.'),
    nth0(IQue, TRow, '?'),!,
    (IQue < IDot -> 
        Partial = true, Num = IQue
    ; 
        Partial = false, Num = IDot
    ).
% no dot:
trow_frontNum(TRow, Num, true) :-
    nth0(Num, TRow, '?').

row_generate_nextpos(Row, [Row]) :- 
    row_done(Row),!.
row_generate_nextpos(Row, [Row1, Row2]) :- 
    nth0(I, Row, '?'),!,
    list_change_element(Row, I, '.', Row1),
    list_change_element(Row, I, '#', Row2).

data_valid(M-[]-[], M-[]-[]). 
data_valid(M-R-[], M-[]-[]) :- \+ member('#', R).
data_valid(Data, NormalizedData) :-
    Data = M-Row-Nums,
    % write(Data), nl,
    Nums = [N|Ns],
    row_trimfront(Row, TRow),
    % write(TRow), nl,
    trow_frontNum(TRow, CNum, Partial),
    % write(CNum-Partial), nl,
    ((Partial = true -> 
        CNum =< N, 
        NormalizedData = M-TRow-Nums
    ;
        CNum = N,
        (nth0(IDot, TRow, '.'),!; length(TRow, IDot)),
        % write(IDot), nl,
        drop(IDot, TRow, TRow2),
        % write(TRow2), nl,
        row_trimfront(TRow2, TRow3),
        NormalizedData = M-TRow3-Ns
    ); NormalizedData = 0-[]-[]).
data_valid(_, 0-[]-[]).

row_data(Row, [M,Nums], Data) :- Data = M-Row-Nums.
data_nextdatas(Data, NextDatas) :-
    Data = M-Row-Nums,
    row_generate_nextpos(Row, NRows),
    map3(row_data, NRows, [M,Nums], NDatas),
    maplist(data_valid, NDatas, NextDatasMaybe),!,
    findall(ND, (member(ND, NextDatasMaybe), ND=M-_-_, M \= 0), NextDatas).

% combine_datas(Datas, CombinedDatas)
combine_datas([], []).
combine_datas([D], [D]).
combine_datas([D1, D2|Ds], [DComb|CDs]) :-
    D1 = M1-Row-Nums,
    D2 = M2-Row-Nums,
    MComb is M1 + M2,
    DComb = MComb-Row-Nums,
    combine_datas(Ds, CDs).
combine_datas([D1, D2|Ds], [D1|CDatas]) :-
    combine_datas([D2|Ds], CDatas).

% order_data(=, D, D).
order_data(<, _-R1-N1, _-R2-N2) :- compare(<, N1-R1, N2-R2).
order_data(>, _-R1-N1, _-R2-N2) :- compare(>, N1-R1, N2-R2).
order_data(>, _-R1-N1, _-R2-N2) :- compare(=, N1-R1, N2-R2).

datas_nextdatas(Datas, NextDatas) :-
    maplist(data_nextdatas, Datas, NextDatasArr),
    flatten1(NextDatasArr, NextDatasNoncombined),
    predsort(order_data, NextDatasNoncombined, NextDatasNoncombinedSorted),
    combine_datas(NextDatasNoncombinedSorted, NextDatas).

% Data (Multiplicity-Row-Nums)
record_trimfront(Row-Nums, TRow-Nums) :- row_trimfront(Row, TRow).

datas_count_possible([], -1).
datas_count_possible([Count-[]-[]], Count).
datas_count_possible(Datas, Count) :-
    datas_nextdatas(Datas, NextDatas),!,
    % write(NextDatas), nl,
    datas_count_possible(NextDatas, Count).

record_5record(Row-Nums, FRow-FNums) :-
    append([Row, ['?'], Row, ['?'], Row, ['?'], Row, ['?'], Row], FRow),
    append([Nums, Nums, Nums, Nums, Nums], FNums).

initialrecord_datas(Row-Nums, [Data]) :- Data = 1-Row-Nums.
main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    write(Lines), nl,
    maplist(parse_line, Lines, Records),!,
    write(Records), nl,
    maplist(record_5record, Records, FRecords),!,
    maplist(initialrecord_datas, FRecords, InitialDatas),!,
    write(InitialDatas), nl,
    maplist_debug(datas_count_possible, InitialDatas, Counts),!,
    sumlist(Counts, Sum),
    write(Counts), nl,
    write(Sum), nl,
    nl.
