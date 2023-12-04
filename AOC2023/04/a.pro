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

parse_line(Line, [WinningNumbers, SelfNumbers]) :-
    split_string(Line, ":", " ", [_,NumberStr]),
    split_string(NumberStr, "|", "", [WNumbersStr, SNumbersStr]),
    normalize_space(string(WNumbersStrNormal), WNumbersStr),
    normalize_space(string(SNumbersStrNormal), SNumbersStr),
    split_string(WNumbersStrNormal, " ", "", WNumbersStrList),
    split_string(SNumbersStrNormal, " ", "", SNumbersStrList),
    map(number_string, WinningNumbers, WNumbersStrList),
    map(number_string, SelfNumbers, SNumbersStrList).

count_matches([], _, 0).
count_matches([A|As], Bs, Count) :-
    member(A, Bs),
    count_matches(As, Bs, Count1),
    Count is Count1 + 1.
count_matches([A|As], Bs, Count) :-
    \+ member(A, Bs),
    count_matches(As, Bs, Count).

count_point([Winnings, SelfNumbers], Point) :-
    count_matches(SelfNumbers, Winnings, Count),
    (Count = 0 -> Point = 0; Point is 2^(Count-1)).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    map(parse_line, Lines, ParsedList),
    map(count_point, ParsedList, PointsList),
    sum_list(PointsList, Sum),
    % write(ParsedList), nl,
    % write(PointsList), nl,
    write(Sum),
    nl.
