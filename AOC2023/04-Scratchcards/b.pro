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

list_from_repeat(0, _, []).
list_from_repeat(N, X, [X|Xs]) :-
    N > 0, N1 is N - 1, list_from_repeat(N1, X, Xs).

add_vector([], [], []).
add_vector([A|As], [B|Bs], [C|Cs]) :-
    C is A + B, add_vector(As, Bs, Cs).
scale_vector([], _, []).
scale_vector([A|As], B, [C|Cs]) :-
    C is A * B, scale_vector(As, B, Cs).

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

count_card_matches([WinningNumbers, SelfNumbers], Count) :-
    count_matches(SelfNumbers, WinningNumbers, Count).

matches_point(0, 0).
matches_point(Count, Point) :- Point is 2^(Count-1).

% [0,1,2,....,A,A+1,...,B-1,B,...,Length-2,Length-1]
% [0,0,0,....,1,1,...,1,1,...,0,0]
vector_range(A, B, Length, Vector) :-
    f_vector_range(A, B, Length, 0, Vector).

f_vector_range(_, _, Length, Length, []).
f_vector_range(A, B, Length, Id, [1|VectorRest]) :-
    A =< Id, Id =< B,
    Id1 is Id + 1, f_vector_range(A, B, Length, Id1, VectorRest).
f_vector_range(A, B, Length, Id, [0|VectorRest]) :-
    (Id < A; B < Id),
    Id1 is Id + 1, f_vector_range(A, B, Length, Id1, VectorRest).

any_one(_,1).
count_copies(MatchesCounts, CardCount) :-
    map(any_one, MatchesCounts, Ones),
    f_count_copies(0, MatchesCounts, Ones, CardCount).

f_count_copies(Id, MatchesCounts, CardCount, CardCount) :-
    length(MatchesCounts, Length),
    Id1 is Id + 1,
    Id1 >= Length.
    % Id1 >= 2.
f_count_copies(Id, MatchesCounts, CardCount, NewCardCount) :-
    length(MatchesCounts, Length),
    Id1 is Id + 1,
    nth0(Id, MatchesCounts, MCount),
    Id2 is Id + MCount,
    nth0(Id, CardCount, Count),
    vector_range(Id1, Id2, Length, Vector1),
    scale_vector(Vector1, Count, VectorScaled),
    add_vector(CardCount, VectorScaled, CardCount2),
    f_count_copies(Id1, MatchesCounts, CardCount2, NewCardCount).


main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    map(parse_line, Lines, ParsedList),
    map(count_card_matches, ParsedList, MatchesList),
    count_copies(MatchesList, CardCount),
    sum_list(CardCount, Sum),
    write(MatchesList), nl,
    write(CardCount), nl,
    write(Sum), nl,
    % write(ParsedList), nl,
    nl.
