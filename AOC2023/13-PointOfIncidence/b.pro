:- initialization main, halt.
:- consult("../common.pro").

% pattern_VreflectionId(Pattern, Id) :-
%     length(Pattern, Len), Lm1 is Len - 1,
%     between(1, Lm1, Id),
%     take(Id, Pattern, Up),
%     drop(Id, Pattern, Down),
%     reverse(Up, RUp),
%     (prefix(RUp, Down); prefix(Down, RUp)), !.
% pattern_VreflectionId(_, 0).
%
% pattern_HreflectionId(Pattern, Id) :-
%     transpose(Pattern, TPattern),
%     pattern_VreflectionId(TPattern, Id).

count_diff_row([], _, Acc, Acc).
count_diff_row(_, [], Acc, Acc).
count_diff_row([X|As], [X|Bs], Acc, Count) :-
    count_diff_row(As, Bs, Acc, Count).
count_diff_row([X|As], [Y|Bs], Acc, Count) :-
    X \= Y, NewAcc is Acc + 1,
    count_diff_row(As, Bs, NewAcc, Count).

% count_diff([X|Xs], [Y|Ys], Acc, Count) :-
count_diff(_, [], Acc, Acc).
count_diff([], _, Acc, Acc).
count_diff(_, _, Acc, Acc) :- Acc > 1,!. % short-circuit (we don't care if count > 1 anyway)
count_diff([X|Xs], [Y|Ys], Acc, Count) :-
    count_diff_row(X,Y, 0, CountRow),
    NewAcc is Acc + CountRow,
    count_diff(Xs, Ys, NewAcc, Count).
count_diff(A,B, Count) :- count_diff(A,B, 0, Count).

pattern_VreflectionId(Pattern, Id) :-
    length(Pattern, Len), Lm1 is Len - 1,
    between(1, Lm1, Id),
    take(Id, Pattern, Up),
    drop(Id, Pattern, Down),
    reverse(Up, RUp),
    % count_diff(RUp, Down, 0), !. % 0 is for exact match (like in part1)
    count_diff(RUp, Down, 1), !.   % 1 mean there is a single difference

pattern_VreflectionId(_, 0).
pattern_HreflectionId(Pattern, Id) :-
    transpose(Pattern, TPattern),
    pattern_VreflectionId(TPattern, Id).

value(Pattern, Value) :-
    pattern_VreflectionId(Pattern, VId),
    pattern_HreflectionId(Pattern, HId),
    Value is HId + 100*VId.

main :-
    % ---------- parsing ----------
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    split_by('', Lines, PatternsStr),
    maplist([X,Y]>>(maplist(string_chars,X,Y)), PatternsStr, Patterns),
    % ---------- parsing ----------
    maplist(value, Patterns, Values),
    write(Values), nl,
    sum_list(Values, Sum),
    write(Sum),
    % debug
    % nth0(0, Patterns, P),
    % (pattern_VreflectionId(P, 4) -> write('yes'); write('no')),
    % pattern_VreflectionId(P, VId),
    % pattern_HreflectionId(P, HId),
    % write(VId), nl,
    % write(HId), nl,
    % debug
    nl.
