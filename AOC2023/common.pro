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

maplist_debug(_, [], []).
maplist_debug(Pred, [A|As], [B|Bs]) :-
    call(Pred, A, B),
    % debug
    length(As, Len), write("---------------------"), 
    write("Remaining: "), writeln(Len),
    write(A), nl, write(" -> "), writeln(B),
    % debug
    maplist_debug(Pred, As, Bs).
map3_debug(_, [], _, []).
map3_debug(Pred, [A|As], B, [C|Cs]) :-
    call(Pred, A, B, C),
    % debug
    length(As, Len), write("---------------------"), 
    write("Remaining: "), writeln(Len),
    write(A), nl, write(" -> "), writeln(C),
    % debug
    map3_debug(Pred, As, B, Cs).

map3(_, [], _, []).
map3(Pred, [A|As], B, [C|Cs]) :-
    call(Pred, A, B, C),
    map3(Pred, As, B, Cs).

zip([], _, []).
zip(_, [], []).
zip([A|As], [B|Bs], [[A,B]|Cs]) :-
    zip(As, Bs, Cs).

tail([_|Tail], Tail).

init([_],[]).
init([I|Is], [I|RestInit]) :- init(Is, RestInit).

count(Elem, List, Count) :-
    count(Elem, List, 0, Count).
count(_, [], Count, Count).
count(Elem, [Elem|T], Count, Result) :-
    Count1 is Count + 1,
    count(Elem, T, Count1, Result).
count(Elem, [H|T], Count, Result) :-
    Elem \= H,
    count(Elem, T, Count, Result).

take(0, _, []) :- !.
take(N, [H|T1], [H|T2]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T1, T2).

drop(0, List, List) :- !.
drop(N, [_|T1], T2) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T1, T2).

list_fromrepeat(_, 0, []) :- !.
list_fromrepeat(Elem, N, [Elem|RestList]) :-
    N > 0,
    N1 is N - 1,
    list_fromrepeat(Elem, N1, RestList).
list_fromrepeat(_, _, []) :- !.

% join_string(Strings, Delimiter, Joined)
join_strings([], _, "") :- !.
join_strings([S], _, S) :- !.
join_strings([S1, S2|Ss], Delimiter, Joined) :-
    string_concat(S1, Delimiter, S1D),
    string_concat(S1D, S2, S1DS2),
    join_strings([S1DS2|Ss], Delimiter, Joined).

% cycle_list(Original, Cycled)
cycle_list([H|Hs], Cycled) :- append(Hs, [H], Cycled).

prod_list([], 1).
prod_list([A|As], Prod) :-
    prod_list(As, ProdRest),
    Prod is A * ProdRest.

split_by(Elem, List, [List]) :-
    \+ member(Elem, List).
split_by(Elem, List, [LArr|RestArr]) :-
    append(LArr, [Elem|Rest], List),
    split_by(Elem, Rest, RestArr).

flatten1([], []).
flatten1([[]|Cs], Flattened) :-
    flatten1(Cs, Flattened).
flatten1([C|Cs], Flattened) :-
    flatten1(Cs, FlattenedRest),
    append(C, FlattenedRest, Flattened).

join_strings([], "").
join_strings([S], S).
join_strings([S1, S2|Ss], Joined) :-
    string_concat(S1, S2, S1S2),
    join_strings([S1S2|Ss], Joined).

gcd(X, 0, X) :- !.
gcd(X, Y, Z) :-
    H is X rem Y,
    gcd(Y, H, Z).
lcm(X,Y,LCM):-
    gcd(X,Y,GCD),
    LCM is X*Y//GCD.

gcd_of_list([], 0) :- !.
gcd_of_list([X|Xs], GCD) :- gcd_of_list(Xs, GCD2), gcd(X, GCD2, GCD). 
lcm_of_list([],1) :- !.
lcm_of_list([X|Xs],LCM) :- lcm_of_list(Xs,LCM2), lcm(X,LCM2,LCM).

write_list([]).
write_list([X|Xs]) :- write(X), nl, write_list(Xs).

list_change_element(List, Index, NewElement, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElement, Rest).
list2D_change_element(List2D, [Row, Col], NewElement, NewList2D) :-
    nth0(Row, List2D, RowList, Rest),
    nth0(Col, RowList, _, RestRow),
    nth0(Row, NewList2D, NewRowList, Rest),
    nth0(Col, NewRowList, NewElement, RestRow).

transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Col|Transposed]) :-
    transpose_1st_col(Matrix, Col, RestMatrix),
    transpose(RestMatrix, Transposed),!.
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_1st_col(Rows, Hs, Ts).


enumerate(List, Enumerated) :-
    enumerate(List, 0, Enumerated).
enumerate1(List, Enumerated) :-
    enumerate(List, 1, Enumerated).
enumerate([], _, []).
enumerate([H|T], N, [N-H|Rest]) :-
    N1 is N + 1,
    enumerate(T, N1, Rest).
