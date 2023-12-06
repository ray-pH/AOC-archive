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

zip([], _, []).
zip(_, [], []).
zip([A|As], [B|Bs], [[A,B]|Cs]) :-
    zip(As, Bs, Cs).

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
