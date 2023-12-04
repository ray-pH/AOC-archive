:- initialization main, halt.

read_numbers_from_file(File, Numbers) :-
    open(File, read, Stream),
    read_numbers_from_stream(Stream, Numbers),
    close(Stream).

read_numbers_from_stream(Stream, Numbers) :-
    read_line_to_codes(Stream, Line),
    ( Line \= end_of_file ->
        atom_codes(Atom, Line),
        atom_number(Atom, Number),
        read_numbers_from_stream(Stream, Rest),
        Numbers = [Number | Rest]
    ; Numbers = []
    ).

fmodule(Fuel, Mass) :-
    Fuel is Mass // 3 - 2.

mapp(_, [], []).
mapp(Pred, [A|As], [B|Bs]) :-
    call(Pred, B, A),
    mapp(Pred, As, Bs).

main :-
    % read_numbers_from_file('./inpex.txt', Input),
    read_numbers_from_file('./input.txt', Input),
    mapp(fmodule, Input, Output),
    sum_list(Output, Sum),
    write(Sum), nl.
