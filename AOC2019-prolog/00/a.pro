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

average(List, Average) :-
    sum_list(List, Sum),
    length(List, Length),
    Average is Sum / Length.

main :-
    read_numbers_from_file('./inpex.txt', Numbers),
    average(Numbers, Average),
    write(Numbers), nl,
    write(Average), nl.
