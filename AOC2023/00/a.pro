:- initialization main, halt.
:- consult("../common.pro").

main :-
    read_file_lines('./inpex.txt', Lines),
    write(Lines), nl.
