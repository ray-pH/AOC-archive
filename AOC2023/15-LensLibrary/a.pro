:- initialization main, halt.
:- consult("../common.pro").

char_hash(Char, CurrentValue, NewValue) :-
    char_code(Char, Code),
    V1 is CurrentValue + Code,
    V2 is V1 * 17,
    NewValue is V2 mod 256.

chars_hash([], CurrentValue, CurrentValue).
chars_hash([Char|Chars], CurrentValue, NewValue) :-
    char_hash(Char, CurrentValue, V),
    chars_hash(Chars, V, NewValue).

string_hash(Str, Hash) :-
    string_chars(Str, Chars),
    chars_hash(Chars, 0, Hash).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    nth0(0, Lines, Line),
    split_string(Line, ",", "", Strs),
    maplist(string_hash, Strs, Hashes),
    sum_list(Hashes, Sum),
    % write(Strs), nl,
    % write(Hashes), nl,
    write(Sum), nl,
    nl.
