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

% chars_firstDigit([C|Cs], Digit):-
%     (char_type(C,digit) -> 
%         C = Digit
%     ;
%         chars_firstDigit(Cs, Digit)
%     ).
chars_firstDigit([Digit|_], Digit):-
    char_type(Digit,digit).
chars_firstDigit([_|Cs], Digit):-
    chars_firstDigit(Cs, Digit).

chars_lastDigit(CList, Digit):-
    reverse(CList, ReversedList),
    chars_firstDigit(ReversedList, Digit).

chars_getvalue_number(CList, Number):-
    chars_firstDigit(CList, A),
    chars_lastDigit(CList, B),
    string_chars(NumStr, [A,B]),
    number_string(Number, NumStr).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    map(string_chars, Lines, LinesAsChars),
    map(chars_getvalue_number, LinesAsChars, Numbers),
    sum_list(Numbers, Sum),
    % write(Numbers), nl.
    write(Sum), nl.
