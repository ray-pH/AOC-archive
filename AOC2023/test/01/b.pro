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


% digit_name(Digit, StringName)
digit_name('1', "one").
digit_name('2', "two").
digit_name('3', "three").
digit_name('4', "four").
digit_name('5', "five").
digit_name('6', "six").
digit_name('7', "seven").
digit_name('8', "eight").
digit_name('9', "nine").
digit_name_chars(Digit, NameChars):-
    digit_name(Digit, Name),
    string_chars(Name, NameChars).

chars_firstDigit([Digit|_], Digit):-
    char_type(Digit,digit).
chars_firstDigit(CList, Digit):-
    digit_name_chars(Digit, NameChars),
    prefix(NameChars, CList).
chars_firstDigit([_|Cs], Digit):-
    chars_firstDigit(Cs, Digit).

chars_lastDigit(CList, Digit):-
    reverse(CList, ReversedList),
    chars_lastDigitRev(ReversedList, Digit).

chars_lastDigitRev([Digit|_], Digit):-
    char_type(Digit,digit).
chars_lastDigitRev(CList, Digit):-
    digit_name_chars(Digit, NameChars),
    reverse(NameChars, ReversedNameChars),
    prefix(ReversedNameChars, CList).
chars_lastDigitRev([_|Cs], Digit):-
    chars_lastDigitRev(Cs, Digit).

chars_getvalue_number(CList, Number):-
    chars_firstDigit(CList, A),
    chars_lastDigit(CList, B),
    string_chars(NumStr, [A,B]),
    number_string(Number, NumStr).

main :-
    read_file_lines('./inpwol.txt', Lines),
    % read_file_lines('./inpex2.txt', Lines),
    % read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    map(string_chars, Lines, LinesAsChars),
    map(chars_getvalue_number, LinesAsChars, Numbers),
    write(Numbers), nl,
    sum_list(Numbers, Sum),
    % write(Numbers), nl.
    write(Sum), nl.
