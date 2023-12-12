:- initialization main, halt.
:- consult("../common.pro").

no_empty_string([], []).
no_empty_string([H|T], [H|T1]) :-
    H \= "", no_empty_string(T, T1).
no_empty_string([H|T], T1) :-
    H = "", no_empty_string(T, T1).

parse_line(Str, Numbers) :-
    split_string(Str, ":", "", [_, Str0]),
    split_string(Str0, " ", "", Strs1),
    join_strings(Strs1, Str2),
    number_string(Number,Str2),
    Numbers = [Number].

quadratic_roots(A, B, C, X1, X2) :-
    D is B*B - 4*A*C, D >= 0,
    X1 is (-B - sqrt(D)) / (2*A),
    X2 is (-B + sqrt(D)) / (2*A).

hold_time_range(T, Drec, Thold1, Thold2) :-
    quadratic_roots(1, -T, Drec, Thold1f, Thold2f),
    Thold1 is floor(Thold1f+1), Thold2 is ceiling(Thold2f-1).

count_ways([T, Drec], N) :-
    hold_time_range(T, Drec, Thold1, Thold2),
    N is Thold2 - Thold1 + 1.

% total race time is T, record distance is Drec
% if the button is hold for Thold, the speed will be V = Thold * (1 mm/ms/ms)
% the time left is Tleft = T - Thold
% in that time, the boat will travel by Dleft = Tleft * V = Tleft * Thold * (1 mm/ms/ms)
% the record will be beaten if Dleft > Drec
% : Tleft * Thold * (1 mm/ms/ms) > Drec
% : (T - Thold) * Thold > Drec
% : Thold^2 - T*Thold + Drec < 0
% : X_1 <= Thold <= X_2

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    [TimeStr, DistStr] = Lines,
    parse_line(TimeStr, Times),
    parse_line(DistStr, Dists),
    zip(Times, Dists, Races),
    map(count_ways, Races, Ns),
    prod_list(Ns, N),
    write(Times),nl,
    write(Ns),nl,
    write(N),nl,
    nl.
