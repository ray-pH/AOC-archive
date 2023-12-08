:- initialization main, halt.
:- consult("../common.pro").

parse_network(Line, [Name,Left,Right]) :-
    split_string(Line, "=", " ", [Name, RestStr]),
    string_chars(RestStr, RestChars),
    tail(RestChars, RTail), head(RTail, RightChars),
    string_chars(RightStr, RightChars),
    split_string(RightStr, ",", " ", [Left, Right]).

% move(Dir, Network, Current, Next)
% move(Dir, [[Name,_,_]|Ns], Current, Next) :-
%     Name \= Current, move(Dir, Ns, Current, Next).
move('L', [[Current,Left,_]|_], Current, Left).
move('R', [[Current,_,Right]|_], Current, Right).
move(Dir, [_|Ns], Current, Next) :- move(Dir, Ns, Current, Next).

% move_steps_untilZZZ(Steps, Newtork, [Pos, Count], [LastPos, LastCount])
move_steps_untilZZZ(_, _, ["ZZZ", Count], ["ZZZ", Count]).
move_steps_untilZZZ([S|Ss], Network, [Pos, Count], [LastPos, LastCount]) :-
    cycle_list([S|Ss], CSteps),
    move(S, Network, Pos, NPos),
    NCount is Count + 1,
    move_steps_untilZZZ(CSteps, Network, [NPos, NCount], [LastPos, LastCount]).

main :-
    % read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./inpex2.txt', Lines),
    read_file_lines('./input.txt', Lines),
    Lines = [StepsStr, _ |NetworkStr],
    string_chars(StepsStr, Steps),
    map(parse_network, NetworkStr, Network),
    move_steps_untilZZZ(Steps, Network, ["AAA", 0], [LastPos, Count]),
    % write(Steps), nl,
    % write(NetworkStr), nl,
    % write(Network), nl,
    write(LastPos), nl,
    write(Count),
    nl.
