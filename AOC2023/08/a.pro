:- initialization main, halt.
:- consult("../common.pro").

parse_network(Line, [Name,Left,Right]) :-
    split_string(Line, "=", " ", [Name, RestStr]),
    string_chars(RestStr, RestChars),
    tail(RestChars, RTail), head(RTail, RightChars),
    string_chars(RightStr, RightChars),
    split_string(RightStr, ",", " ", [Left, Right]).

move(Dir, NetworkAssoc, Current, Next) :- 
    get_assoc(Current, NetworkAssoc, [Left, Right]),
    (Dir = 'L' -> Next = Left; Next = Right).

% move_steps_untilZZZ(Steps, Newtork, [Pos, Count], [LastPos, LastCount])
move_steps_untilZZZ(_, _, ["ZZZ", Count], ["ZZZ", Count]).
move_steps_untilZZZ([S|Ss], NetworkAssoc, [Pos, Count], [LastPos, LastCount]) :-
    cycle_list([S|Ss], CSteps),
    move(S, NetworkAssoc, Pos, NPos),
    NCount is Count + 1,
    move_steps_untilZZZ(CSteps, NetworkAssoc, [NPos, NCount], [LastPos, LastCount]).

networklist_pair([Name,Left,Right], Name-[Left,Right]).

main :-
    % read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./inpex2.txt', Lines),
    read_file_lines('./input.txt', Lines),
    Lines = [StepsStr, _ |NetworkStr],
    string_chars(StepsStr, Steps),
    map(parse_network, NetworkStr, Network),
    map(networklist_pair, Network, NetworkPairs),
    list_to_assoc(NetworkPairs, NetworkAssoc),
    move_steps_untilZZZ(Steps, NetworkAssoc, ["AAA", 0], [LastPos, Count]),
    % write(Steps), nl,
    % write(NetworkStr), nl,
    % write(Network), nl,
    write(LastPos), nl,
    write(Count),
    nl.
