:- initialization main, halt.
:- consult("../common.pro").

parse_network(Line, [Name,Left,Right]) :-
    split_string(Line, "=", " ", [Name, RestStr]),
    string_chars(RestStr, RestChars),
    tail(RestChars, RTail), head(RTail, RightChars),
    string_chars(RightStr, RightChars),
    split_string(RightStr, ",", " ", [Left, Right]).

move(Current, [Dir, NetworkAssoc], Next) :- 
    get_assoc(Current, NetworkAssoc, [Left, Right]),
    (Dir = 'L' -> Next = Left; Next = Right).

networklist_pair([Name,Left,Right], Name-[Left,Right]).

is_start(Name) :- string_chars(Name, [_,_,'A']).
is_end(Name)   :- string_chars(Name, [_,_,'Z']).

% find_start(NetworkList, Starts)
find_start([], []).
find_start([[Name|_]|Ns], [Name|Starts]) :- is_start(Name), find_start(Ns, Starts).
find_start([_|Ns], Starts) :- find_start(Ns, Starts).

% move_steps_collect_ends(Steps, NetworkAssoc, Pos, Count, LastCount, Ends) :-
move_steps_collect_ends(_, _, _, Count, Count, []).
move_steps_collect_ends([S|Ss], NetworkAssoc, Pos, Count, LastCount, Ends) :-
    cycle_list([S|Ss], CSteps),
    move(Pos, [S, NetworkAssoc], NPos),
    NCount is Count + 1,
    (is_end(NPos) -> Ends = [NCount-NPos|NEnds]; Ends = NEnds),
    % write(NCount), write(" : "),
    % write(NPos), nl,
    move_steps_collect_ends(CSteps, NetworkAssoc, NPos, NCount, LastCount, NEnds).
move_steps_collect_ends_mappable(Pos, [Steps, NetworkAssoc, LastCount], Ends) :-
    move_steps_collect_ends(Steps, NetworkAssoc, Pos, 0, LastCount, Ends).

main :-
    % read_file_lines('./inpex3.txt', Lines),
    read_file_lines('./input.txt', Lines),
    Lines = [StepsStr, _ |NetworkStr],
    string_chars(StepsStr, Steps),
    map(parse_network, NetworkStr, NetworkList),
    length(NetworkList, Len),
    write(Len), nl,
    Lenn is Len * 100,
    map(networklist_pair, NetworkList, NetworkPairs),
    %
    list_to_assoc(NetworkPairs, NetworkAssoc),
    find_start(NetworkList, Starts),
    write(Starts), nl,
    %
    map3(move_steps_collect_ends_mappable, Starts, [Steps, NetworkAssoc, Lenn], EndsList),
    % nth0(1, Starts, Start0),
    % move_steps_collect_ends(Steps, NetworkAssoc, Start0, 0, Len, Ends),
    write("-----------"), nl,
    write_list(EndsList), 
    write("-----------"), nl,
    %
    % write(NetworkList), nl,
    % move_steps_untilEnd(Steps, NetworkAssoc, Starts, Count),
    % write(LastPosList), nl,
    % write(Count),
    nl.
