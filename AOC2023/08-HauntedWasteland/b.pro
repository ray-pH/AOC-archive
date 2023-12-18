:- initialization main, halt.
:- consult("../common.pro").

parse_network(Line, [Name,Left,Right]) :-
    split_string(Line, "=", " ", [Name, RestStr]),
    string_chars(RestStr, RestChars),
    tail(RestChars, RTail), init(RTail, RightChars),
    string_chars(RightStr, RightChars),
    split_string(RightStr, ",", " ", [Left, Right]).

move(Current, [Dir, NetworkAssoc], Next) :- 
    get_assoc(Current, NetworkAssoc, [Left, Right]),
    (Dir = 'L' -> Next = Left; Next = Right).

networklist_pair([Name,Left,Right], Name-[Left,Right]).

is_start(Name) :- string_chars(Name, [_,_,'A']).
is_end(Name)   :- string_chars(Name, [_,_,'Z']).

find_start([], []).
find_start([[Name|_]|Ns], [Name|Starts]) :- is_start(Name), find_start(Ns, Starts).
find_start([_|Ns], Starts) :- find_start(Ns, Starts).

% move_steps_collect_ends(Steps, NetworkAssoc, Pos, Count, LastCount, Ends) :-
move_steps_collect_firstend(_, _, _, Count, Count, -1-"FAILED").
move_steps_collect_firstend([S|Ss], NetworkAssoc, Pos, Count, LastCount, End) :-
    cycle_list([S|Ss], CSteps),
    move(Pos, [S, NetworkAssoc], NPos),
    NCount is Count + 1,
    (is_end(NPos) -> 
        End = NCount-NPos
    ; 
        move_steps_collect_firstend(CSteps, NetworkAssoc, NPos, NCount, LastCount, End)
    ).
    % write(NCount), write(" : "),
    % write(NPos), nl,
move_steps_collect_end_mappable(Pos, [Steps, NetworkAssoc, LastCount], End) :-
    move_steps_collect_firstend(Steps, NetworkAssoc, Pos, 0, LastCount, End).

end_num(Count-_, Count).
main :-
    % read_file_lines('./inpex3.txt', Lines),
    read_file_lines('./input.txt', Lines),
    Lines = [StepsStr, _ |NetworkStr],
    string_chars(StepsStr, Steps),
    map(parse_network, NetworkStr, NetworkList),
    length(NetworkList, Len),
    write(Len), nl,
    Lenn is Len * Len,
    map(networklist_pair, NetworkList, NetworkPairs),
    %
    list_to_assoc(NetworkPairs, NetworkAssoc),
    find_start(NetworkList, Starts),
    write(Starts), nl,
    %
    map3(move_steps_collect_end_mappable, Starts, [Steps, NetworkAssoc, Lenn], EndsList),
    write("-----------"), nl,
    write(EndsList), nl,
    write("-----------"), nl,
    map(end_num, EndsList, EndNums),
    lcm_of_list(EndNums, LCM),
    write(LCM),
    nl.
