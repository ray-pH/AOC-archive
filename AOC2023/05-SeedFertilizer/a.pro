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
map3(_, [], _, []).
map3(Pred, [A|As], B, [C|Cs]) :-
    call(Pred, A, B, C),
    map3(Pred, As, B, Cs).

split_by(Elem, List, [List]) :-
    \+ member(Elem, List).
split_by(Elem, List, [LArr|RestArr]) :-
    append(LArr, [Elem|Rest], List),
    split_by(Elem, Rest, RestArr).

parse_space_separated(Str, Parsed) :-
    split_string(Str, ' ', '', SeparatedStr),
    map(number_string, Parsed, SeparatedStr).

parse_seeds(SeedsStr, Seeds):-
    nth0(0, SeedsStr, SeedsStr0),
    string_concat('seeds: ', SeedsStr1, SeedsStr0),
    parse_space_separated(SeedsStr1, Seeds).

parse_map(MapStrArr, Map) :-
    MapStrArr = [_|MapStrArr0],
    map(parse_space_separated, MapStrArr0, Map).

parse_lines(Lines, Seeds, Maps) :-
    split_by('', Lines, Groups),
    Groups = [SeedsStr | MapsStr],
    parse_seeds(SeedsStr, Seeds),
    map(parse_map, MapsStr, Maps).

% MapLine : [Dst,Src,L]
inside_map(Value, MapLine) :-
    MapLine = [_,Src,L],
    Srcmax is Src + L - 1,
    Src =< Value, Value =< Srcmax.

% apply_map(Value, Map, NewValue)
apply_map(Value, [], Value).
apply_map(Value, [M|RestMap], NewValue) :-
    \+ inside_map(Value, M),
    apply_map(Value, RestMap, NewValue).
apply_map(Value, [M|_], NewValue) :-
    inside_map(Value, M),
    M = [Dst,Src,_],
    NewValue is Dst + (Value - Src).

apply_maps(Value, [], Value).
apply_maps(Value, [M|RestMaps], NewValue) :-
    apply_map(Value, M, NewValue0),
    apply_maps(NewValue0, RestMaps, NewValue).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), 
    parse_lines(Lines, Seeds, Maps),
    map3(apply_maps, Seeds, Maps, Results),
    min_list(Results, Min),
    % write(Maps), nl,
    write(Seeds), nl,
    write(Results), nl,
    write(Min),
    nl.
