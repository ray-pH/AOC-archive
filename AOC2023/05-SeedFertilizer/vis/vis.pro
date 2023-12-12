:- initialization main, halt.
:- consult("../../common.pro").

range(A,B, List) :- findall(X, between(A,B,X), List).
range_fromlength(A, Len, List) :- B is A + Len - 1, range(A, B, List).

parse_space_separated(Str, Parsed) :-
    split_string(Str, ' ', '', SeparatedStr),
    map(number_string, Parsed, SeparatedStr).

parse_seeds(SeedsStr, Seeds):-
    nth0(0, SeedsStr, SeedsStr0),
    string_concat('seeds: ', SeedsStr1, SeedsStr0),
    parse_space_separated(SeedsStr1, Seeds).

flip_AB([A,B|Rest], [B,A|Rest]).
parse_map(MapStrArr, MapSorted) :-
    MapStrArr = [_|MapStrArr0],
    map(parse_space_separated, MapStrArr0, Map),
    map(flip_AB, Map, MapFlipped),
    sort(MapFlipped, MapSorted).

parse_lines(Lines, Seeds, Maps) :-
    split_by('', Lines, Groups),
    Groups = [SeedsStr | MapsStr],
    parse_seeds(SeedsStr, Seeds),
    map(parse_map, MapsStr, Maps).

% MapLine : [Src,Dst,L]
inside_map(Value, MapLine) :-
    MapLine = [Src,_,L],
    Srcmax is Src + L - 1,
    Src =< Value, Value =< Srcmax.

% map_splitpoint(Map, SplitPoint) :-
map_splitpoints([], []).
map_splitpoints([M|RestMap], [SplitPoints|RestSplitPoints]) :-
    M = [SplitPoints,_,_],
    map_splitpoints(RestMap, RestSplitPoints).

% range [SP1,SP2), [SP2,SP3)
% split_range(Range, SplitPoints, Ranges) :-
split_range([A,B], [], [[A,B]]).
split_range([A,B], [SP|_], Ranges) :-
    B < SP, 
    Ranges = [[A,B]].
split_range([A,B], [SP|SPs], Ranges) :-
    A < SP, B >= SP, 
    Ranges = [[A,SPm1]|NextRanges], SPm1 is SP - 1,
    split_range([SP,B], SPs, NextRanges).
split_range([A,B], [SP|SPs], Ranges) :-
    A >= SP,
    split_range([A,B], SPs, Ranges).

combine_fromranges([R], [R]).
combine_fromranges([R1,R2|Rs], Ranges) :-
    R1 = [A1, B1], R2 = [A2, B2],
    B1p1 is B1 + 1,
    (B1p1 = A2 ->
        combine_fromranges([[A1,B2]|Rs], Ranges)
    ;
        Ranges = [R1|RestRanges],
        combine_fromranges([R2|Rs], RestRanges)
    ).

split_ranges(Ranges, SP, RangesSplitted) :-
    map3(split_range, Ranges, SP, RangesSplittedNonflat),
    flatten1(RangesSplittedNonflat, RangesSplitted).

% only for single range that is already been splitted
% apply_map_range(Range, Map, Ranges)
apply_map_range(Range, [], [Range]).
apply_map_range([A,B], [M|RestMap], Ranges) :-
    \+ inside_map(A, M),
    apply_map_range([A,B], RestMap, Ranges).
apply_map_range([A,B], [M|_], Ranges) :-
    inside_map(A, M),
    M = [Src,Dst,_],
    NewA is Dst + (A - Src),
    NewB is Dst + (B - Src),
    Ranges = [[NewA,NewB]].

apply_map_ranges(Ranges, Map, MappedRanges) :-
    map_splitpoints(Map, SplitPoints),
    split_ranges(Ranges, SplitPoints, SplittedRanges),
    map3(apply_map_range, SplittedRanges, Map, MappedRangesNonFlatten),
    flatten1(MappedRangesNonFlatten, MappedRangesNonSorted),
    sort(MappedRangesNonSorted, MappedRangesNonCombined),
    combine_fromranges(MappedRangesNonCombined, MappedRanges),
    write(Ranges), nl,
    write(SplittedRanges), nl,
    write(MappedRangesNonSorted), nl,
    write(MappedRangesNonCombined), nl,
    write(MappedRanges), nl, nl.

apply_maps_ranges(Ranges, [], Ranges).
apply_maps_ranges(Ranges, [M|RestMaps], NewRanges) :-
    apply_map_ranges(Ranges, M, NewRanges0),
    apply_maps_ranges(NewRanges0, RestMaps, NewRanges).

seed_ranges([A,L], Ranges) :-
    B is A + L - 1, Ranges = [A,B].
seeds_ranges([], []).
seeds_ranges([_], []).
seeds_ranges([S1,L1|Rest], Ranges) :-
    Ranges = [R1|RestRanges],
    seed_ranges([S1,L1], R1),
    seeds_ranges(Rest, RestRanges).

% apply_map(Value, Map, NewValue)
apply_map(Value, [], Value).
apply_map(Value, [M|RestMap], NewValue) :-
    \+ inside_map(Value, M),
    apply_map(Value, RestMap, NewValue).
apply_map(Value, [M|_], NewValue) :-
    inside_map(Value, M),
    M = [Src,Dst,_],
    NewValue is Dst + (Value - Src).

apply_maps(Value, [], Value).
apply_maps(Value, [M|RestMaps], NewValue) :-
    apply_map(Value, M, NewValue0),
    write([NewValue0,Value]), write(','),
    apply_maps(NewValue0, RestMaps, NewValue).
reverse_map(Map, ReversedMap) :-
    map(flip_AB, Map, ReversedMap).
reverse_maps(Maps, ReversedMaps) :-
    reverse(Maps, RMaps),
    map(reverse_map, RMaps, ReversedMaps).

main :-
    read_file_lines('../inpex.txt', Lines),
    % read_file_lines('../input.txt', Lines),
    parse_lines(Lines, Seeds, Maps),
    seeds_ranges(Seeds, SeedRanges),
    % write(SeedRanges), nl,
    apply_maps_ranges(SeedRanges, Maps, Ranges),
    write('::'), nl,
    flatten1(Ranges,Ranges0),
    nth0(0,Ranges0,Min),
    reverse_maps(Maps, ReversedMaps),
    apply_maps(Min, ReversedMaps, OriginalMin),
    % write(OriginalMin), nl,
    % write(Ho),
    nl.
