:- initialization main, halt.
:- dynamic memo/2.
:- consult("../common.pro").

% ...#.o.#.oo..#
% -> [...], [.o.], [.oo..]
row_group_hash(Row, Grouped) :-
    string_chars(RowStr, Row),
    split_string(RowStr, "#", "", GroupedStr),
    maplist(string_chars, GroupedStr, Grouped).
% [...], [.o.], [.oo..]
% -> [...], [..o], [...oo]
group_slideRight(Group, SlidGroup) :-
    count('O', Group, OCount),
    length(Group, GroupLen),
    DotCount is GroupLen - OCount,
    list_fromrepeat('O', OCount, OList),
    list_fromrepeat('.', DotCount, DotList),
    append(DotList, OList, SlidGroup).
groups_slideRight(Groups, SlidGroups) :-
    maplist(group_slideRight, Groups, SlidGroups).

row_slideRight(Row, NewRow) :-
    append(Row, ['#'], RowAppended),
    row_group_hash(RowAppended, Groups),
    groups_slideRight(Groups, SlidGroups),
    maplist(string_chars, SlidGroupsStr, SlidGroups),
    join_strings(SlidGroupsStr, "#", NewRowStr),
    string_chars(NewRowStr, NewRowT),
    head(NewRowT, NewRow).
row_slideLeft(Row, NewRow) :-
    reverse(Row, RowReversed),
    row_slideRight(RowReversed, NewRowReversed),
    reverse(NewRowReversed, NewRow).

map_slideUp(Map, NewMap) :-
    transpose(Map, MapT),
    maplist(row_slideLeft, MapT, NewMapT),
    transpose(NewMapT, NewMap).
map_slideDown(Map, NewMap) :-
    transpose(Map, MapT),
    maplist(row_slideRight, MapT, NewMapT),
    transpose(NewMapT, NewMap).
map_slideRight(Map, NewMap) :-
    maplist(row_slideRight, Map, NewMap).
map_slideLeft(Map, NewMap) :-
    maplist(row_slideLeft, Map, NewMap).
map_slideCycle(Map, NewMap) :-
    map_slideUp(Map, Map1),
    map_slideLeft(Map1, Map2),
    map_slideDown(Map2, Map3),
    map_slideRight(Map3, NewMap).

map_iter_cycle(Map, 0, Map) :- !.
map_iter_cycle(Map, N, NewMap) :-
    % write(N), nl,
    map_slideCycle(Map, Map1),!, N1 is N - 1,
    map_iter_cycle(Map1, N1, NewMap).

map_findcycle(Map, Id, Id0-Id-Map) :-
    memo(Id0, Map), !.
map_findcycle(Map, Id, CycleInfo) :-
    assertz(memo(Id, Map)),
    % write(Id), nl,
    map_slideCycle(Map, NewMap),!,
    Id1 is Id + 1,
    map_findcycle(NewMap, Id1, CycleInfo),!.

row_calc_load(Multiplier-Row, Load) :-
    string_chars(Row, RowChars),
    count('O', RowChars, OCount),
    Load is OCount * Multiplier.

map_calc_load(Map, Load) :-
    reverse(Map, MapReversed),
    enumerate1(MapReversed, EnumMapReversed),
    maplist(row_calc_load, EnumMapReversed, Loads),
    sumlist(Loads, Load).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    map(string_chars, Lines, Map),
    map_findcycle(Map, 0, Id0-Id-MapCycle),!,
    write('Cycle found: '),
    write(Id0), write(' '), write(Id), nl,
    Target = 1000000000,
    A = Id0,
    B = Id,
    Base is B - A,
    % TargetM is ((Target - A) mod Base) + A,
    Ncycle is ((Target - A) mod Base),
    map_iter_cycle(MapCycle, Ncycle, TargetMap),
    map_calc_load(TargetMap, Load),
    write(Load), nl,

    % 1000000000
    % 3 - 10 - 17 - 24
    %  4    11   18   25
    %   5     12   19   26
    % v
    % 0999999997
    % 0 -  7 - 14 - 21
    %  1    8    15   22
    %   2    9    16   23
    nl.
