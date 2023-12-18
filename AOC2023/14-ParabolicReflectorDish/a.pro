:- initialization main, halt.
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
    init(NewRowT, NewRow).
row_slideLeft(Row, NewRow) :-
    reverse(Row, RowReversed),
    row_slideRight(RowReversed, NewRowReversed),
    reverse(NewRowReversed, NewRow).

map_slideUp(Map, NewMap) :-
    transpose(Map, MapT),
    maplist(row_slideLeft, MapT, NewMapT),
    transpose(NewMapT, NewMap).

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
    % write_list(Map), nl,
    map_slideUp(Map, NewMap),
    % write_list(NewMap), nl,
    map_calc_load(NewMap, Load),
    write(Load), nl,
    %
    nl.
