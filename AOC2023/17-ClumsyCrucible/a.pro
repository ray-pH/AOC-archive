:- initialization main, halt.
:- consult("../common.pro").
% :- dynamic memo_minimum_heat_lost/2.

parse_line(Line, Parsed) :-
    string_chars(Line, Chars),
    maplist(atom_number, Chars, Parsed).

direction_vector('>', (0, 1)).
direction_vector('<', (0, -1)).
direction_vector('^', (-1, 0)).
direction_vector('v', (1, 0)).
add_vectors((X1,Y1), (X2,Y2), (X3,Y3)) :- X3 is X1 + X2, Y3 is Y1 + Y2.
element_at((Row,Col), Map, Element) :- nth0(Row, Map, RowList), nth0(Col, RowList, Element).
% direction_leftright('>', ['^','v']).
% direction_leftright('<', ['v','^']).
% direction_leftright('^', ['<','>']).
% direction_leftright('v', ['>','<']).
direction_next('>', ['^','v','>']).
direction_next('<', ['v','^','<']).
direction_next('^', ['<','>','^']).
direction_next('v', ['>','<','v']).

validpos((Row,Col), Map) :-
    length(Map, LRows),
    nth0(0, Map, Row0), length(Row0, LCols),
    Row >= 0, Row < LRows,
    Col >= 0, Col < LCols.

init_distanceMap(Map, DistanceMap) :-
    length(Map, LRows),
    nth0(0, Map, Row0), length(Row0, LCols),
    maplist(sumlist, Map, RowSums),
    sumlist(RowSums, Total),
    list_fromrepeat(Total, LCols, RowRep),
    list_fromrepeat(RowRep, LRows, DistanceMap).

dir_validhead(Dir, PrevHead, Map, DistanceMap, ValidHead) :-
    PrevHead = PrevPos-PrevDir-PrevCount-PrevDist,
    (Dir = PrevDir -> Count is PrevCount + 1; Count = 1),
    Count =< 3,!,
    direction_vector(Dir, V),
    add_vectors(V, PrevPos, Pos),
    validpos(Pos, Map),!,
    element_at(Pos, Map, Heat),
    Dist is PrevDist + Heat,
    element_at(Pos, DistanceMap, MapDist),
    Dist < MapDist + 18,!,
    ValidHead = Pos-Dir-Count-Dist.

distancemap_update_single(Head, DistanceMap, NewDistanceMap) :-
    Head = Pos-_-_-Dist,
    element_at(Pos, DistanceMap, MapDist),
    (Dist < MapDist ->
        Pos = (Row, Col),
        list2D_change_element(DistanceMap, [Row, Col], Dist, NewDistanceMap)
        % set_element_at(Pos, DistanceMap, Dist, NewDistanceMap)
        % list2D_change_element(List2D, [Row, Col], NewElement, NewList2D) :-
    ;
        NewDistanceMap = DistanceMap).
distancemap_update([], DistanceMap, DistanceMap).
distancemap_update([Head|Heads], DistanceMap, NewDistanceMap) :-
    distancemap_update_single(Head, DistanceMap, NewDistanceMap1),
    distancemap_update(Heads, NewDistanceMap1, NewDistanceMap).

sortedheads_prune([], []).
sortedheads_prune([H], [H]).
sortedheads_prune([H1,H2|T], T2) :-
    H1 = P-D-C-Dist1,
    H2 = P-D-C-Dist2,
    Dist1 < Dist2,
    heads_prune([H1|T], T2).
sortedheads_prune([H1,H2|T], [H1|T2]) :-
    heads_prune([H2|T], T2).

heads_prune(Heads, Pruned) :-
    sort(Heads, Sorted),
    sortedheads_prune(Sorted, Pruned).

head_next_heads(Head, [Map, DistanceMap], NextHeads) :-
    Head = _-Dir-_-_,
    direction_next(Dir, NextDirs),
    findall(NextHead, (member(NDir, NextDirs), dir_validhead(NDir, Head, Map, DistanceMap, NextHead)), NextHeads).

heads_next_heads(Heads, Map, DistanceMap, NextHeads) :-
    map3(head_next_heads, Heads, [Map, DistanceMap], NextHeadsList),
    flatten1(NextHeadsList, NextHeads).

% traverse(Heads, Map, DistMap)
traverse(Heads, _, Map, _, MinDist) :-
    length(Map, L), Lm1 is L - 1,
    TargetPos = (Lm1, Lm1),
    member(H, Heads), H = TargetPos-_-_-MinDist, 
    write("done"), write(H), nl, !.
traverse(Heads, I, Map, DistMap, MinDist) :-
    heads_prune(Heads, PrunedHeads),
    findall(H, (member(H,PrunedHeads), H = _-_-_-I), ThisTimeHeads),!,
    %
    length(PrunedHeads, L),
    length(ThisTimeHeads, L2),
    write(I-L-L2), nl,
    %
    heads_next_heads(ThisTimeHeads, Map, DistMap, NextHeads),
    distancemap_update(NextHeads, DistMap, NewDistMap),
    findall(H, (member(H,Heads), \+member(H,ThisTimeHeads)) ,Heads2),!,
    append(Heads2, NextHeads, NewHeads),
    I1 is I + 1,
    traverse(NewHeads, I1, Map, NewDistMap, MinDist).

% Head = Pos-Dir-Count-Dist
main :-
    read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./input.txt', Lines),
    maplist(parse_line, Lines, Map),
    write(Map), nl,
    init_distanceMap(Map, DistanceMap),
    write(DistanceMap), nl,
    Head = Pos-Dir-Count-Dist,
    InitHead = (0,0)-'>'-0-0,
    traverse([InitHead], 0, Map, DistanceMap, MinDist),
    write(MinDist), nl,
    % traverse([InitHead], Map, DistanceMap, FinalDistanceMap),
    % traverse2(4, [InitHead], Map, DistanceMap, FinalDistanceMap),
    % write_list(FinalDistanceMap), nl,
    nl.
