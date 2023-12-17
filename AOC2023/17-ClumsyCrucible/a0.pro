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
% validdir(CurrentPos, Dir, Map, NextPos) :-
%     direction_vector(Dir, Vector),
%     add_vectors(Pos, Vector, NextPos),!,
%     validpos(NextPos, Map).
% validhead(Pos-Dir-Count-Dist, Map) :-
    % Count < 4, validpos(Pos, Map).
% valid_poscount(Pos, Count, Map) :-
    % validpos(Pos, Map), Count < 4.
nextdir_nexthead(NextDir, Pos-Dir-Count-Dist, Map, NextHead) :-
    % write("nextdir_nexthead: "),nl,
    % write(NextDir-Pos-Dir-Count-Dist), nl,
    (NextDir = Dir, Count < 3, NextCount is Count + 1
    ; NextDir \= Dir, NextCount = 1),
    % write(NextCount), nl,
    direction_vector(NextDir, Vector),!,
    % write(NextDir-Vector), nl,
    add_vectors(Pos, Vector, NextPos),
    % write("added"), nl,
    % write(NextPos), nl,
    validpos(NextPos, Map),
    element_at(NextPos, Map, NextHeat),
    NextDist is Dist + NextHeat,
    NextHead = NextPos-NextDir-NextCount-NextDist.

% head_nextHeads(Pos-Dir-Count-Dist, Map, NextHeads) :-
head_nextHeads(Head, Map, NextHeads) :-
    % write("head_nextHeads: "),nl,
    % write(Head), nl,
    direction_next(Dir, NextDirs),
    findall(NHead, (member(NextDir, NextDirs), nextdir_nexthead(NextDir, Head, Map, NHead)), NextHeads).
    % write(NextHeads),nl.

head_updateDistance(Head, DistanceMap, NewDistanceMap) :-
    % write("head_updateDistance: "),nl,
    % write(Head), nl,
    Head = Pos-_-_-Dist,
    element_at(Pos, DistanceMap, PrevDist),!,
    % write(Dist-PrevDist), nl,
    (
        Dist < PrevDist,
        Pos = (Row,Col),
        list2D_change_element(DistanceMap, [Row,Col], Dist, NewDistanceMap)
    ;
        PDM is PrevDist + 9*3,
        Dist =< PDM,
        NewDistanceMap = DistanceMap
    ).
    % write("changed"), nl.

heads_nextHeads(Heads, Map, NextHeads) :-
    map3(head_nextHeads, Heads, Map, HeadsNextHeads),
    flatten1(HeadsNextHeads, NextHeads).

% heads_updateDistance_nextheads([H|Hs], DistanceMap, NewDistanceMap, NewHeadsAcc, NewHeads) :-
heads_updateDistance_nextheads([], DistanceMapAcc, DistanceMapAcc, NewHeadsAcc, NewHeadsAcc).
heads_updateDistance_nextheads([H|Hs], DistanceMap, NewDistanceMap, NewHeadsAcc, NewHeads) :-
    head_updateDistance(H, DistanceMap, DistanceMap1),
    heads_updateDistance_nextheads(Hs, DistanceMap1, NewDistanceMap, [H|NewHeadsAcc], NewHeads).
heads_updateDistance_nextheads([H|Hs], DistanceMap, NewDistanceMap, NewHeadsAcc, NewHeads) :-
    \+ head_updateDistance(H, DistanceMap, _),
    heads_updateDistance_nextheads(Hs, DistanceMap, NewDistanceMap, NewHeadsAcc, NewHeads).

heads_updateDistance_nextheads(Heads, DistanceMap, NewDistanceMap, NewHeads) :-
    heads_updateDistance_nextheads(Heads, DistanceMap, NewDistanceMap, [], NewHeads).

% Head = Pos-Dir-Count-Dist
prune_sortedHeads([], []).
prune_sortedHeads([H], [H]).
prune_sortedHeads([H1,H2|Hs], Pruned) :-
    H1 = P-D-C-Dist1,
    H2 = P-D-C-Dist2,
    Dist1 =< Dist2,
    prune_sortedHeads([H1|Hs], Pruned).
prune_sortedHeads([H1,H2|Hs], Pruned) :-
    Pruned = [H1|NPruned],
    prune_sortedHeads([H2|Hs], NPruned).

% traverse(Heads, Map, DistanceMap, FinalDistanceMap)
traverse([], _, FinalDistanceMap, FinalDistanceMap).
traverse(Heads, Map, DistanceMap, FinalDistanceMap) :-
    write("traverse: "),nl,
    write(Heads), nl,
    heads_nextHeads(Heads, Map, NextHeads),
    write(NextHeads), nl,
    heads_updateDistance_nextheads(NextHeads, DistanceMap, NewDistanceMap, NewHeads),!,
    write(NewHeads), nl,
    sort(NewHeads, SortedHeads),
    prune_sortedHeads(SortedHeads, PrunedHeads),!,
    % write(NewDistanceMap), nl.
    traverse(PrunedHeads, Map, NewDistanceMap, FinalDistanceMap).
    % traverse(NewHeads, Map, NewDistanceMap, FinalDistanceMap).
traverse2(0, _, _, FinalDistanceMap, FinalDistanceMap).
traverse2(L, Heads, Map, DistanceMap, FinalDistanceMap) :-
    write("traverse2: "),nl,
    write(Heads), nl,
    heads_nextHeads(Heads, Map, NextHeads),
    write(NextHeads), nl,
    heads_updateDistance_nextheads(NextHeads, DistanceMap, NewDistanceMap, NewHeads),!,
    write(NewHeads), nl,
    % write(NewDistanceMap), nl.
    NL is L - 1,
    traverse2(NL, NewHeads, Map, NewDistanceMap, FinalDistanceMap).


% minimum_heat_lost(Pos-_-_, Map-Target, MinimumHeatLost) :-
%     Pos = Target, MinimumHeatLost = element_at(Pos, Map, HeatLost), !.
% minimum_heat_lost(Pos-LastDir-3, Map-Target, MinimumHeatLost) :-
%     element_at(Pos, Map, ThisHeatLost),
%     direction_leftright(LastDir, NextDirs),
%     findall(NextPos-Dir-1, (member(Dir, NextDirs), validdir(Pos,Dir,Map,NextPos)), NextPosDirs),!,
%     map3(minimum_heat_lost, NextPosDirs, Map-Target, NextHeatLosts),
%     min_list(NextHeatLosts, MinimumNextHeatLost),
%     MinimumHeatLost is ThisHeatLost + MinimumNextHeatLost.

% Head = Pos-Dir-Count-Dist
% update_distanceMap_single(Head, Map, DistanceMap, NewDistanceMap) :-
% update_distanceMap_single(_-_-4-_, _, _, DistanceMap, DistanceMap) :- !.
% update_distanceMap_single(Pos-_-_-_, _, Map, DistanceMap, DistanceMap) :- \+ validpos(Pos, Map), !.
% update_distanceMap_single(Pos-_-_-PrevDist, Map, DistanceMap, NewDistanceMap) :- 
%     validpos(Pos, Map),
%     element_at(Pos, Map, ThisHeat),
%     element_at(Pos, DistanceMap, ThisDist),
%     NewDist is ThisHeat + PrevDist,
%     (NewDist < ThisDist -> 
%         list2D_change_element(DistanceMap, Pos, NewDist, NewDistanceMap)
%     ;
%         NewDistanceMap = DistanceMap
%     ).


% update_distanceMap(Heads, Map, DistanceMap, NewDistanceMap) :-

init_distanceMap(Map, DistanceMap) :-
    length(Map, LRows),
    nth0(0, Map, Row0), length(Row0, LCols),
    maplist(sumlist, Map, RowSums),
    sumlist(RowSums, Total),
    list_fromrepeat(Total, LCols, RowRep),
    list_fromrepeat(RowRep, LRows, DistanceMap).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(parse_line, Lines, Map),
    write(Map), nl,
    init_distanceMap(Map, DistanceMap),
    write(DistanceMap), nl,
    % Head = Pos-Dir-Count-Dist
    InitHead = (0,0)-'>'-1-2,
    traverse([InitHead], Map, DistanceMap, FinalDistanceMap),
    % traverse2(4, [InitHead], Map, DistanceMap, FinalDistanceMap),
    write_list(FinalDistanceMap), nl,
    nl.
