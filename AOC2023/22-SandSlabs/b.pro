:- initialization main, halt.
:- consult("../common.pro").


down([X,Y,Z], [X,Y,Z1]) :- Z1 is Z-1.
downN(N, [X,Y,Z], [X,Y,Z1]) :- Z1 is Z-N.
up([X,Y,Z], [X,Y,Z1]) :- Z1 is Z+1.

parse_line(Line, Range):-
    split_string(Line, "~", "", [LStr, RStr]),
    split_string(LStr, ",", "", LListStr),
    maplist(number_string, LList,  LListStr),
    split_string(RStr, ",", "", RListStr),
    maplist(number_string, RList, RListStr),
    Range = LList-RList.

store_position(Name, Pos, PosAssoc, NewPosAssoc) :-
    \+ (get_assoc(Pos, PosAssoc, _), write("ERROR: "), write(Pos)),
    put_assoc(Pos, PosAssoc, Name, NewPosAssoc).
remove_position(Name, Pos, PosAssoc, NewPosAssoc) :-
    del_assoc(Pos, PosAssoc, Name, NewPosAssoc).

store_range(Name, Range, PosAssoc-BlockAssoc, NewPosAssoc-NewBlockAssoc) :-
    Range = [X1,Y1,Z1]-[X2,Y2,Z2],
    findall([X,Y,Z], (between(X1,X2,X), between(Y1,Y2,Y), between(Z1,Z2,Z)), PosList),
    put_assoc(Name, BlockAssoc, PosList, NewBlockAssoc),
    foldl(store_position(Name), PosList, PosAssoc, NewPosAssoc).
store_range(Name-Range, PosAssoc-BlockAssoc, NewPosAssoc-NewBlockAssoc) :-
    store_range(Name, Range, PosAssoc-BlockAssoc, NewPosAssoc-NewBlockAssoc).

% ---------- dropping -----------------------------

is_blocked_atN(Name, PosAssoc-BlockAssoc, N) :-
    get_assoc(Name, BlockAssoc, PosList),
    % ∃ Pos ∈ PosList, s.t. 
    % {[down(Pos) ∈ PosAssoc and down(Pos) ∉ PosList] or [down(Pos).z = 0]}
    between(1,10000,N),
    member(Pos, PosList),
    downN(N, Pos, PosDown),
    (get_assoc(PosDown, PosAssoc, Name2), Name \= Name2; PosDown = [_,_,0]),!.

drop_blockN(N, Name, PosAssoc-BlockAssoc, NewPosAssoc-NewBlockAssoc) :-
    get_assoc(Name, BlockAssoc, PosList),
    maplist(downN(N), PosList, PosListDown),
    put_assoc(Name, BlockAssoc, PosListDown, NewBlockAssoc),
    foldl(remove_position(Name), PosList, PosAssoc, PosAssoc2),
    foldl(store_position(Name), PosListDown, PosAssoc2, NewPosAssoc).

keep_dropping(Names, PosAssoc-BlockAssoc, NewPosAssoc-NewBlockAssoc, Acc, Count) :-
    member(Name, Names),
    % if there exists Name that can fall, do it an repeat
    is_blocked_atN(Name, PosAssoc-BlockAssoc, Np1), Np1 > 1,!, 
    N is Np1-1,
    write("Dropping "), write(Name), nl,
    drop_blockN(N, Name, PosAssoc-BlockAssoc, NextPosAssoc-NextBlockAssoc),
    NAcc is Acc+1,
    keep_dropping(Names, NextPosAssoc-NextBlockAssoc, NewPosAssoc-NewBlockAssoc, NAcc, Count).
    % otherwise, stop
keep_dropping(_, PosAssoc-BlockAssoc, PosAssoc-BlockAssoc, Count, Count).

% ----------- check for support

cannot_be_disintregated(Names, PosAssoc-BlockAssoc, Name) :-
    get_assoc(Name, BlockAssoc, PosList),
    foldl(remove_position(Name), PosList, PosAssoc, PosAssocRemoved),!,
    % there exist a block that can fall
    member(Nam, Names), Nam \= Name,
    is_blocked_atN(Nam, PosAssocRemoved-BlockAssoc, Np1), Np1 > 1,!.

count_after_disintegrate(Names, PosAssoc-BlockAssoc, Name, Count) :-
    get_assoc(Name, BlockAssoc, PosList),
    foldl(remove_position(Name), PosList, PosAssoc, PosAssocRemoved),!,
    del_assoc(Name, BlockAssoc, _, BlockAssocRemoved),
    keep_dropping(Names, PosAssocRemoved-BlockAssocRemoved, _, 0, Count).

% ----------

% comparez(C, [_,_,Z1], [_,_,Z2]) :- compare(C, Z1, Z2).
comparerangez(<, _-[_,_,Z1], _-[_,_,Z2]) :- compare(=, Z1, Z2).
comparerangez(C, _-[_,_,Z1], _-[_,_,Z2]) :- compare(C, Z1, Z2).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    % write(Lines), nl,
    maplist(parse_line, Lines, Ranges0),
    write(Ranges0), nl,
    predsort(comparerangez, Ranges0, Ranges),
    write(Ranges), nl,
    % length(Ranges, NBlock),
    enumerate(Ranges, EnumRanges),
    maplist([X,Y]>>(X=Y-_), EnumRanges, Names),
    write(EnumRanges), nl,
    write(Names), nl,
    foldl(store_range, EnumRanges, t-t, PosAssoc-BlockAssoc),
    keep_dropping(Names, PosAssoc-BlockAssoc, FinalPosAssoc-FinalBlockAssoc, 0, _),
    maplist(count_after_disintegrate(Names, FinalPosAssoc-FinalBlockAssoc), Names, Counts),
    write(Counts), nl,
    sum_list(Counts, Sum),
    write(Sum), nl.
