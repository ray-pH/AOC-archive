:- initialization main, halt.
:- consult("../common.pro").

border_direction_char('R', '-').
border_direction_char('D', '|').
border_direction_char('L', '-').
border_direction_char('U', '|').
border_direction_char('R', 'D', '7').
border_direction_char('D', 'L', 'J').
border_direction_char('L', 'U', 'L').
border_direction_char('U', 'R', 'F').
border_direction_char('R', 'U', 'J').
border_direction_char('U', 'L', '7').
border_direction_char('L', 'D', 'F').
border_direction_char('D', 'R', 'L').

% pos -> bottom,right
% neg -> top,left

flip(pos, neg).
flip(neg, pos).
%  .         .    Ex : F-J if the "hugs" is initially 'U','L',
%   F---------J        the final is still 'U','L'
%    .         .
%
%        Cor  Conn Cor' Init Final
transfer('F', '|', 'L', V-H, Vp-H) :- flip(V, Vp).
transfer('L', '|', 'F', V-H, Vp-H) :- flip(V, Vp).
transfer('7', '|', 'J', V-H, Vp-H) :- flip(V, Vp).
transfer('J', '|', '7', V-H, Vp-H) :- flip(V, Vp).
transfer( _,  '|',  _,  V-H, V-H ).

transfer('F', '-', '7', V-H, V-Hp) :- flip(H, Hp).
transfer('7', '-', 'F', V-H, V-Hp) :- flip(H, Hp).
transfer('L', '-', 'J', V-H, V-Hp) :- flip(H, Hp).
transfer('J', '-', 'L', V-H, V-Hp) :- flip(H, Hp).
transfer( _,  '-',  _,  V-H, V-H ).

corner_region_offset('F', pos, pos-pos).
corner_region_offset('F', neg, neg-neg).
corner_region_offset('L', pos, neg-pos).
corner_region_offset('L', neg, pos-neg).
corner_region_offset('7', pos, pos-neg).
corner_region_offset('7', neg, neg-pos).
corner_region_offset('J', pos, neg-neg).
corner_region_offset('J', neg, pos-pos).

offset_num(pos, 0.5).
offset_num(neg, -0.5).
offset_vector(V-H, Vector) :-
    offset_num(V, OffsetV),
    offset_num(H, OffsetH),
    Vector = (OffsetV, OffsetH).

direction_vector('R', (0, 1)).
direction_vector('D', (1, 0)).
direction_vector('L', (0, -1)).
direction_vector('U', (-1, 0)).
add_vector((X1, Y1), (X2, Y2), (X3, Y3)) :- X3 is X1 + X2, Y3 is Y1 + Y2.
% c = a + m*b
add_vector_scaled((X1, Y1), (X2, Y2), M, (X3, Y3)) :- X3 is X1 + X2 * M, Y3 is Y1 + Y2 * M.

string_inner(Str, Inner) :-
    string_chars(Str, Chars),
    append([['('], InnerChars, [')']], Chars),
    string_chars(Inner, InnerChars).

parse_line(Line, Parsed) :-
    split_string(Line, " ", "", [DirStr, NumStr, ColorInParen]),
    number_string(Num, NumStr),
    string_inner(ColorInParen, Color),
    string_chars(DirStr, [Dir]),
    Parsed = Dir-Num-Color.

% Plan : list of Dir-Num-Color
% CornerList : list of (X, Y)-Char
% gen_border(Plan, CurrentPos, BorderList)
gen_corners([], _, []).
gen_corners([_], _, []).
gen_corners([Pl1,Pl2|Pls], CurrentPos, CornerList):-
    Pl1 = Dir1-Num1-_,
    Pl2 = Dir2-_-_,
    direction_vector(Dir1, Vector1),
    add_vector_scaled(CurrentPos, Vector1, Num1, CornerPos),
    border_direction_char(Dir1, Dir2, CharCorner),
    Corner = CornerPos-CharCorner,
    CornerList = [Corner|NextCornerList],
    gen_corners([Pl2|Pls], CornerPos, NextCornerList).
gen_corners(Plan, CornerList) :-
    Plan = [P|_],
    append(Plan, [P], PlanWithTail),
    gen_corners(PlanWithTail, (0,0), CornerList0),
    cycle_list(CornerList, CornerList0).

transfer_dir((R,_), (R,_), '-').
transfer_dir((_,C), (_,C), '|').

% gen_path_init(V-H, CornerList, PathVertexList) :-
gen_path_woffset(_, [], []).
gen_path_woffset(_, [_], []).
gen_path_woffset(V-H, [C1,C2|Cs], [Vertex|Vs]) :-
    C1 = Pos1-Char1,
    C2 = Pos2-Char2,
    offset_vector(V-H, OffsetVector),
    add_vector(Pos1, OffsetVector, Vertex),
    transfer_dir(Pos1, Pos2, TransferChar),
    transfer(Char1, TransferChar, Char2, V-H, NextV-NextH),
    gen_path_woffset(NextV-NextH, [C2|Cs], Vs).

gen_path(CornerList, PosPathVertexList, NegPathVertexList) :-
    CornerList = [C|_],
    append(CornerList, [C], CornerListWithTail),
    C = _-Char,
    corner_region_offset(Char, pos, PosOffset),
    corner_region_offset(Char, neg, NegOffset),
    gen_path_woffset(PosOffset, CornerListWithTail, PosPathVertexList),!,
    gen_path_woffset(NegOffset, CornerListWithTail, NegPathVertexList),!.

% Green's Theorem : Area = ∫∫dA = ∮xdy
% f_calc_area(VertexList, Area)
calc_area([], Acc, Acc).
calc_area([_], Acc, Acc).
calc_area([V1,V2|Vs], Acc, Area) :-
    V1 = (X, Y1),
    V2 = (X, Y2),
    ThisInt is X * (Y2 - Y1),
    NextAcc is Acc + ThisInt,
    calc_area([V2|Vs], NextAcc, Area).
calc_area([V1,V2|Vs], Acc, Area) :-
    V1 = (_, Y),
    V2 = (_, Y),
    % the area is 0 (dy = 0)
    calc_area([V2|Vs], Acc, Area).

calc_area(VertexList, Area) :-
    VertexList = [V|_],
    append(VertexList, [V], VertexListWithTail),
    calc_area(VertexListWithTail, 0, SignedArea),
    Area is abs(SignedArea).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(parse_line, Lines, Plan),
    % write(Plan), nl,
    gen_corners(Plan, CornerList),
    % write(CornerList), nl,
    gen_path(CornerList, PosPathVertexList, NegPathVertexList),
    % write(PosPathVertexList), nl,
    % write(NegPathVertexList), nl,
    calc_area(PosPathVertexList, PosArea),
    calc_area(NegPathVertexList, NegArea),
    % write(PosArea), nl,
    % write(NegArea), nl,
    OuterArea is max(PosArea, NegArea),
    write(OuterArea),
    nl.

