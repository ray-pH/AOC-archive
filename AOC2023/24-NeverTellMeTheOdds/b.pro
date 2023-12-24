:- initialization main, halt.
:- consult("../common.pro").

parse_line(Line, Pos-Vel) :-
    % 19, 13, 30 @ -2,  1, -2
    split_string(Line, "@", " ", [PosStr, VelStr]),
    split_string(PosStr, ",", " ", PosStrs),
    maplist(number_string, [X,Y,Z], PosStrs),
    split_string(VelStr, ",", " ", VelStrs),
    maplist(number_string, [VX,VY,VZ], VelStrs),
    Pos = (X,Y,Z), Vel = (VX,VY,VZ).

cross(V1, V2, V3) :-
    V1 = (X1,Y1,Z1), V2 = (X2,Y2,Z2),
    V3X is Y1*Z2 - Z1*Y2,
    V3Y is Z1*X2 - X1*Z2,
    V3Z is X1*Y2 - Y1*X2,
    V3 = (V3X,V3Y,V3Z).
dot(V1, V2, R) :-
    V1 = (X1,Y1,Z1), V2 = (X2,Y2,Z2),
    R is X1*X2 + Y1*Y2 + Z1*Z2.
normalize(V, N) :-
    V = (X,Y,Z),
    Len is sqrt(X*X + Y*Y + Z*Z),
    NX is X / Len, NY is Y / Len, NZ is Z / Len,
    N = (NX,NY,NZ).
vector_sub(V1, V2, V3) :-
    V1 = (X1,Y1,Z1), V2 = (X2,Y2,Z2),
    V3X is X1-X2, V3Y is Y1-Y2, V3Z is Z1-Z2,
    V3 = (V3X,V3Y,V3Z).

% L = R + E*t : L,R,E is a 3D vector
% https://math.stackexchange.com/questions/2213165/find-shortest-distance-between-lines-in-3d
min_dist(L1, L2, MinDist) :-
    L1 = R1-V1, L2 = R2-V2,
    normalize(V1, E1), normalize(V2, E2),
    cross(E1, E2, N),
    (N = (0.0,0.0,0.0) ->
        % parallel
        MinDist = para
    ;
        vector_sub(R1, R2, R1mR2), % R1mR2 = R1-R2,
        dot(N, R1mR2, NdotR1mR2), % NdotR1mR2 = N \cdot R1mR2
        Top is abs(NdotR1mR2),
        dot(N, N, NdotN), % NdotN = N \cdot N
        Bot is sqrt(NdotN),
        MinDist is Top / Bot
    ).

is_intersect_or_parallel(L1, L2):-
    min_dist(L1, L2, MinDist),
    (MinDist = para; MinDist = 0).

% X = X0 + VX*t
% Y = Y0 + VY*t
% Z = Z0 + VZ*t
% check for intersection
%
% X0_A + VX_A*t = X0_B + VX_B*t

% is_parallel(Data1, Data2) :-
%     Data1 = _-(VX1,VY1,VZ1),
%     Data2 = _-(VX2,VY2,VZ2),
%     RX is VX1 / VX2,
%     RY is VY1 / VY2,
%     RZ is VZ1 / VZ2,
%     % parallel if all ratios are equal
%     RX = RY, RY = RZ.

compare_data(C, D1, D2) :- 
    D1 = P1-V1, D2 = P2-V2, 
    P1 = (X1,Y1,Z1), P2 = (X2,Y2,Z2),
    V1 = (VX1,VY1,VZ1), V2 = (VX2,VY2,VZ2),
    compare(C, [X1,Y1,Z1,VX1,VY1,VZ1], [X2,Y2,Z2,VX2,VY2,VZ2]).

get_planar(Datas, Planars) :-
    findall(D1-D2, (
        member(D1, Datas), member(D2, Datas), 
        compare_data(<,D1,D2), is_intersect_or_parallel(D1, D2)
    ), Planars).
    % findall(D1-D2, (member(D1, Datas), member(D2, Datas), D1\=D2, is_intersect_or_parallel(D1, D2)), Paralels).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines), 
    maplist(parse_line, Lines, Data),
    % write(Data), nl,
    get_planar(Data, Planars),
    write(Planars), nl,
    nl.
