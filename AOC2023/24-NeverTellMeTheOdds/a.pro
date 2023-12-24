:- initialization main, halt.
:- consult("../common.pro").

% ignore the z axis
parse_line(Line, Pos-Vel) :-
    % 19, 13, 30 @ -2,  1, -2
    split_string(Line, "@", " ", [PosStr, VelStr]),
    split_string(PosStr, ",", " ", PosStrs),
    maplist(number_string, [X,Y,_], PosStrs),
    split_string(VelStr, ",", " ", VelStrs),
    maplist(number_string, [VX,VY,_], VelStrs),
    Pos = (X,Y), Vel = (VX,VY).

% X = X0 + VX*t { t = (X - X0)/VX }
% Y = Y0 + VY*t
% -> (X - X0)/VX = (Y - Y0)/VY
% -> Y = Y0 + VY/VX * (X - X0) 
% :: M = VY/VX
% -> Y = Y0 + M * (X - X0) = Y0 + M*X - M*X0
% -> Y = M*X + (Y0 - M*X0)
% :: C = Y0 - M*X0
% -> Y = M*X + C { M = VY/VX, C = Y0 - M*X0 }
%
% given two data Y = M1*X + C1, Y = M2*X + C2
% calculate the intersection point
% M1*X + C1 = M2*X + C2
% (M1 - M2)*X = C2 - C1
% X = (C2 - C1)/(M1 - M2)
% Y = M1*X + C1
% t = (X - X01)/VX1

posvel_coeffs(Pos-Vel, Pos-Vel-M-C) :-
    Pos = (X0,Y0), Vel = (VX,VY),
    M is VY/VX, C is Y0 - M*X0.
coeffs_intersection(_-_-M1-_, _-_-M2-_, X-Y-T1-T2) :-
    M1 = M2,!, X = inf, Y = inf, T1 = -inf, T2 = -inf.
coeffs_intersection(P1-V1-M1-C1, P2-V2-M2-C2, X-Y-T1-T2) :-
    M1 \= M2,
    X is (C2 - C1)/(M1 - M2),
    Y is M1*X + C1,
    P1 = (X01,_), V1 = (VX1,_), T1 is (X - X01)/VX1,
    P2 = (X02,_), V2 = (VX2,_), T2 is (X - X02)/VX2.

compare_data(C, D1, D2) :- 
    D1 = P1-V1-_-_, D2 = P2-V2-_-_, 
    P1 = (X1,Y1), P2 = (X2,Y2),
    V1 = (VX1,VY1), V2 = (VX2,VY2),
    compare(C, [X1,Y1,VX1,VY1], [X2,Y2,VX2,VY2]).

betweenf(Min, Max, X) :- X >= Min, X =< Max.
get_crossing(Area, CoeffDatas, Crossings) :-
    Area = (Min, Max),
    findall(D1-D2-X-Y-T1-T2, (
        member(D1, CoeffDatas), member(D2, CoeffDatas), 
        compare_data(<, D1, D2),
        % D1 \= D2,
        coeffs_intersection(D1, D2, X-Y-T1-T2),
        T1 >= 0, T2 >= 0, betweenf(Min, Max, X), betweenf(Min, Max, Y)
    ), Crossings).

main :-
    % read_file_lines('./inpex.txt', Lines), Area = (7,27),
    read_file_lines('./input.txt', Lines), Area = (200000000000000,400000000000000),
    maplist(parse_line, Lines, Data),
    write(Data), nl,
    maplist(posvel_coeffs, Data, Coeffs),
    write(Coeffs), nl,
    get_crossing(Area, Coeffs, Crossings),
    length(Crossings, Len),
    write(Len),
    nl.
