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

% x_R + vx_R*ti = xi + vxi*ti
% ...
% x + u*t_i = x_i + v_i*t_i
write_numdata_eqstring(Num-Data) :-
    Data = (X,Y,Z)-(VX,VY,VZ),
    format("x + u*t~w - ~w - ~w*t~w\n", [Num, X, VX, Num]),
    format("y + v*t~w - ~w - ~w*t~w\n", [Num, Y, VY, Num]),
    format("z + w*t~w - ~w - ~w*t~w\n", [Num, Z, VZ, Num]).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines), 
    take(3, Lines, Lines3),
    maplist(parse_line, Lines3, Data),
    enumerate(Data, EnumData),
    maplist(write_numdata_eqstring, EnumData).
