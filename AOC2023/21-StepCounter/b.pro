:- initialization main, halt.
:- consult("../common.pro").
:- use_module(library(clpfd)).
:- dynamic visited/1.
:- dynamic quadratics/2.

vector_add((X1,Y1), (X2,Y2), (X3,Y3)) :-
    X3 is X1 + X2,
    Y3 is Y1 + Y2.

char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowStr),
    nth0(Col, RowStr, Char).
char_at_inf((Row,Col), Map, Char) :-
    length(Map, MapSize),
    R is Row mod MapSize,
    C is Col mod MapSize,
    char_at((R,C), Map, Char).

find_start(Map, StartPos) :-
    char_at(StartPos, Map, 'S').

step_single(Map, Pos, NewPosList) :-
    DirectionList = [(-1,0), (1,0), (0,-1), (0,1)],
    maplist(vector_add(Pos), DirectionList, NewPosList0),
    findall(P, (
        member(P, NewPosList0),
        char_at_inf(P, Map, C),
        member(C, ['.', 'S']),
        \+ visited(P)
    ), NewPosList).

step(Map, PosList, NewPosList) :-
    maplist(step_single(Map), PosList, NewPosList0),
    flatten1(NewPosList0, NewPosList1),
    unique(NewPosList1, NewPosList).

store_visited([]).
store_visited([H|T]) :- assertz(visited(H)), store_visited(T).
even(N) :- 0 is N mod 2.

traverse(328, _, _, EvenList-OddList, EvenList-OddList).
traverse(N, _, _, EvenListAcc-OddListAcc, _-_) :-
    member(X, [0,1,2]),
    x_step(X,N), 
    (even(N)->
        write(N), write(' '), write(EvenListAcc), nl,
        assertz(quadratics(X, EvenListAcc))
    ;
        write(N), write(' '), write(OddListAcc), nl,
        assertz(quadratics(X, OddListAcc))
    ),
    fail.
traverse(N, Map, PosList, EvenListAcc-OddListAcc, EvenList-OddList) :-
    step(Map, PosList, NewPosList),
    store_visited(NewPosList),
    % write(NewPosList), nl,
    N1 is N + 1,
    length(NewPosList, NewPosListLength),
    (even(N1) ->
        % NEvenListAcc = [NewPosList|EvenListAcc],
        % append(NewPosList, EvenListAcc, NEvenListAcc),
        NEvenListAcc is EvenListAcc + NewPosListLength,
        NOddListAcc = OddListAcc
    ;
        NEvenListAcc = EvenListAcc,
        NOddListAcc is OddListAcc + NewPosListLength
        % append(NewPosList, OddListAcc, NOddListAcc)
        % NOddListAcc = [NewPosList|OddListAcc]
    ),
    write(N), write(' '),
    write(NEvenListAcc-NOddListAcc), nl,
    traverse(N1, Map, NewPosList, NEvenListAcc-NOddListAcc, EvenList-OddList).
traverse(N, Map, PosList, EvenList-OddList) :-
    traverse(N, Map, PosList, 0-0, EvenList-OddList).


% Step = 65 + 131 * X
x_step(X, Step) :- Step #= 65 + 131 * X.
solve_polynomial(Result) :-
    quadratics(0,Y0),
    quadratics(1,Y1),
    quadratics(2,Y2),
    C = Y0,
    A is (Y2 - 2*Y1 + C) / 2,
    B is Y1 - A - C,
    TargetStep = 26501365,
    x_step(X, TargetStep),!,
    % write(A-B-C-X), nl,
    Result is A*X*X + B*X + C.

% A + B + C = Y1
% 4A + 2B + C = Y2
%
% 2A + 2B + 2C = 2Y1
% 4A + 2B + C = Y2
% 2A - C = Y2 - 2Y1
% A = (Y2 - 2Y1 + C) / 2
%
% B = Y1 - A - C

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    find_start(Map, StartPos),
    % write(StartPos), nl,
    % traverse(196, Map, [StartPos], EvenLength-OddLength),
    % traverse(327, Map, [StartPos], EvenLength-OddLength),
    % traverse(65, Map, [StartPos], EvenLength-OddLength),
    traverse(0, Map, [StartPos], EvenLength-OddLength),
    % n=0 :  3755
    % n=1 : 33494
    % n=2 : 92811
    % assertz(quadratics(0, 3755)),
    % assertz(quadratics(1, 33494)),
    % assertz(quadratics(2, 92811)),
    solve_polynomial(Result),
    write(Result), nl,
    % write(EvenList), nl,
    % write(EvenLength-OddLength), nl,
    nl.
