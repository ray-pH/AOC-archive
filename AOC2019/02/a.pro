:- initialization main, halt.

read_numbers_from_file(File, Numbers) :-
    open(File, read, Stream),
    read_line_to_codes(Stream, Line),
    close(Stream),
    atom_codes(Atom, Line),
    atomic_list_concat(StrNumbers, ',', Atom),
    maplist(atom_number, StrNumbers, Numbers).

% run_program(Program, Ptr) :-
%     nth0(Ptr, Program, 99), !.
% run_program(Program, Ptr) :-
%     nth0(Ptr, Program, Opcode),
% replace_ith(List, Index, NewElement, NewList)
replace_ith([_|Xs], 0, NewElement, [NewElement|Xs]).
replace_ith([X|Xs], Index, NewElement, [X|NewLists]) :-
    Index > 0,
    NextIndex is Index - 1,
    replace_ith(Xs, NextIndex, NewElement, NewLists).

op_sum(Program, NewProgram, PtrOp1, PtrOp2, PtrTarget) :-
    nth0(PtrOp1, Program, Op1),
    nth0(PtrOp2, Program, Op2),
    Result is Op1 + Op2,
    replace_ith(Program, PtrTarget, Result, NewProgram).
op_mul(Program, NewProgram, PtrOp1, PtrOp2, PtrTarget) :-
    nth0(PtrOp1, Program, Op1),
    nth0(PtrOp2, Program, Op2),
    Result is Op1 * Op2,
    replace_ith(Program, PtrTarget, Result, NewProgram).

run_program(Program, NewProgram, Ptr) :-
    nth0(Ptr, Program, OpCode),
    OpCode =:= 1 ->


main :-
    read_numbers_from_file('./inpex.txt', Program),
    op_sum(Program, NewProgram, 9, 10, 3),
    op_mul(NewProgram, NewNProgram, 3, 11, 0),
    write(NewNProgram), nl.
