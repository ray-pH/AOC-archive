:- initialization main, halt.
:- consult("../common.pro").

char_hash(Char, CurrentValue, NewValue) :-
    char_code(Char, Code),
    V1 is CurrentValue + Code,
    V2 is V1 * 17,
    NewValue is V2 mod 256.

chars_hash([], CurrentValue, CurrentValue).
chars_hash([Char|Chars], CurrentValue, NewValue) :-
    char_hash(Char, CurrentValue, V),
    chars_hash(Chars, V, NewValue).

string_hash(Str, Hash) :-
    string_chars(Str, Chars),
    chars_hash(Chars, 0, Hash).

% chars_op([C|Cs], LabelAcc, Op) :-
chars_op(['-'|_], Label, '-'-Label-0) :- !.
chars_op(['='|Cs], Label, '='-Label-FocalLength) :-
    number_chars(FocalLength, Cs), !.
chars_op([C|Cs], LabelAcc, Op) :-
    append(LabelAcc, [C], NLabelAcc),
    chars_op(Cs, NLabelAcc, Op), !.

string_op(String, Op) :-
    string_chars(String, Chars),
    chars_op(Chars, [], O-LabelChars-FocalLength),
    string_chars(Label, LabelChars),
    Op = O-Label-FocalLength.
% op_hashedop(Op, HashedOp) :-
%     Op = O-Label-FocalLength,
%     string_hash(Label, LabelHash),
%     HashedOp = O-Label-LabelHash-FocalLength.

% box_removelabel(Box, Label, NewBox) :-
box_removelabel([], _, []).
box_removelabel([Label-_|Bs], Label, Bs).
box_removelabel([L-FL|Bs], Label, [L-FL|RestBox]) :-
    L \= Label, box_removelabel(Bs, Label, RestBox).

% box_addlens(Box, Lens, NewBox)
box_addlens([], Lens, [Lens]).
box_addlens([Label-_|Bs], Label-NewFocalLength, [Label-NewFocalLength|Bs]).
box_addlens([L-FL|Bs], Label-FocalLength, [L-FL|RestBox]) :-
    L \= Label, box_addlens(Bs, Label-FocalLength, RestBox).

single_op(Boxes, '='-Label-FocalLength, NewBoxes) :-
    string_hash(Label, Hash),
    (get_assoc(Hash, Boxes, Box); Box = []),
    box_addlens(Box, Label-FocalLength, NewBox),
    put_assoc(Hash, Boxes, NewBox, NewBoxes).
single_op(Boxes, '-'-Label-_, NewBoxes) :-
    string_hash(Label, Hash),
    get_assoc(Hash, Boxes, Box),
    box_removelabel(Box, Label, NewBox),
    put_assoc(Hash, Boxes, NewBox, NewBoxes).
single_op(Boxes, _, Boxes).

% do_ops(Ops, Boxes, FinalBoxes)
do_ops([], Boxes, Boxes).
do_ops([Op|Ops], Boxes, FinalBoxes) :-
    % write(Op-Boxes), nl,
    single_op(Boxes, Op, NewBoxes),
    % write(NewBoxes), nl,
    do_ops(Ops, NewBoxes, FinalBoxes).

idbox_calc_power(Id-Box, Power) :-
    enumerate1(Box, EnBox),
    maplist([I,O]>>(I=IdSlot-(_-FocalLength), O is FocalLength * IdSlot), EnBox, Powers),
    sum_list(Powers, Power0),
    Power is (Id+1) * Power0.

boxes_calc_power(Boxes, Power) :-
    assoc_to_list(Boxes, BoxList),
    maplist(idbox_calc_power, BoxList, Powers),
    sum_list(Powers, Power).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    nth0(0, Lines, Line),
    split_string(Line, ",", "", Strs),
    maplist(string_op, Strs, Ops),
    % write(Ops), nl,
    empty_assoc(Boxes),
    do_ops(Ops, Boxes, FinalBoxes),
    % write(FinalBoxes), nl,
    boxes_calc_power(FinalBoxes, Power), !,
    write(Power),
    nl.
