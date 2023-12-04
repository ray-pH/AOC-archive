% Define the succ/2 predicate
succ(X, Y) :- X is Y + 1.

% Define a predicate to apply succ/2 to each element of a list
apply_succ_to_list([], []).
apply_succ_to_list([X | Rest], [Y | ResultRest]) :-
    succ(Y, X),
    apply_succ_to_list(Rest, ResultRest).

apply_operation_to_list(_, [], []).
apply_operation_to_list(Operation, [X | Rest], [Y | ResultRest]) :-
    call(Operation, Y, X),
    apply_operation_to_list(Operation, Rest, ResultRest).
