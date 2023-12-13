:- initialization main, halt.
:- consult("../common.pro").
:- use_module(library(sort)).

char_card('T', 10).
char_card('J', 11).
char_card('Q', 12).
char_card('K', 13).
char_card('A', 14).
char_card(X, N) :- number_chars(N, [X]).

parse_line(Line, [Hand, Bid]):-
    split_string(Line, " ", "", [HandStr, BidStr]),
    string_chars(HandStr, HandChars),
    map(char_card, HandChars, HandUnsorted),
    msort(HandUnsorted, Hand),
    number_string(Bid, BidStr).

% card_value(Hand, [Value, HandSummary])
card_value([A,A,A,A,A], [6,[A]]       ).
card_value([A,A,A,A,B], [5,[A,B]]     ).
card_value([A,B,B,B,B], [5,[B,A]]     ).
card_value([A,A,A,B,B], [4,[A,B]]     ).
card_value([A,A,B,B,B], [4,[B,A]]     ).
card_value([X,X,X,B,C], [3,[X,C,B]]   ).
card_value([B,X,X,X,C], [3,[X,C,B]]   ).
card_value([B,C,X,X,X], [3,[X,C,B]]   ).
card_value([A,A,B,B,C], [2,[B,A,C]]   ).
card_value([A,A,C,B,B], [2,[B,A,C]]   ).
card_value([C,A,A,B,B], [2,[B,A,C]]   ).
card_value([X,X,A,B,C], [1,[X,C,B,A]] ).
card_value([A,X,X,B,C], [1,[X,C,B,A]] ).
card_value([A,B,X,X,C], [1,[X,C,B,A]] ).
card_value([A,B,C,X,X], [1,[X,C,B,A]] ).
card_value(Hand, [0,Hand]).

player_value([Hand, Bid], [Value, Hand, HandSummary, Bid]) :-
    card_value(Hand, [Value, HandSummary]).

card_ranking(=, S, S).
card_ranking(>, [X,_], [Y,_]) :- X \= Y, X > Y.
card_ranking(<, [X,_], [Y,_]) :- X \= Y, X < Y.
card_ranking(>, [X,H1], [X,H2]) :- H1 @> H2.
card_ranking(<, [X,H1], [X,H2]) :- H1 @< H2.

% Player : [Value, Hand, HandSummary, Bid]
player_ranking(=, [V1,_,H1,_], [V2,_,H2,_]) :- card_ranking(=, [V1,H1], [V2,H2]).
player_ranking(>, [V1,_,H1,_], [V2,_,H2,_]) :- card_ranking(>, [V1,H1], [V2,H2]).
player_ranking(<, [V1,_,H1,_], [V2,_,H2,_]) :- card_ranking(<, [V1,H1], [V2,H2]).

% Order
% 6 Five of a kind
% 5 Four of a kind
% 4 Full house
% 3 Three of a kind
% 2 Two pair
% 1 One pair
% 0 High card
%
player_bid([_,_,_,Bid], Bid).

calc_winnings(Bids, W) :-
    length(Bids, L), f_calc_winnings(L, Bids, W).
f_calc_winnings(_, [], 0).
f_calc_winnings(I, [B|Bs], TotalW) :-
    I1 is I - 1,
    W is I * B,
    f_calc_winnings(I1, Bs, TW1),
    TotalW is W + TW1.

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    map(parse_line, Lines, Game),
    map(player_value, Game, Players),
    predsort(player_ranking, Players, PlayersSorted),
    map(player_bid, PlayersSorted, Bids),
    calc_winnings(Bids, W),
    % write(Players), nl,
    % write(Game), nl,
    % write(PlayersSorted), nl,
    write(Bids), nl,
    write(W),
    nl.
