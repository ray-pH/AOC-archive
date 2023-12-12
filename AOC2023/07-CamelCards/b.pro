:- initialization main, halt.
:- consult("../common.pro").
:- use_module(library(sort)).
:- use_module(library(lists)).

char_card('T', 10).
char_card('J', 1).
char_card('Q', 12).
char_card('K', 13).
char_card('A', 14).
char_card(X, N) :- number_chars(N, [X]).

parse_line(Line, [Hand, Bid]):-
    split_string(Line, " ", "", [HandStr, BidStr]),
    string_chars(HandStr, HandChars),
    map(char_card, HandChars, Hand),
    number_string(Bid, BidStr).

% card_value(Hand, Value)
card_value([X,X,X,X,X], 6).
card_value([X,X,X,X,_], 5).
card_value([_,X,X,X,X], 5).
card_value([A,A,A,B,B], 4).
card_value([A,A,B,B,B], 4).
card_value([X,X,X,_,_], 3).
card_value([_,X,X,X,_], 3).
card_value([_,_,X,X,X], 3).
card_value([A,A,B,B,_], 2).
card_value([A,A,_,B,B], 2).
card_value([_,A,A,B,B], 2).
card_value([X,X,_,_,_], 1).
card_value([_,X,X,_,_], 1).
card_value([_,_,X,X,_], 1).
card_value([_,_,_,X,X], 1).
card_value(_, 0).
% Order
% 6 Five of a kind
% 5 Four of a kind
% 4 Full house
% 3 Three of a kind
% 2 Two pair
% 1 One pair
% 0 High card
%

card_ranking(=, S, S).
card_ranking(>, [X,_], [Y,_]) :- X \= Y, X > Y.
card_ranking(<, [X,_], [Y,_]) :- X \= Y, X < Y.
card_ranking(>, [X,H1], [X,H2]) :- H1 @> H2.
card_ranking(<, [X,H1], [X,H2]) :- H1 @< H2.

contain_joker([]) :- false.
contain_joker([1|_]).
contain_joker([_|Xs]) :- contain_joker(Xs).

% change_joker(TargetVal, Hand, NewHand)
change_joker(_,[],[]).
change_joker(TVal, [1|Xs], [TVal|Ys]) :- change_joker(TVal, Xs, Ys).
change_joker(TVal, [X|Xs], [X|Ys]) :- change_joker(TVal, Xs, Ys).

player_value([Hand, Bid], [Value, Hand, Bid]) :-
    msort(Hand, SortedHand),
    card_value(SortedHand, Value).

% Player : [Value, Hand, Bid]
player_ranking(=, [V1,H1,_], [V2,H2,_]) :- card_ranking(=, [V1,H1], [V2,H2]).
player_ranking(>, [V1,H1,_], [V2,H2,_]) :- card_ranking(>, [V1,H1], [V2,H2]).
player_ranking(<, [V1,H1,_], [V2,H2,_]) :- card_ranking(<, [V1,H1], [V2,H2]).

pad_bid(H, [H,0]).
jokerhand_besthand(OriginalHand, [BestValue,BestHand]) :-
    Targets = [1,2,3,4,5,6,7,8,9,10,12,13,14],
    map3(change_joker, Targets, OriginalHand, Hands),
    map(pad_bid, Hands, Game),
    map(player_value, Game, Players),
    predsort(player_ranking, Players, PlayersSorted),
    last(PlayersSorted, BestPlayer),
    BestPlayer = [BestValue,BestHand,_].

player_value_jokercheck([Hand, Bid], [Value, Hand, Bid]) :-
    \+ contain_joker(Hand),
    player_value([Hand, Bid], [Value, Hand, Bid]).
player_value_jokercheck([Hand, Bid], [Value, Hand, Bid]) :-
    contain_joker(Hand),
    jokerhand_besthand(Hand, [Value, _]).

player_bid([_,_,Bid], Bid).

calc_winnings(Bids, W) :- f_calc_winnings(1, Bids, W).
f_calc_winnings(_, [], 0).
f_calc_winnings(I, [B|Bs], TotalW) :-
    I1 is I + 1,
    W is I * B,
    f_calc_winnings(I1, Bs, TW1),
    TotalW is W + TW1.

printlist([]).
printlist([X|Xs]) :- write(X), nl, printlist(Xs).

main :-
    % read_file_lines('./inpex.txt', Lines),
    read_file_lines('./input.txt', Lines),
    map(parse_line, Lines, Game),
    map(player_value_jokercheck, Game, Players),
    predsort(player_ranking, Players, PlayersSorted),
    map(player_bid, PlayersSorted, Bids),
    calc_winnings(Bids, W),
    % write(Players), nl,
    % write(Game), nl,
    % write(PlayersSorted), nl,
    % printlist(PlayersSorted),
    write(Bids), nl,
    write(W),
    nl.
