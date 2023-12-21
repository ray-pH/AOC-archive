:- initialization main, halt.
:- consult("../common.pro").

char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowStr),
    nth0(Col, RowStr, Char).

get_stones(Map, StoneList) :-
    length(Map, MapLen),
    findall((Row,Col), (
        between(0, MapLen, Row),
        between(0, MapLen, Col),
        char_at((Row,Col), Map, '#')
    ), StoneList).

even(X) :- 0 is X mod 2.
evenpos(Pos) :-
    Pos = (Row,Col),
    Sum is Row + Col,
    even(Sum).

topleft(Pos) :-
    Pos = (Row,Col),
    % y < -x + 66 + 1
    Max is -Col + 66 + 1,
    Row < Max.
topright(Pos) :-
    Pos = (Row,Col),
    % y < x - 66 + 1
    Max is Col - 66 + 1,
    Row < Max.
botleft(Pos) :-
    Pos = (Row,Col),
    % y > x + 66 - 1
    Min is Col + 66 - 1,
    Row > Min.
botright(Pos) :-
    Pos = (Row,Col),
    % y > -x + 66 + 131
    Min is -Col + 66 + 131,
    Row > Min.
center(Pos) :-
    \+ topleft(Pos),
    \+ topright(Pos),
    \+ botleft(Pos),
    \+ botright(Pos).

main :-
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    get_stones(Map, StoneList),
    % write(StoneList), nl,
    length(StoneList, StoneCount),
    write(StoneCount), nl,
    %
    include(topleft, StoneList, TopLeft), length(TopLeft, TopLeftCount), 
    write("Top Left: "), write(TopLeftCount), nl,
    include(topright, StoneList, TopRight), length(TopRight, TopRightCount), 
    write("Top Right: "), write(TopRightCount), nl,
    include(botleft, StoneList, BotLeft), length(BotLeft, BotLeftCount), 
    write("Bot Left: "), write(BotLeftCount), nl,
    include(botright, StoneList, BotRight), length(BotRight, BotRightCount), 
    write("Bot Right: "), write(BotRightCount), nl,
    sumlist([TopLeftCount, TopRightCount, BotLeftCount, BotRightCount], CornerCount),
    write("Corner: "), write(CornerCount), nl,
    include(center, StoneList, Center),
    length(Center, CenterCount), write("Center: "), write(CenterCount), nl,
    %
    write("Even-Odd"), nl,
    include(evenpos, TopLeft, TopLeftEven), length(TopLeftEven, TopLeftEvenCount), 
    TopLeftOddCount is TopLeftCount - TopLeftEvenCount,
    write("Top Left: "), write(TopLeftEvenCount-TopLeftOddCount), nl,
    include(evenpos, TopRight, TopRightEven), length(TopRightEven, TopRightEvenCount),
    TopRightOddCount is TopRightCount - TopRightEvenCount,
    write("Top Right: "), write(TopRightEvenCount-TopRightOddCount), nl,
    include(evenpos, BotLeft, BotLeftEven), length(BotLeftEven, BotLeftEvenCount),
    BotLeftOddCount is BotLeftCount - BotLeftEvenCount,
    write("Bot Left: "), write(BotLeftEvenCount-BotLeftOddCount), nl,
    include(evenpos, BotRight, BotRightEven), length(BotRightEven, BotRightEvenCount),
    BotRightOddCount is BotRightCount - BotRightEvenCount,
    write("Bot Right: "), write(BotRightEvenCount-BotRightOddCount), nl,
    sumlist([TopLeftEvenCount, TopRightEvenCount, BotLeftEvenCount, BotRightEvenCount], EvenCornerCount),
    sumlist([TopLeftOddCount, TopRightOddCount, BotLeftOddCount, BotRightOddCount], OddCornerCount),
    write("Corner: "), write(EvenCornerCount-OddCornerCount), nl,
    include(evenpos, Center, CenterEven), length(CenterEven, CenterEvenCount),
    CenterOddCount is CenterCount - CenterEvenCount,
    write("Center: "), write(CenterEvenCount-CenterOddCount),
    nl.
