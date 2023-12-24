:- initialization main, halt.
:- consult("../common.pro").
:- dynamic visited/1.
:- dynamic calc_pathlength_memo/2.

dir_vector('^', (-1, 0)).
dir_vector('v', (1, 0)).
dir_vector('<', (0, -1)).
dir_vector('>', (0, 1)).
reverse_dir('^', 'v').
reverse_dir('v', '^').
reverse_dir('<', '>').
reverse_dir('>', '<').
add_vector((X1, Y1), (X2, Y2), (X3, Y3)) :- X3 is X1 + X2, Y3 is Y1 + Y2.
char_at((Row,Col), Map, Char) :-
    nth0(Row, Map, RowStr),
    nth0(Col, RowStr, Char).

pos_neighbours(Map, Pos, Neighbours) :-
    char_at(Pos, Map, C),
    C \= '#',
    findall(NPos, (
        dir_vector(_, Dir),
        add_vector(Pos, Dir, NPos),
        char_at(NPos, Map, NC),
        NC \= '#'
    ), Neighbours).

put_assocl_network(Pos-Neighbours, Assoc0, Assoc) :- put_assoc(Pos, Assoc0, Neighbours, Assoc).
networks_graph(Networks, Graph) :-
    % N = Pos-Neighbours,
    foldl(put_assocl_network, Networks, t, Graph).
generate_graph(Map, Graph) :-
    length(Map, MapSize), MM is MapSize - 1,
    findall(Pos-Neighbours, (
        between(0, MM, Row), between(0, MM, Col),
        Pos = (Row,Col), 
        pos_neighbours(Map, Pos, Neighbours)
    ), Networks),
    networks_graph(Networks, Graph).

find_node(Map, Nodes) :-
    length(Map, MapSize), MM is MapSize - 1,
    findall(Pos, (
        between(0, MM, Row), between(0, MM, Col),
        Pos = (Row,Col), 
        \+ char_at(Pos, Map, '#'),
        pos_neighbours(Map, Pos, Neighbours),
        \+ length(Neighbours, 2)
    ), Nodes).

find_ends(Map, Points) :-
    length(Map, MapSize), MM is MapSize - 1,
    findall(Pos, (
        between(0, MM, Row), between(0, MM, Col),
        Pos = (Row,Col), 
        \+ char_at(Pos, Map, '#'),
        pos_neighbours(Map, Pos, Neighbours),
        length(Neighbours, 1)
    ), Points).

traverse_edge(_, Nodes, _, Dist, Curr, Target) :-
    member(Curr, Nodes),
    Target = Curr-Dist.
traverse_edge(Prev, Nodes, Graph, Dist, Curr, Target) :-
    % write('traversing:  '), write(Curr), write(' '), write(Dist), nl,
    get_assoc(Curr, Graph, Neighbours),
    member(B, Neighbours), B \= Prev,
    NDist is Dist + 1, !,
    traverse_edge(Curr, Nodes, Graph, NDist, B, Target).

edges_from_node(Nodes, Graph, Node, GroupedEdges) :-
    get_assoc(Node, Graph, Neighbours),
    % Targets = Listof Node-Dist.
    maplist(traverse_edge(Node,Nodes,Graph,1), Neighbours, Targets),
    maplist([X,Y]>>(X=T-Dist, Y = Node-T-Dist), Targets, Edges0),
    remove_sameedge_shorter(Edges0, GroupedEdges).

f_remove_sameedge_shorter([], []).
f_remove_sameedge_shorter([X], [X]).
f_remove_sameedge_shorter([A,B|Xs], NextEdges) :-
    % remove A
    A = M-N-DistA, B = M-N-DistB, DistA < DistB,
    f_remove_sameedge_shorter([B|Xs], NextEdges).
f_remove_sameedge_shorter([A,B|Xs], [A|NextEdges]) :-
    f_remove_sameedge_shorter([B|Xs], NextEdges).

remove_sameedge_shorter(Edges, NewEdges) :-
    sort(Edges, SortedEdges),
    f_remove_sameedge_shorter(SortedEdges, NewEdges).


egdes_graph(EdgeList, Graph0, Graph) :-
    nth0(0, EdgeList, A-_-_),
    put_assoc(A, Graph0, EdgeList, Graph).
gedges_graph(GroupedEdges, Graph) :-
    foldl(egdes_graph, GroupedEdges, t, Graph).

done(Target, Path) :- reverse(Path, [_-Target-_|_]).
edge_hasbeen_traversed(Path, Edge) :-
    Edge = _-B-_,
    maplist([X,Y]>>(X=Y-_-_), Path, PosList),
    member(B, PosList).

prepend(X, Y, L) :- append(X, [Y], L).
travere_step(Graph, Path, NextPaths) :-
    % write("  traverse_step"), nl,
    % write("  "), write(Path), nl,
    % reverse(Path, X),
    % write("  "), write(X), nl,
    reverse(Path, [_-Curr-_|_]),
    % write("  "), write(Curr), nl,
    get_assoc(Curr, Graph, EdgeList),
    exclude(edge_hasbeen_traversed(Path), EdgeList, NextEdges),
    % write("  "), write(NextEdges), nl,
    maplist(prepend(Path), NextEdges, NextPaths).

traverse_all([], _, _,  Done, Done).
traverse_all(Queue, Graph, Target, DoneAcc, Done) :-
    % write("traverse_all"), nl,
    % write(Queue), nl,
    length(Queue, QLen), write(QLen), nl,
    maplist(travere_step(Graph), Queue, NextQueue0),
    flatten1(NextQueue0, NextQueue1),
    include(done(Target), NextQueue1, ThisDone),!,
    exclude(done(Target), NextQueue1, NextQueue),!,
    append(DoneAcc, ThisDone, NextDoneAcc),
    traverse_all(NextQueue, Graph, Target, NextDoneAcc, Done).

box(X, [X]).
traverse_all(Start, Graph, Target, Paths) :-
    get_assoc(Start, Graph, EdgeList),
    maplist(box, EdgeList, Queue),
    traverse_all(Queue, Graph, Target, [], Paths).

target(Map, Target) :-
    length(Map, MapSize), Row is MapSize - 1, Col is MapSize - 2,
    Target = (Row,Col).

path_dist(Path, Dist) :-
    maplist([X,Y]>>(X=_-_-Y), Path, Ds),
    sumlist(Ds, Dist).

main :-
    % read_file_lines('./inpex.txt', Lines),
    % read_file_lines('./inpex0.txt', Lines),
    read_file_lines('./input.txt', Lines),
    maplist(string_chars, Lines, Map),
    Start = (0,1),
    target(Map, Target),
    generate_graph(Map, Graph),
    find_node(Map, Nodes),
    find_ends(Map, Ends),
    % find_points(Map, Points),
    write(Target), nl,
    write(Nodes), nl,
    write(Ends), nl,
    maplist(edges_from_node(Nodes, Graph), Nodes, GroupedEdges),
    % write(GroupedEdges), nl,
    gedges_graph(GroupedEdges, SimpleGraph),!,
    % write(SimpleGraph), nl,
    assoc_to_list(SimpleGraph, SimpleGraphList),
    write(SimpleGraphList), nl,
    %
    % traverse_all(Start, SimpleGraph, Target, Paths),
    % write(Paths), nl,
    %
    % maplist(path_dist, Paths, Dists),
    % write(Dists), nl,
    nl.
